//! Loads what studies read and catalogs what they do: a `Study` journals every dataset its loaders read and every
//! experiment it reports, then merges the journal into the records bucket.

pub mod compare;
pub mod dataset;
pub mod replay;

use std::io;
use std::num::NonZeroU32;
use std::path::PathBuf;

use chrono::Utc;
use tokio::time::Instant;
use uuid::Uuid;

use crate::archive::Archive;
use crate::common::journal::{Observation, RunId};
use crate::common::laboratory::dataset::Fingerprint;
use crate::common::laboratory::experiment::{
    DatasetRead, Elapsed, ExperimentRan, Label, Machine, Outputs, Parameters,
};
use crate::common::storage::{Host, JournalKey, Key};
use crate::common::time::SessionDate;
use crate::journal::{Journal, file_name};
use crate::laboratory::dataset::Lineage;
use crate::records::{RESHIPPED_DAYS, ShipFailure, contents, merge_into};

/// One run of study code: its journal, what it is about, the machine it runs on, and when it opened.
pub struct Study {
    journal: Journal,
    label: Label,
    machine: Machine,
    opened: Instant,
}

#[derive(Debug, thiserror::Error)]
pub enum StudyError {
    #[error("the journal failed: {0}")]
    Journal(io::Error),
    /// The dataset was read by another run, whose journal holds its `dataset_read`.
    #[error("the dataset was read by run {run}, not this one")]
    ReadByAnotherRun { run: RunId },
}

impl Study {
    /// Opens a run journaling into `directory`, named for this machine's hostname.
    pub fn open(label: Label, directory: impl Into<PathBuf>) -> io::Result<Self> {
        let output = std::process::Command::new("hostname").output()?;
        if !output.status.success() {
            return Err(io::Error::other(format!(
                "hostname failed: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }
        let cores = std::thread::available_parallelism()?;
        let machine = Machine::new(
            String::from_utf8_lossy(&output.stdout),
            std::env::consts::ARCH,
            std::env::consts::OS,
            NonZeroU32::try_from(cores).unwrap_or(NonZeroU32::MAX),
        )
        .map_err(|refusal| io::Error::new(io::ErrorKind::InvalidData, refusal.to_string()))?;
        Ok(Self {
            journal: Journal::open(directory, RunId::new(Uuid::new_v4()))?,
            label,
            machine,
            opened: Instant::now(),
        })
    }

    pub fn run_id(&self) -> RunId {
        self.journal.run_id()
    }

    /// Journals a read; only loaders call it, so every dataset a study holds was cataloged.
    pub(crate) fn read(&mut self, fingerprint: &Fingerprint) -> io::Result<()> {
        let read = DatasetRead::new(
            self.label.clone(),
            self.machine.clone(),
            fingerprint.clone(),
        );
        self.journal
            .append(Utc::now(), Observation::DatasetRead(Box::new(read)))
    }

    /// Journals one experiment over the datasets `lineages` came from, which only the loader builds, so the record
    /// names exactly what was read; refused for a dataset another run read.
    pub fn experiment(
        &mut self,
        parameters: Parameters,
        lineages: &[&Lineage],
        outputs: Outputs,
    ) -> Result<(), StudyError> {
        if let Some(lineage) = lineages
            .iter()
            .find(|lineage| lineage.run() != self.run_id())
        {
            return Err(StudyError::ReadByAnotherRun { run: lineage.run() });
        }
        let ran = ExperimentRan::new(
            self.label.clone(),
            self.machine.clone(),
            parameters,
            lineages
                .iter()
                .map(|lineage| lineage.fingerprint().clone())
                .collect(),
            outputs,
            Elapsed::from_milliseconds(
                u64::try_from(self.opened.elapsed().as_millis()).unwrap_or(u64::MAX),
            ),
        );
        self.journal
            .append(Utc::now(), Observation::ExperimentRan(Box::new(ran)))
            .map_err(StudyError::Journal)
    }

    /// Merges each recent session file in the journal directory into its researcher object in `records`, so studies
    /// from other directories or machines that shipped the same session keep their records.
    pub async fn finish(self, records: &Archive) -> Vec<(Key, Result<(), ShipFailure>)> {
        let today = SessionDate::at(Utc::now());
        let mut shipped = Vec::new();
        for days in 0..RESHIPPED_DAYS {
            let session = today.plus_calendar_days(-days);
            let key = JournalKey::new(Host::Researcher, session);
            let outcome = match contents(&self.journal.directory().join(file_name(session))) {
                Some(Ok(text)) => merge_into(records, key, &text).await,
                Some(Err(failure)) => Err(failure),
                None => continue,
            };
            shipped.push((key.into(), outcome));
        }
        shipped
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Two studies in different directories ship the same session; the object keeps both runs' records.
    #[tokio::test]
    #[ignore = "writes researcher journal objects to the bucket AWS_S3_RECORDS_BUCKET_NAME names, which must be a \
                development bucket"]
    async fn live_two_studies_shipping_one_session_keep_both() {
        let bucket = std::env::var("AWS_S3_RECORDS_BUCKET_NAME").unwrap();
        assert!(
            bucket.contains("development"),
            "refusing to write test journals into {bucket}"
        );
        let configuration = aws_config::load_from_env().await;
        let records = Archive::records(&configuration).unwrap();
        let mut runs = Vec::new();
        for _ in 0..2 {
            let directory = std::env::temp_dir().join(format!("fund-study-{}", Uuid::new_v4()));
            let mut study =
                Study::open(Label::new("live merge check").unwrap(), &directory).unwrap();
            study
                .experiment(Parameters::default(), &[], Outputs::default())
                .unwrap();
            runs.push(study.run_id());
            for (key, outcome) in study.finish(&records).await {
                assert!(outcome.is_ok(), "{}: {outcome:?}", key.path());
            }
            std::fs::remove_dir_all(&directory).unwrap();
        }
        let key = JournalKey::new(Host::Researcher, SessionDate::at(Utc::now()));
        let held = crate::archive::journal::decode(
            &key,
            records.get(&Key::from(key)).await.unwrap().unwrap(),
        )
        .unwrap();
        for run in runs {
            assert!(
                held.iter().any(|line| matches!(
                    line,
                    crate::common::journal::ReadLine::Read(record) if record.run_id() == run
                )),
                "{run} is missing"
            );
        }
    }
}
