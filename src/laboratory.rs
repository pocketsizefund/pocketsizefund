//! Loads what studies read and catalogs what they do: a `Study` journals every dataset its loaders read and every
//! experiment it reports, then merges the journal into the profile bucket.

pub mod compare;
pub mod dataset;
pub mod replay;
pub mod source;

use std::io;
use std::num::NonZeroU32;
use std::path::PathBuf;

use chrono::Utc;
use tokio::time::Instant;
use uuid::Uuid;

use crate::common::journal::{Observation, RunId};
use crate::common::laboratory::dataset::Fingerprint;
use crate::common::laboratory::experiment::{
    DatasetRead, Elapsed, ExperimentRan, Label, Machine, Outputs, Parameters,
};
use crate::common::storage::{Host, JournalKey, Key, ObjectKey, SourceKey};
use crate::common::time::SessionDate;
use crate::journal::{Journal, file_name};
use crate::laboratory::dataset::Lineage;
use crate::laboratory::source::Source;
use crate::records::{RESHIPPED_DAYS, ShipFailure, contents, merge_into};
use crate::storage::{Bucket, BucketError};

/// One run of study code: its journal, what it is about, the code it runs, the machine it runs on, and when it opened.
pub struct Study {
    journal: Journal,
    label: Label,
    source: Source,
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
    /// Opens a run journaling into `directory`, named for this machine's hostname; a study binary opens through
    /// `study!`, which captures `source` from the files compiled into it.
    pub fn open(label: Label, directory: impl Into<PathBuf>, source: Source) -> io::Result<Self> {
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
            source,
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
            self.source.digest(),
        );
        self.journal
            .append(Utc::now(), Observation::ExperimentRan(Box::new(ran)))
            .map_err(StudyError::Journal)
    }

    /// Stores the source under its digest, then merges each recent session file in the journal directory into its
    /// researcher object in `profile`, so studies from other directories or machines that shipped the same session keep
    /// their records; each outcome is named by the path it wrote.
    pub async fn finish(self, profile: &Bucket) -> Vec<(String, Result<(), ShipFailure>)> {
        let source = SourceKey::new(self.source.digest());
        // The key is the content's digest, so an object already under it holds these bytes.
        let stored = match profile.create(&source, self.source.encode()).await {
            Ok(()) | Err(BucketError::Contended { .. }) => Ok(()),
            Err(error) => Err(ShipFailure::Bucket(error)),
        };
        let mut shipped = vec![(source.path(), stored)];
        let today = SessionDate::at(Utc::now());
        for days in 0..RESHIPPED_DAYS {
            let session = today.plus_calendar_days(-days);
            let key = JournalKey::new(Host::Researcher, session);
            let outcome = match contents(&self.journal.directory().join(file_name(session))) {
                Some(Ok(text)) => merge_into(profile, key, &text).await,
                Some(Err(failure)) => Err(failure),
                None => continue,
            };
            shipped.push((Key::from(key).path(), outcome));
        }
        shipped
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Two studies in different directories ship the same session and source; the object keeps both runs' records and
    /// the second finds its source already stored.
    #[tokio::test]
    #[ignore = "writes researcher journal objects to the bucket AWS_S3_PROFILE_BUCKET_NAME names, which must be a \
                development bucket"]
    async fn live_two_studies_shipping_one_session_keep_both() {
        let bucket = std::env::var("AWS_S3_PROFILE_BUCKET_NAME").unwrap();
        assert!(
            bucket.contains("development"),
            "refusing to write test journals into {bucket}"
        );
        let configuration = aws_config::load_from_env().await;
        let profile = Bucket::profile(&configuration).unwrap();
        let source = Source::new("live merge check.rs", &Uuid::new_v4().to_string());
        let mut runs = Vec::new();
        for _ in 0..2 {
            let directory = std::env::temp_dir().join(format!("fund-study-{}", Uuid::new_v4()));
            let mut study = Study::open(
                Label::new("live merge check").unwrap(),
                &directory,
                source.clone(),
            )
            .unwrap();
            study
                .experiment(Parameters::default(), &[], Outputs::default())
                .unwrap();
            runs.push(study.run_id());
            let shipped = study.finish(&profile).await;
            assert_eq!(shipped[0].0, SourceKey::new(source.digest()).path());
            for (path, outcome) in shipped {
                assert!(outcome.is_ok(), "{path}: {outcome:?}");
            }
            std::fs::remove_dir_all(&directory).unwrap();
        }
        assert_eq!(
            profile.get(&SourceKey::new(source.digest())).await.unwrap(),
            Some(source.encode())
        );
        let key = JournalKey::new(Host::Researcher, SessionDate::at(Utc::now()));
        let held = crate::storage::journal::decode(
            &key,
            profile.get(&Key::from(key)).await.unwrap().unwrap(),
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
