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
    DatasetRead, Elapsed, ExperimentRan, Label, Machine, Outputs, Parameters, SourceDigest,
};
use crate::common::storage::{Host, JournalKey, Key, ObjectKey, SourceKey};
use crate::common::time::SessionDate;
use crate::journal::{Journal, file_name};
use crate::laboratory::dataset::Lineage;
use crate::laboratory::source::{Source, digest_of};
use crate::records::{RESHIPPED_DAYS, ShipFailure, contents, merge_into};
use crate::storage::{Bucket, BucketError};

/// One run of study code: its journal, what it is about, the code it runs, the machine it runs on, and when it opened.
pub struct Study {
    journal: Journal,
    label: Label,
    source_digest: SourceDigest,
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
    /// Opens a run journaling into `directory`, named for this machine's hostname, and writes `source` into its `sources`
    /// subdirectory for `finish` to ship; a study binary opens through `study!`, which captures `source`.
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
        let journal = Journal::open(directory, RunId::new(Uuid::new_v4()))?;
        let sources = journal.directory().join(SOURCES);
        std::fs::create_dir_all(&sources)?;
        let source_digest = source.digest();
        // Written aside and renamed, so a concurrent `finish` never reads a partial source.
        let partial = sources.join(format!(".{}.{}", source_digest.as_str(), journal.run_id()));
        std::fs::write(&partial, source.encode())?;
        std::fs::rename(
            &partial,
            sources.join(format!("{}.{SOURCE_EXTENSION}", source_digest.as_str())),
        )?;
        Ok(Self {
            journal,
            label,
            source_digest,
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
            self.source_digest.clone(),
        );
        self.journal
            .append(Utc::now(), Observation::ExperimentRan(Box::new(ran)))
            .map_err(StudyError::Journal)
    }

    /// Stores each source kept in the journal directory under its digest, then merges each recent session file there
    /// into its researcher object in `profile`, so studies from other directories or machines that shipped the same
    /// session keep their records; each outcome is named by the path it wrote.
    pub async fn finish(self, profile: &Bucket) -> Vec<(String, Result<(), ShipFailure>)> {
        let mut shipped = Vec::new();
        let sources = self.journal.directory().join(SOURCES);
        match std::fs::read_dir(&sources) {
            Ok(entries) => {
                for entry in entries {
                    let path = match entry {
                        Ok(entry)
                            if entry
                                .path()
                                .extension()
                                .is_some_and(|extension| extension == SOURCE_EXTENSION) =>
                        {
                            entry.path()
                        }
                        Ok(..) => continue,
                        Err(error) => {
                            let failure = ShipFailure::Read {
                                path: sources.clone(),
                                error,
                            };
                            shipped.push((sources.display().to_string(), Err(failure)));
                            continue;
                        }
                    };
                    match std::fs::read(&path) {
                        Ok(encoded) => {
                            let key = SourceKey::new(digest_of(&encoded));
                            let outcome = store_source(profile, &key, encoded).await;
                            if outcome.is_ok()
                                && let Err(error) = std::fs::remove_file(&path)
                            {
                                tracing::warn!(path = %path.display(), %error, "Shipped source not removed");
                            }
                            shipped.push((key.path(), outcome));
                        }
                        Err(error) => {
                            shipped.push((
                                path.display().to_string(),
                                Err(ShipFailure::Read { path, error }),
                            ));
                        }
                    }
                }
            }
            Err(error) => shipped.push((
                sources.display().to_string(),
                Err(ShipFailure::Read {
                    path: sources,
                    error,
                }),
            )),
        }
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

/// The subdirectory of a journal directory holding each source a study there opened with and no `finish` has stored
/// yet; one that failed to store stays for the next `finish`.
const SOURCES: &str = "sources";

const SOURCE_EXTENSION: &str = "json";

/// Creates `encoded` under `key`; a taken key counts as stored only when it reads back as these bytes, since a create
/// can also be contended by a concurrent delete.
async fn store_source(
    profile: &Bucket,
    key: &SourceKey,
    encoded: Vec<u8>,
) -> Result<(), ShipFailure> {
    match profile.create(key, encoded.clone()).await {
        Ok(()) => Ok(()),
        Err(BucketError::Contended { path }) => match profile.get(key).await {
            Ok(Some(held)) if held == encoded => Ok(()),
            Ok(Some(..) | None) => Err(ShipFailure::Unmatched { path }),
            Err(error) => Err(ShipFailure::Bucket(error)),
        },
        Err(error) => Err(ShipFailure::Bucket(error)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a_study_keeps_its_source_beside_its_journal_until_it_ships() {
        let directory = std::env::temp_dir().join(format!("fund-study-{}", Uuid::new_v4()));
        let source = Source::new("kept.rs", "fn main() {}");
        Study::open(Label::new("kept").unwrap(), &directory, source.clone()).unwrap();
        let kept: Vec<_> = std::fs::read_dir(directory.join("sources"))
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect();
        assert_eq!(
            kept,
            [directory.join(
                "sources/5c44ba654f4225378bdd19b74298a74f21cd7f31077f66d91f1a7f10e3119c51.json"
            )]
        );
        assert_eq!(std::fs::read(&kept[0]).unwrap(), source.encode());
        std::fs::remove_dir_all(&directory).unwrap();
    }

    /// Two studies in different directories ship the same session and source; the object keeps both runs' records,
    /// the second finds its source already stored, a study abandoned before `finish` has its source shipped by the next
    /// one in its directory, and each stored source leaves the directory.
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
        let abandoned = Source::new("live merge check.rs", &Uuid::new_v4().to_string());
        let mut runs = Vec::new();
        for iteration in 0..2 {
            let directory = std::env::temp_dir().join(format!("fund-study-{}", Uuid::new_v4()));
            if iteration == 0 {
                Study::open(
                    Label::new("live merge check").unwrap(),
                    &directory,
                    abandoned.clone(),
                )
                .unwrap();
            }
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
            assert!(
                shipped
                    .iter()
                    .any(|(path, _)| *path == SourceKey::new(source.digest()).path())
            );
            for (path, outcome) in shipped {
                assert!(outcome.is_ok(), "{path}: {outcome:?}");
            }
            assert_eq!(
                std::fs::read_dir(directory.join("sources"))
                    .unwrap()
                    .count(),
                0
            );
            std::fs::remove_dir_all(&directory).unwrap();
        }
        assert_eq!(
            profile.get(&SourceKey::new(source.digest())).await.unwrap(),
            Some(source.encode())
        );
        assert_eq!(
            profile
                .get(&SourceKey::new(abandoned.digest()))
                .await
                .unwrap(),
            Some(abandoned.encode())
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
