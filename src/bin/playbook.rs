//! Uploads a playbook to the profile bucket, where the trader reads it at startup, and journals the upload under
//! `producer=operator`: `playbook upload <path>`.

use std::num::NonZeroU64;
use std::path::PathBuf;
use std::process::ExitCode;

use chrono::{DateTime, Utc};
use tracing_subscriber::EnvFilter;
use uuid::Uuid;

use fund::common::journal::{Observation, Record, RunId};
use fund::common::playbook::{Playbook, PlaybookUploaded};
use fund::common::storage::{ConfigurationKey, EntityTag, JournalKey, Key, ObjectKey};
use fund::common::time::SessionDate;
use fund::journal::built_commit;
use fund::records::{RefusedToStart, merge_into};
use fund::storage::Bucket;

const USAGE: &str = "playbook upload <path>";

/// What uploading a playbook does to the one held now.
#[derive(Debug, PartialEq, Eq)]
enum Upload<'a> {
    /// Nothing is held, so the write must still find the key empty.
    Create,
    /// The held playbook is byte for byte the one given.
    Unchanged,
    /// The write must still find the version this tag names, so a concurrent upload is refused rather than lost.
    Replace(&'a EntityTag),
}

fn upload<'a>(held: Option<&'a (Vec<u8>, EntityTag)>, contents: &[u8]) -> Upload<'a> {
    match held {
        None => Upload::Create,
        Some((bytes, _)) if bytes == contents => Upload::Unchanged,
        Some((_, tag)) => Upload::Replace(tag),
    }
}

/// The upload's record, the first and only of its own run, and the operator journal of the session it happened in.
fn uploaded(
    contents: String,
    replaced: Option<EntityTag>,
    run: RunId,
    at: DateTime<Utc>,
) -> (JournalKey, Record) {
    let record = Record::new(
        run,
        NonZeroU64::MIN,
        at,
        built_commit(),
        Observation::PlaybookUploaded(PlaybookUploaded::new(contents, replaced)),
    );
    (JournalKey::operator(SessionDate::at(at)), record)
}

#[tokio::main]
async fn main() -> ExitCode {
    // An operator's command that ships nothing, so it logs to standard output alone rather than to a file.
    tracing_subscriber::fmt()
        .json()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();
    let arguments: Vec<String> = std::env::args().skip(1).collect();
    let path = match arguments.as_slice() {
        [command, path] if command == "upload" => PathBuf::from(path),
        _ => {
            tracing::error!(?arguments, usage = USAGE, "Usage refused");
            return RefusedToStart.into();
        }
    };
    let contents = match std::fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(error) => {
            tracing::error!(path = %path.display(), %error, "Playbook not read");
            return RefusedToStart.into();
        }
    };
    // The trader refuses a playbook it cannot parse, so one is never uploaded.
    if let Err(refusal) = Playbook::parse(&contents) {
        tracing::error!(path = %path.display(), %refusal, "Playbook refused");
        return RefusedToStart.into();
    }
    let profile = match Bucket::profile(&aws_config::load_from_env().await) {
        Ok(profile) => profile,
        Err(refusal) => {
            tracing::error!(%refusal, "Records configuration refused");
            return RefusedToStart.into();
        }
    };
    let key = ConfigurationKey::Playbook;
    let held = match profile.get_tagged(&key).await {
        Ok(held) => held,
        Err(error) => {
            tracing::error!(%error, "Uploaded playbook not read");
            return ExitCode::FAILURE;
        }
    };
    let body = contents.clone().into_bytes();
    let written = match upload(held.as_ref(), &body) {
        Upload::Unchanged => {
            tracing::info!(path = key.path(), "Playbook unchanged");
            return ExitCode::SUCCESS;
        }
        Upload::Create => profile.create(&key, body).await,
        Upload::Replace(tag) => profile.replace(&key, body, tag).await,
    };
    match written {
        Ok(()) => {}
        Err(error) => {
            tracing::error!(%error, "Playbook not uploaded");
            return ExitCode::FAILURE;
        }
    }
    let replaced = held.map(|(_, tag)| tag);
    tracing::info!(
        path = key.path(),
        replaced = replaced.is_some(),
        "Playbook uploaded"
    );
    let (journal, record) = uploaded(contents, replaced, RunId::new(Uuid::new_v4()), Utc::now());
    let journal_path = Key::from(journal).path();
    match merge_into(&profile, journal, &record.encode()).await {
        Ok(()) => {
            tracing::info!(path = journal_path, "Upload journaled");
            ExitCode::SUCCESS
        }
        // The playbook is already uploaded, so this fails only the record of it.
        Err(failure) => {
            tracing::error!(path = journal_path, %failure, "Upload not journaled");
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_an_upload_creates_keeps_or_replaces_what_is_held() {
        let tag = EntityTag::new("\"abc\"");
        let held = (b"old".to_vec(), tag.clone());
        assert_eq!(upload(None, b"new"), Upload::Create);
        assert_eq!(upload(Some(&held), b"old"), Upload::Unchanged);
        assert_eq!(upload(Some(&held), b"new"), Upload::Replace(&tag));
    }

    /// An upload is journaled under the operator's session it happened in, as its own run's first record, with the
    /// playbook whole and the version it replaced.
    #[test]
    fn test_an_upload_is_journaled_to_the_operator_session_it_happened_in() {
        let run = RunId::new(Uuid::from_u128(3));
        // 23:30 Eastern on 2026-10-14, so the session is the Eastern date, not the UTC one.
        let at = "2026-10-15T03:30:00Z".parse().unwrap();
        let tag = EntityTag::new("\"abc\"");
        let (journal, record) = uploaded(
            "roll_off_minutes = 5\n".to_string(),
            Some(tag.clone()),
            run,
            at,
        );
        assert_eq!(
            Key::from(journal).path(),
            "records/journal/producer=operator/year=2026/month=10/day=14/data.parquet"
        );
        assert_eq!(
            (record.run_id(), record.sequence(), record.timestamp()),
            (run, 1, at)
        );
        assert_eq!(
            record.observation(),
            &Observation::PlaybookUploaded(PlaybookUploaded::new(
                "roll_off_minutes = 5\n".to_string(),
                Some(tag)
            ))
        );
    }
}
