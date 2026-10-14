//! Vendor bytes stored as served: flat files copied byte for byte in parts fetched and uploaded in parallel, and fetched
//! response pages written a line each into one gzip stream, so no file is ever held whole, under a full-object CRC64
//! that S3 computes and any later copy of the same bytes must match.

use std::collections::HashMap;
use std::io::Write;
use std::num::NonZeroU64;
use std::sync::Arc;
use std::time::Duration;

use aws_sdk_s3::primitives::ByteStream;
use aws_sdk_s3::types::{
    ChecksumAlgorithm, ChecksumMode, ChecksumType, CompletedMultipartUpload, CompletedPart,
};
use chrono::{DateTime, Utc};
use tokio::sync::Semaphore;
use tokio::task::JoinSet;

use super::bars::{MetadataEntry, Provenance};
use super::{Archive, ArchiveError};
use crate::common::storage::{EntityTag, Key, StorageClass};
use crate::ingest::flat_files::{FlatFileDataset, FlatFileError, FlatFiles, Listed};

/// The largest quote file, about 19 GB, is under three hundred parts of this size.
const PART_LENGTH: u64 = 64 * 1024 * 1024;

/// Attempts at one part, each fetching and uploading it afresh, before the file is abandoned.
const PART_ATTEMPTS: u32 = 5;

/// What the archive holds under a key: its length and the checksum S3 holds for it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stored {
    length: u64,
    checksum: Checksum,
    /// When the vendor's bytes were fetched, from the object's metadata; `None` on a copy that never recorded it.
    fetched_at: Option<DateTime<Utc>>,
    /// The version read, which a conditional delete must still match.
    tag: EntityTag,
}

impl Stored {
    pub fn tag(&self) -> &EntityTag {
        &self.tag
    }

    pub fn fetched_at(&self) -> Option<DateTime<Utc>> {
        self.fetched_at
    }

    pub fn length(&self) -> u64 {
        self.length
    }

    pub fn checksum(&self) -> &Checksum {
        &self.checksum
    }
}

/// Base64 of the CRC64/NVME over a whole object, the same whatever part size wrote it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Crc64(String);

impl Crc64 {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// The checksum S3 holds for an object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Checksum {
    FullObject(Crc64),
    /// A checksum S3 did not name as over the whole object, such as one over the parts, so no other copy can match it.
    Composite,
    Absent,
}

impl Checksum {
    fn of(checksum_type: Option<&ChecksumType>, crc64: Option<&str>) -> Self {
        match (checksum_type, crc64) {
            (Some(ChecksumType::FullObject), Some(crc64)) => {
                Self::FullObject(Crc64(crc64.to_string()))
            }
            (Some(_) | None, Some(_)) => Self::Composite,
            (Some(_) | None, None) => Self::Absent,
        }
    }
}

/// Why a vendor file was not copied.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum CopyError {
    #[error("{0}")]
    Source(FlatFileError),
    #[error("{0}")]
    Archive(ArchiveError),
    /// The vendor lists the file with no bytes, which a multipart upload cannot hold.
    #[error("{path} is listed with no bytes")]
    Empty { path: String },
    /// Stored at a different length than the vendor listed.
    #[error("{path} stored {stored:?} bytes where {listed} were listed")]
    Length {
        path: String,
        listed: u64,
        stored: Option<u64>,
    },
}

impl Archive {
    /// Copies `dataset`'s file for `listed` under its key, only if nothing is there yet; `permits` bounds the parts in
    /// flight across every copy sharing it.
    pub async fn copy_flat_file(
        &self,
        flat_files: &FlatFiles,
        dataset: FlatFileDataset,
        listed: &Listed,
        provenance: &Provenance,
        permits: Arc<Semaphore>,
    ) -> Result<Stored, CopyError> {
        let key = dataset.key(listed.session());
        let path = key.path();
        if listed.length() == 0 {
            return Err(CopyError::Empty { path });
        }
        let upload_id = self
            .create_upload(&key, provenance)
            .await
            .map_err(CopyError::Archive)?;
        let uploaded = self
            .upload_parts(flat_files, dataset, listed, &upload_id, permits)
            .await;
        let completed = match uploaded {
            Ok(parts) => self.complete(&path, &upload_id, parts).await,
            Err(error) => Err(error),
        };
        if let Err(error) = completed {
            self.abort_upload(&path, &upload_id).await;
            return Err(error);
        }
        match self.stored(&key).await.map_err(CopyError::Archive)? {
            Some(stored) if stored.length == listed.length() => Ok(stored),
            stored => Err(CopyError::Length {
                path,
                listed: listed.length(),
                stored: stored.map(|stored| stored.length),
            }),
        }
    }

    /// Opens a multipart upload under `key` stamped with `provenance`, returning its id; finish or abort it, or its
    /// parts keep billing.
    async fn create_upload(
        &self,
        key: &Key,
        provenance: &Provenance,
    ) -> Result<String, ArchiveError> {
        let path = key.path();
        let failed = |reason: String| ArchiveError::Put {
            path: path.clone(),
            reason,
        };
        let created = self
            .s3_client
            .create_multipart_upload()
            .bucket(&self.bucket_name)
            .key(&path)
            .storage_class(storage_class(key))
            .checksum_algorithm(ChecksumAlgorithm::Crc64Nvme)
            .checksum_type(ChecksumType::FullObject)
            .set_metadata(Some(metadata(provenance)))
            .send()
            .await
            .map_err(|error| failed(aws_sdk_s3::error::DisplayErrorContext(error).to_string()))?;
        Ok(created
            .upload_id()
            .ok_or_else(|| failed("no upload id".to_string()))?
            .to_string())
    }

    /// Discards an upload and its parts; a failed abort is logged with the id to abort by hand.
    async fn abort_upload(&self, path: &str, upload_id: &str) {
        if let Err(abort) = self
            .s3_client
            .abort_multipart_upload()
            .bucket(&self.bucket_name)
            .key(path)
            .upload_id(upload_id)
            .send()
            .await
        {
            tracing::error!(
                path,
                upload_id,
                error = %aws_sdk_s3::error::DisplayErrorContext(abort),
                "Multipart upload not aborted"
            );
        }
    }

    async fn upload_parts(
        &self,
        flat_files: &FlatFiles,
        dataset: FlatFileDataset,
        listed: &Listed,
        upload_id: &str,
        permits: Arc<Semaphore>,
    ) -> Result<Vec<CompletedPart>, CopyError> {
        let path = dataset.key(listed.session()).path();
        let mut tasks = JoinSet::new();
        for range in ranges(listed.length()) {
            let part = Part {
                s3_client: self.s3_client.clone(),
                bucket_name: self.bucket_name.clone(),
                path: path.clone(),
                upload_id: upload_id.to_string(),
                range,
            };
            let flat_files = flat_files.clone();
            let listed = listed.clone();
            let permits = Arc::clone(&permits);
            tasks.spawn(async move {
                let _permit = permits
                    .acquire_owned()
                    .await
                    .expect("the semaphore is never closed");
                part.upload(&flat_files, dataset, &listed).await
            });
        }
        let mut parts = Vec::new();
        while let Some(joined) = tasks.join_next().await {
            let part = joined.map_err(|error| {
                CopyError::Archive(ArchiveError::Put {
                    path: path.clone(),
                    reason: error.to_string(),
                })
            })??;
            parts.push(part);
        }
        parts.sort_by_key(|part| part.part_number());
        Ok(parts)
    }

    async fn complete(
        &self,
        path: &str,
        upload_id: &str,
        parts: Vec<CompletedPart>,
    ) -> Result<(), CopyError> {
        self.s3_client
            .complete_multipart_upload()
            .bucket(&self.bucket_name)
            .key(path)
            .upload_id(upload_id)
            .checksum_type(ChecksumType::FullObject)
            .multipart_upload(
                CompletedMultipartUpload::builder()
                    .set_parts(Some(parts))
                    .build(),
            )
            .if_none_match("*")
            .send()
            .await
            .map_err(|error| {
                // 412 is a key already written; 409 is a conditional write that raced another.
                CopyError::Archive(
                    match error
                        .raw_response()
                        .map(|response| response.status().as_u16())
                    {
                        Some(409 | 412) => ArchiveError::Contended {
                            path: path.to_string(),
                        },
                        Some(_) | None => ArchiveError::Put {
                            path: path.to_string(),
                            reason: aws_sdk_s3::error::DisplayErrorContext(error).to_string(),
                        },
                    },
                )
            })?;
        Ok(())
    }

    /// What is stored under `key`, read from its metadata alone, so a Deep Archive object needs no restore.
    pub async fn stored(&self, key: &Key) -> Result<Option<Stored>, ArchiveError> {
        let path = key.path();
        let failed = |reason: String| ArchiveError::Get {
            path: path.clone(),
            reason,
        };
        match self
            .s3_client
            .head_object()
            .bucket(&self.bucket_name)
            .key(&path)
            .checksum_mode(ChecksumMode::Enabled)
            .send()
            .await
        {
            Ok(response) => {
                let length = response
                    .content_length()
                    .and_then(|length| u64::try_from(length).ok())
                    .ok_or_else(|| failed("no length".to_string()))?;
                let fetched_at = fetched_at(response.metadata()).map_err(failed)?;
                let tag = EntityTag::new(
                    response
                        .e_tag()
                        .ok_or_else(|| failed("no entity tag".to_string()))?,
                );
                Ok(Some(Stored {
                    tag,
                    length,
                    checksum: Checksum::of(
                        response.checksum_type(),
                        response.checksum_crc64_nvme(),
                    ),
                    fetched_at,
                }))
            }
            Err(error)
                if error
                    .as_service_error()
                    .is_some_and(|error| error.is_not_found()) =>
            {
                Ok(None)
            }
            Err(error) => Err(failed(
                aws_sdk_s3::error::DisplayErrorContext(error).to_string(),
            )),
        }
    }
}

/// Why a session's fetched pages were not stored.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum TeeError {
    #[error("{0}")]
    Archive(ArchiveError),
    /// A page holding a line break would split into two lines that are neither the page.
    #[error("{path} was sent a page holding a line break")]
    LineBreak { path: String },
    /// No page was sent, which a multipart upload cannot hold.
    #[error("{path} was sent no pages")]
    Empty { path: String },
    /// Stored at a different length than was written.
    #[error("{path} stored {stored:?} bytes where {written} were written")]
    Length {
        path: String,
        written: u64,
        stored: Option<u64>,
    },
}

/// Pages gzipped into one stream a line each, cut into parts of at least `part_length` bytes as it grows.
struct GzipLines {
    encoder: flate2::write::GzEncoder<Vec<u8>>,
    part_length: usize,
    lines: u64,
}

impl GzipLines {
    fn new(part_length: usize) -> Self {
        Self {
            encoder: flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default()),
            part_length,
            lines: 0,
        }
    }

    /// Appends `line` and its line break, returning a part once the compressed bytes reach `part_length`.
    fn push(&mut self, line: &[u8]) -> Result<Option<Vec<u8>>, LineBreak> {
        if line.contains(&b'\n') {
            return Err(LineBreak);
        }
        self.encoder
            .write_all(line)
            .and_then(|()| self.encoder.write_all(b"\n"))
            .expect("writing to a vector cannot fail");
        self.lines += 1;
        Ok((self.encoder.get_ref().len() >= self.part_length)
            .then(|| std::mem::take(self.encoder.get_mut())))
    }

    /// The last part, holding the stream's trailer, or `None` when no line was pushed.
    fn finish(self) -> Option<Vec<u8>> {
        let lines = self.lines;
        let rest = self
            .encoder
            .finish()
            .expect("writing to a vector cannot fail");
        (lines > 0).then_some(rest)
    }
}

/// A page refused for holding a line break.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LineBreak;

/// A session's fetched pages being stored under one raw key, a line each, replacing whatever the key held.
pub struct RawWriter {
    archive: Archive,
    key: Key,
    upload_id: String,
    lines: GzipLines,
    parts: Vec<CompletedPart>,
    written: u64,
}

impl Archive {
    /// Opens an upload under `key` stamped with `provenance`; finish or abort it, or its parts keep billing.
    pub async fn begin_raw(
        &self,
        key: Key,
        provenance: &Provenance,
    ) -> Result<RawWriter, TeeError> {
        let upload_id = self
            .create_upload(&key, provenance)
            .await
            .map_err(TeeError::Archive)?;
        Ok(RawWriter {
            archive: self.clone(),
            key,
            upload_id,
            lines: GzipLines::new(PART_LENGTH as usize),
            parts: Vec::new(),
            written: 0,
        })
    }
}

impl RawWriter {
    /// Appends one page as a line, uploading a part whenever enough has accumulated.
    pub async fn push_line(&mut self, page: &[u8]) -> Result<(), TeeError> {
        match self.lines.push(page) {
            Ok(Some(part)) => self.upload(part).await,
            Ok(None) => Ok(()),
            Err(LineBreak) => Err(TeeError::LineBreak {
                path: self.key.path(),
            }),
        }
    }

    /// Uploads the last part and completes the object, aborting the upload on any failure.
    pub async fn finish(mut self) -> Result<Stored, TeeError> {
        let path = self.key.path();
        let last = std::mem::replace(&mut self.lines, GzipLines::new(0)).finish();
        let completed = match last {
            Some(part) => match self.upload(part).await {
                Ok(()) => self.complete().await,
                Err(error) => Err(error),
            },
            None => Err(TeeError::Empty { path: path.clone() }),
        };
        if let Err(error) = completed {
            self.abort().await;
            return Err(error);
        }
        match self
            .archive
            .stored(&self.key)
            .await
            .map_err(TeeError::Archive)?
        {
            Some(stored) if stored.length == self.written => Ok(stored),
            stored => Err(TeeError::Length {
                path,
                written: self.written,
                stored: stored.map(|stored| stored.length),
            }),
        }
    }

    /// Discards the upload and its parts; a failed abort is logged with the id to abort by hand.
    pub async fn abort(self) {
        self.archive
            .abort_upload(&self.key.path(), &self.upload_id)
            .await;
    }

    async fn upload(&mut self, part: Vec<u8>) -> Result<(), TeeError> {
        let path = self.key.path();
        let number =
            i32::try_from(self.parts.len() + 1).expect("an upload holds at most 10,000 parts");
        let length = part.len() as u64;
        let body = bytes::Bytes::from(part);
        let mut attempt = 1;
        let uploaded = loop {
            match self
                .archive
                .s3_client
                .upload_part()
                .bucket(&self.archive.bucket_name)
                .key(&path)
                .upload_id(&self.upload_id)
                .part_number(number)
                .checksum_algorithm(ChecksumAlgorithm::Crc64Nvme)
                .body(ByteStream::from(body.clone()))
                .send()
                .await
            {
                Ok(uploaded) => break uploaded,
                Err(error) if attempt < PART_ATTEMPTS => {
                    tracing::warn!(path, part = number, attempt, error = %aws_sdk_s3::error::DisplayErrorContext(error), "Retrying a part");
                    tokio::time::sleep(Duration::from_secs(2_u64.pow(attempt))).await;
                    attempt += 1;
                }
                Err(error) => return Err(put_failed(&path, error)),
            }
        };
        self.parts.push(
            CompletedPart::builder()
                .part_number(number)
                .set_e_tag(uploaded.e_tag().map(String::from))
                .set_checksum_crc64_nvme(uploaded.checksum_crc64_nvme().map(String::from))
                .build(),
        );
        self.written += length;
        Ok(())
    }

    async fn complete(&mut self) -> Result<(), TeeError> {
        let path = self.key.path();
        self.archive
            .s3_client
            .complete_multipart_upload()
            .bucket(&self.archive.bucket_name)
            .key(&path)
            .upload_id(&self.upload_id)
            .checksum_type(ChecksumType::FullObject)
            .multipart_upload(
                CompletedMultipartUpload::builder()
                    .set_parts(Some(std::mem::take(&mut self.parts)))
                    .build(),
            )
            .send()
            .await
            .map_err(|error| put_failed(&path, error))?;
        Ok(())
    }
}

fn put_failed<E: std::error::Error + 'static>(path: &str, error: E) -> TeeError {
    TeeError::Archive(ArchiveError::Put {
        path: path.to_string(),
        reason: aws_sdk_s3::error::DisplayErrorContext(error).to_string(),
    })
}

/// The provenance entries an object's metadata carries.
fn metadata(provenance: &Provenance) -> HashMap<String, String> {
    provenance
        .entries()
        .into_iter()
        .filter_map(|(name, value)| value.map(|value| (<&str>::from(name).to_string(), value)))
        .collect()
}

/// The fetch time an object's metadata records, refusing a stamp that is present but unreadable.
fn fetched_at(metadata: Option<&HashMap<String, String>>) -> Result<Option<DateTime<Utc>>, String> {
    metadata
        .and_then(|metadata| metadata.get(<&str>::from(MetadataEntry::FetchedAt)))
        .map(|raw| {
            DateTime::parse_from_rfc3339(raw)
                .map(|instant| instant.with_timezone(&Utc))
                .map_err(|error| format!("fetch time {raw:?} unreadable: {error}"))
        })
        .transpose()
}

/// One part of an upload: its number, counted from one, its first byte, and how many bytes it holds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PartRange {
    number: i32,
    start: u64,
    length: NonZeroU64,
}

/// The parts a file of `length` bytes is uploaded in.
fn ranges(length: u64) -> impl Iterator<Item = PartRange> {
    (1..)
        .zip((0..length).step_by(PART_LENGTH as usize))
        .map(move |(number, start)| PartRange {
            number,
            start,
            length: NonZeroU64::new(PART_LENGTH.min(length - start))
                .expect("a part starts before the file ends"),
        })
}

fn storage_class(key: &Key) -> aws_sdk_s3::types::StorageClass {
    match key.storage_class() {
        StorageClass::Standard => aws_sdk_s3::types::StorageClass::Standard,
        StorageClass::DeepArchive => aws_sdk_s3::types::StorageClass::DeepArchive,
    }
}

/// One byte range of a file and the part of the upload it becomes.
struct Part {
    s3_client: aws_sdk_s3::Client,
    bucket_name: String,
    path: String,
    upload_id: String,
    range: PartRange,
}

impl Part {
    async fn upload(
        self,
        flat_files: &FlatFiles,
        dataset: FlatFileDataset,
        listed: &Listed,
    ) -> Result<CompletedPart, CopyError> {
        let mut attempt = 1;
        loop {
            match self.attempt(flat_files, dataset, listed).await {
                Ok(part) => return Ok(part),
                Err(error) if attempt < PART_ATTEMPTS => {
                    tracing::warn!(
                        path = self.path,
                        part = self.range.number,
                        attempt,
                        %error,
                        "Retrying a part"
                    );
                    tokio::time::sleep(Duration::from_secs(2_u64.pow(attempt))).await;
                    attempt += 1;
                }
                Err(error) => return Err(error),
            }
        }
    }

    async fn attempt(
        &self,
        flat_files: &FlatFiles,
        dataset: FlatFileDataset,
        listed: &Listed,
    ) -> Result<CompletedPart, CopyError> {
        let body = flat_files
            .range(dataset, listed, self.range.start, self.range.length)
            .await
            .map_err(CopyError::Source)?;
        let uploaded = self
            .s3_client
            .upload_part()
            .bucket(&self.bucket_name)
            .key(&self.path)
            .upload_id(&self.upload_id)
            .part_number(self.range.number)
            .checksum_algorithm(ChecksumAlgorithm::Crc64Nvme)
            .body(ByteStream::from(body))
            .send()
            .await
            .map_err(|error| {
                CopyError::Archive(ArchiveError::Put {
                    path: self.path.clone(),
                    reason: aws_sdk_s3::error::DisplayErrorContext(error).to_string(),
                })
            })?;
        Ok(CompletedPart::builder()
            .part_number(self.range.number)
            .set_e_tag(uploaded.e_tag().map(String::from))
            .set_checksum_crc64_nvme(uploaded.checksum_crc64_nvme().map(String::from))
            .build())
    }
}

#[cfg(test)]
mod tests {
    use proptest::strategy::Strategy;

    use super::*;

    const MEBIBYTE: u64 = 1024 * 1024;

    #[test]
    fn test_a_fetch_stamp_is_read_absent_or_refused_never_dropped() {
        let stamped = |raw: &str| HashMap::from([("fund.fetched_at".to_string(), raw.to_string())]);
        assert_eq!(
            fetched_at(Some(&stamped("2026-10-02T20:00:00-04:00")))
                .map(|instant| instant.map(|instant| instant.to_rfc3339())),
            Ok(Some("2026-10-03T00:00:00+00:00".to_string()))
        );
        assert_eq!(fetched_at(None), Ok(None));
        assert_eq!(fetched_at(Some(&HashMap::new())), Ok(None));
        assert!(fetched_at(Some(&stamped("yesterday"))).is_err());
    }

    #[test]
    fn test_only_a_full_object_checksum_is_kept_as_one() {
        assert_eq!(
            Checksum::of(Some(&ChecksumType::FullObject), Some("AAAAAAAAAAA=")),
            Checksum::FullObject(Crc64("AAAAAAAAAAA=".to_string()))
        );
        assert_eq!(
            Checksum::of(Some(&ChecksumType::Composite), Some("AAAAAAAAAAA=-3")),
            Checksum::Composite
        );
        assert_eq!(
            Checksum::of(None, Some("AAAAAAAAAAA=")),
            Checksum::Composite
        );
        assert_eq!(
            Checksum::of(Some(&ChecksumType::FullObject), None),
            Checksum::Absent
        );
    }

    #[test]
    fn test_parts_cover_every_byte_once_numbered_from_one() {
        let ranges = |length| {
            ranges(length)
                .map(|range| (range.number, range.start, range.length.get()))
                .collect::<Vec<_>>()
        };
        assert_eq!(ranges(0), vec![]);
        assert_eq!(ranges(1), vec![(1, 0, 1)]);
        assert_eq!(ranges(64 * MEBIBYTE), vec![(1, 0, 64 * MEBIBYTE)]);
        assert_eq!(
            ranges(128 * MEBIBYTE + 1),
            vec![
                (1, 0, 64 * MEBIBYTE),
                (2, 64 * MEBIBYTE, 64 * MEBIBYTE),
                (3, 128 * MEBIBYTE, 1),
            ]
        );
    }

    /// Every part `GzipLines` cut for `lines`, the last included.
    fn gzip_parts(lines: &[Vec<u8>], part_length: usize) -> Vec<Vec<u8>> {
        let mut gzip = GzipLines::new(part_length);
        let mut parts: Vec<Vec<u8>> = lines
            .iter()
            .filter_map(|line| gzip.push(line).unwrap())
            .collect();
        parts.extend(gzip.finish());
        parts
    }

    fn gunzip(parts: &[Vec<u8>]) -> Vec<u8> {
        let mut text = Vec::new();
        std::io::Read::read_to_end(
            &mut flate2::read::GzDecoder::new(&parts.concat()[..]),
            &mut text,
        )
        .unwrap();
        text
    }

    #[test]
    fn test_pages_are_stored_a_line_each() {
        let parts = gzip_parts(
            &[
                br#"{"quotes":{}}"#.to_vec(),
                br#"{"next_page_token":null}"#.to_vec(),
            ],
            1 << 20,
        );
        assert_eq!(parts.len(), 1);
        assert_eq!(
            gunzip(&parts),
            b"{\"quotes\":{}}\n{\"next_page_token\":null}\n"
        );
    }

    #[test]
    fn test_a_page_holding_a_line_break_is_refused() {
        assert_eq!(GzipLines::new(1 << 20).push(b"{}\n{}"), Err(LineBreak));
    }

    #[test]
    fn test_no_pages_is_no_part() {
        assert_eq!(GzipLines::new(1 << 20).finish(), None);
    }

    proptest::proptest! {
        /// However the stream is cut, every part but the last reaches the part length and together they unzip to the
        /// pages in order, a line each.
        #[test]
        fn property_parts_unzip_to_the_pages(
            lines in proptest::collection::vec(proptest::collection::vec(0_u8..=255, 0..2_000).prop_map(|line| line.into_iter().filter(|byte| *byte != b'\n').collect::<Vec<_>>()), 1..40),
            part_length in 1_usize..4_000,
        ) {
            let parts = gzip_parts(&lines, part_length);
            for part in &parts[..parts.len() - 1] {
                proptest::prop_assert!(part.len() >= part_length);
            }
            let mut expected = Vec::new();
            for line in &lines {
                expected.extend_from_slice(line);
                expected.push(b'\n');
            }
            proptest::prop_assert_eq!(gunzip(&parts), expected);
        }

        /// Parts are numbered from one, each starts where the last ended, every part but the last is a full 64 MiB, and
        /// together they are the file.
        #[test]
        fn property_parts_tile_the_file(
            length in proptest::prop_oneof![
                0_u64..=20 * 64 * MEBIBYTE + 3,
                (0_u64..=20, 0_u64..=2).prop_map(|(parts, offset)| (parts * 64 * MEBIBYTE + offset).saturating_sub(1)),
            ],
        ) {
            let parts: Vec<(i32, u64, u64)> = ranges(length)
                .map(|range| (range.number, range.start, range.length.get()))
                .collect();
            proptest::prop_assert_eq!(parts.len() as u64, length.div_ceil(64 * MEBIBYTE));
            let mut end = 0;
            for (index, (number, start, part)) in parts.iter().enumerate() {
                proptest::prop_assert_eq!(*number as usize, index + 1);
                proptest::prop_assert_eq!(*start, end);
                proptest::prop_assert!(*part <= 64 * MEBIBYTE);
                if index + 1 < parts.len() {
                    proptest::prop_assert_eq!(*part, 64 * MEBIBYTE);
                }
                end = start + part;
            }
            proptest::prop_assert_eq!(end, length);
        }
    }
}
