#[cfg(feature = "events")]
pub mod meta;
pub use meta::Event;

#[cfg(feature = "events")]
pub mod discord;
