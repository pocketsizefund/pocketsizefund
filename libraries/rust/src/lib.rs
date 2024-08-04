//! Welcome to pocketsizefund!

// this is a module
#[cfg(feature = "polygon")]
// polygon
pub mod polygon;

// this is a module
#[cfg(feature = "schema")]
// this is a module
pub mod schema;

#[cfg(feature = "event")]
pub mod event;

#[cfg(feature = "data")]
pub mod data;
