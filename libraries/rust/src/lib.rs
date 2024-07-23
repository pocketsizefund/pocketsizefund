//! Welcome to pocketsizefund!

pub mod logger;

// this is a module
#[cfg(feature = "polygon")]
// polygon
pub mod polygon;

// this is a module
#[cfg(feature = "schema")]
// this is a module
pub mod schema;
