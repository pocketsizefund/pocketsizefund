use rusoto_core::Region;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Aws {
    pub region: Region,
}

impl Default for Aws {
    fn default() -> Self {
        Self {
            region: Region::UsEast1,
        }
    }
}
