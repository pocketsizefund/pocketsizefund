use crate::system::Bytes;
use sysinfo::System;

#[derive(Debug)]
pub struct Memory {
    pub total: Bytes,
    pub swap: Bytes,
}

impl Default for Memory {
    fn default() -> Self {
        let mut system = System::new();
        system.refresh_memory();

        Self {
            total: Bytes(system.total_memory()),
            swap: Bytes(system.total_swap()),
        }
    }
}
