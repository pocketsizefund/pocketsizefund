use humansize::{format_size, DECIMAL};
use std::fmt;

pub mod os;
pub use os::OS;

pub mod cpu;
pub use cpu::CPU;

pub mod memory;
pub use memory::Memory;

pub mod boot;
pub use boot::Boot;

use opencl3::platform;

pub struct Bytes(pub u64);
impl fmt::Debug for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.0, format_size(self.0, DECIMAL))
    }
}

#[derive(Debug)]
pub struct Platform {
    pub name: String,
    pub profile: String,
    pub version: Option<String>,
    pub vendor: String,
}

#[derive(Debug)]
pub struct System {
    pub os: OS,
    pub cpu: CPU,
    pub memory: Memory,
    pub boot: Boot,
    pub platforms: Vec<Platform>,
}

impl Default for System {
    fn default() -> Self {
        let mut platforms = Vec::new();
        for platform in platform::get_platforms().unwrap() {
            platforms.push(Platform {
                name: platform.name().expect("failed to get platform name"),
                profile: platform.profile().expect("failed to get platform profile"),
                vendor: platform.vendor().expect("failed to get platform vendor"),
                version: Some(platform.version().expect("failed to get platform version")),
            })
        }
        Self {
            os: OS::default(),
            cpu: CPU::default(),
            memory: Memory::default(),
            boot: Boot::default(),
            platforms,
        }
    }
}
