use sysinfo::System;

#[derive(Debug)]
pub struct Version {
    pub short: String,
    pub long: String,
}

#[derive(Debug)]
pub struct OS {
    pub host_name: String,
    pub version: Version,
    pub kernel_version: String,
    pub name: String,
    pub distribution: String,
}

impl Default for OS {
    fn default() -> Self {
        Self {
            host_name: System::host_name().expect("failed to get host name"),
            version: Version {
                short: System::os_version().expect("failed to get os version"),
                long: System::long_os_version().expect("failed to get os type"),
            },
            kernel_version: System::kernel_version().expect("failed to get kernel version"),
            name: System::name().expect("failed to get os name"),
            distribution: System::distribution_id(),
        }
    }
}
