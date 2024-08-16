use sysinfo::{CpuRefreshKind, RefreshKind, System};

#[derive(Debug, PartialEq)]
pub enum CpuArchitecture {
    X86,
    X86_64,
    AMD,
    AMD64,
    ARM,
    ARM64,
}

impl From<&str> for CpuArchitecture {
    fn from(s: &str) -> Self {
        match s {
            "x86" => CpuArchitecture::X86,
            "x86_64" => CpuArchitecture::X86_64,
            "amd" => CpuArchitecture::AMD,
            "amd64" => CpuArchitecture::AMD64,
            "arm" => CpuArchitecture::ARM,
            "arm64" => CpuArchitecture::ARM64,
            _ => panic!("Cpu architecture not yet supported"),
        }
    }
}

#[derive(Debug)]
pub struct Core {
    pub name: String,
    pub vendor_id: String,
    pub brand: String,
    pub frequency: u64,
}

#[derive(Debug)]
pub struct Cores {
    pub physical_count: usize,
    pub cores: Vec<Core>,
}

#[derive(Debug)]
pub struct CPU {
    pub architecture: CpuArchitecture,
    pub cores: Cores,
}

impl Default for CPU {
    fn default() -> Self {
        let mut system =
            System::new_with_specifics(RefreshKind::new().with_cpu(CpuRefreshKind::everything()));
        system.refresh_cpu_all();
        let physical_core_count = system
            .physical_core_count()
            .expect("failed to get physical core count");

        let mut cores = Vec::new();

        for core in system.cpus() {
            cores.push(Core {
                name: core.name().to_string(),
                vendor_id: core.vendor_id().to_string(),
                brand: core.brand().to_string(),
                frequency: core.frequency(),
            });
        }

        Self {
            architecture: CpuArchitecture::from(
                System::cpu_arch()
                    .expect("failed to get cpu architecture")
                    .as_str(),
            ),
            cores: Cores {
                physical_count: physical_core_count,
                cores: cores,
            },
        }
    }
}
