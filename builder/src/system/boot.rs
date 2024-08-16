use chrono::{DateTime, Duration, Utc};
use std::fmt;
use sysinfo::System;

pub struct Uptime(pub Duration);

impl fmt::Debug for Uptime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let duration = self.0;
        let days = duration.num_days();
        let hours = duration.num_hours() % 24;
        let minutes = duration.num_minutes() % 60;
        let seconds = duration.num_seconds() % 60;

        write!(f, "{} {}:{}:{}", days, hours, minutes, seconds)
    }
}

#[derive(Debug)]
pub struct Boot {
    pub boot_time: DateTime<Utc>,
    pub uptime: Uptime,
}

impl Default for Boot {
    fn default() -> Self {
        let boot_time = System::boot_time();
        let boot_time_utc = DateTime::from_timestamp(
            boot_time.try_into().expect("failed to convert boot time"),
            0,
        )
        .unwrap();

        let uptime = Duration::seconds(
            System::uptime()
                .try_into()
                .expect("failed to convert uptime"),
        );

        Self {
            boot_time: boot_time_utc,
            uptime: Uptime(uptime),
        }
    }
}
