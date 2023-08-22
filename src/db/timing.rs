use std::time::{Duration, Instant};

use owo_colors::{AnsiColors, OwoColorize};

#[derive(Debug)]
pub struct Timings {
    enabled: bool,
    passes: Vec<Timing>,
    current: Option<(String, Instant)>,
}

impl Timings {
    pub fn new(enabled: bool) -> Self {
        Self { enabled, passes: vec![], current: None }
    }

    pub fn start(&mut self, name: impl Into<String>) {
        if self.enabled {
            self.stop();
            self.current = Some((name.into(), Instant::now()));
        }
    }

    pub fn stop(&mut self) {
        if let Some((name, inst)) = self.current.take() {
            self.passes.push(Timing { name, duration: inst.elapsed() });
        }
    }

    pub fn print(&self) {
        if self.enabled {
            for t in &self.passes {
                t.print();
            }
        }
    }
}

#[derive(Debug)]
struct Timing {
    name: String,
    duration: Duration,
}

impl Timing {
    fn print(&self) {
        let millis = self.duration.as_millis();

        let color = if millis < 5 {
            AnsiColors::BrightCyan
        } else if millis < 20 {
            AnsiColors::BrightGreen
        } else if millis < 100 {
            AnsiColors::BrightYellow
        } else {
            AnsiColors::BrightRed
        };

        let time_str = if millis == 0 {
            format!("{}μs", self.duration.as_micros())
        } else {
            format!("{millis}ms")
        };

        println!("{: <25}{time_str}", self.name.color(color).bold());
    }
}
