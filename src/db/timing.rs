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
        let value = self.duration.as_millis();

        let color = if value < 5 {
            AnsiColors::BrightCyan
        } else if value < 20 {
            AnsiColors::BrightGreen
        } else if value < 100 {
            AnsiColors::BrightYellow
        } else {
            AnsiColors::BrightRed
        };

        println!("{:<20}{}ms", self.name.color(color).bold(), value);
    }
}
