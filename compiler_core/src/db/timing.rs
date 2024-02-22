use std::{
    fmt,
    time::{Duration, Instant},
};

use owo_colors::{AnsiColors, OwoColorize};

#[derive(Debug)]
pub(super) struct Timings {
    pub(super) passes: Vec<Timing>,
    current: Option<(String, Instant)>,
}

impl Timings {
    pub(super) fn new() -> Self {
        Self { passes: vec![], current: None }
    }

    pub(super) fn start(&mut self, name: impl Into<String>) {
        self.stop();
        self.current = Some((name.into(), Instant::now()));
    }

    pub(super) fn stop(&mut self) {
        if let Some((name, inst)) = self.current.take() {
            self.passes.push(Timing { name, duration: inst.elapsed() });
        }
    }

    pub(super) fn print(&self) {
        for t in &self.passes {
            t.print();
        }

        let total: Duration = self.passes.iter().map(|p| p.duration).sum();

        print_row("total".color(AnsiColors::BrightWhite).bold(), duration_display(total));
    }
}

impl Default for Timings {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub(super) struct Timing {
    pub(super) name: String,
    pub(super) duration: Duration,
}

impl Timing {
    pub(super) fn print(&self) {
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

        print_row(self.name.color(color).bold(), duration_display(self.duration));
    }
}

fn print_row(title: impl fmt::Display, time: impl fmt::Display) {
    println!("{title: <25}{time}");
}

fn duration_display(duration: Duration) -> impl fmt::Display {
    let ms = duration.as_millis();

    if ms == 0 {
        format!("{}μs", duration.as_micros())
    } else {
        format!("{ms}ms")
    }
}
