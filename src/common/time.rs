use owo_colors::{AnsiColors, OwoColorize};
use std::time::Duration;
use stopwatch::Stopwatch as SW;

pub(crate) struct Stopwatch<'s> {
    label: &'s str,
    sw: SW,
}

impl<'s> Stopwatch<'s> {
    #[allow(unused)]
    pub(crate) fn new(label: &'s str) -> Self {
        Self { label, sw: SW::new() }
    }

    pub(crate) fn start_new(label: &'s str) -> Self {
        Self { label, sw: SW::start_new() }
    }

    #[allow(unused)]
    pub(crate) fn start(&mut self) {
        self.sw.start();
    }

    pub(crate) fn elapsed(&self) -> Duration {
        self.sw.elapsed()
    }

    pub(crate) fn print(&self) {
        let value = self.elapsed().as_millis();

        let color = if value < 5 {
            AnsiColors::BrightCyan
        } else if value < 20 {
            AnsiColors::BrightGreen
        } else if value < 100 {
            AnsiColors::BrightYellow
        } else {
            AnsiColors::BrightRed
        };

        println!("{:<16}{}ms", self.label.color(color).bold(), value);
    }
}

pub(crate) fn time<T>(enabled: bool, label: &str, f: impl FnOnce() -> T) -> T {
    if enabled {
        let sw = Stopwatch::start_new(label);
        let res = f();
        sw.print();
        res
    } else {
        f()
    }
}
