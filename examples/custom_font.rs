//! Showcases custom fonts
use clap::{Parser, ValueHint};
use klask::Settings;
use std::{borrow::Cow, path::PathBuf};

#[derive(Parser)]
struct Font {
    #[arg(long, value_hint = ValueHint::AnyPath)]
    żółć: PathBuf,
}

fn main() {
    let mut settings = Settings::default();
    settings.custom_font = Some(Cow::Borrowed(include_bytes!(r"font/Lato-Bold.ttf")));

    #[cfg(not(target_arch = "wasm32"))]
    klask::run_derived_native::<Font, _>(settings, |_| {});
    #[cfg(target_arch = "wasm32")]
    klask::run_derived_web::<Font, _>(settings, |_| async {});
}
