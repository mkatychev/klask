use clap::Parser;
use klask::Settings;

#[derive(Debug, Parser)]
struct Opts {
    #[arg(long)]
    opt1: Option<String>,
    #[arg(long)]
    opt2: Option<String>,
}

fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    klask::run_derived_native::<Opts, _>(Settings::default(), |opt| println!("{opt:?}"));
    #[cfg(target_arch = "wasm32")]
    klask::run_derived_web::<Opts, _>(
        Settings::default(),
        |opt| async move { println!("{opt:?}") },
    );
}
