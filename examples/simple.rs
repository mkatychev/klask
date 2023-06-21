#[cfg(target_arch = "wasm32")]
use clap::ArgMatches;
use clap::{arg, Command};
use klask::Settings;

fn main() {
    let app = Command::new("Example").arg(arg!(--debug <VALUE>).short('d'));
    #[cfg(not(target_arch = "wasm32"))]
    klask::run_app_native(app, Settings::default(), |matches| {
        println!("{:?}", matches.try_contains_id("debug"))
    });
    #[cfg(target_arch = "wasm32")]
    klask::run_app_web(app, Settings::default(), move |matches| {
        let inner =
            |matches: ArgMatches| async move { println!("{:?}", matches.try_contains_id("debug")) };
        inner(matches.clone())
    });
}
