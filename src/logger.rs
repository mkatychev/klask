use eframe::epaint::mutex::Mutex;
use log::LevelFilter;
use std::{
    fmt::Debug,
    sync::{Arc, OnceLock},
};

/// The [`Logger`]. It is static so it can be changed from outside klask from anywhere using [`Logger::set_max_level`].
// Only needed because [`log`] doesn't give direct access to the global boxed logger.
static LOGGER: OnceLock<Arc<Logger>> = OnceLock::new();

/// Implements [`log::Log`] to log messages to the klask gui.
/// # Panics
/// Panics on Wasm if attempts to log while already logging.
pub struct Logger {
    pub(crate) filter: Mutex<LevelFilter>,
    pub(crate) queue: Mutex<Vec<String>>,
}

impl Debug for Logger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Logger").finish_non_exhaustive()
    }
}

impl Logger {
    /// Initialize the [`Logger`] with the given [`LevelFilter`].
    ///
    /// Can be called multiple times because protected by [`OnceLock`].
    ///
    /// Won't set the filter if another thread intialized first.
    pub(crate) fn init(filter: LevelFilter) -> Result<Arc<Self>, log::SetLoggerError> {
        if LOGGER.set(Arc::new(Logger::new(filter))).is_ok() {
            log::set_max_level(filter);
            log::set_boxed_logger(Box::new(
                LOGGER.get().expect("Logger is set already").clone(),
            ))?;
        }
        Ok(LOGGER.get().expect("Logger is set already").clone())
    }

    /// Constructor for [`Logger`].
    fn new(filter: LevelFilter) -> Self {
        Self {
            filter: Mutex::new(filter),
            queue: Default::default(),
        }
    }

    /// Change the log level.
    pub fn set_max_level(filter: LevelFilter) {
        log::set_max_level(filter);
        LOGGER.get().map(|logger| *logger.filter.lock() = filter);
    }
}

impl log::Log for Logger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        metadata.level() <= *self.filter.lock()
    }

    /// Saves the log message so the gui can render it on the next frame.
    fn log(&self, record: &log::Record<'_>) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let msg = if let (Some(file), Some(line)) = (record.file(), record.line()) {
            let file = shorten_file_path(file);
            format!("[{}] {file}:{line}: {}", record.target(), record.args())
        } else {
            format!("[{}] {}", record.target(), record.args())
        };
        // Panics if lock can't be obtained
        self.queue.lock().push(msg);
        // TODO replace string return with formatted return
        /* self.queue.lock().push_back(match record.level() {
            log::Level::Trace => Box::new(move |ui: &mut Ui| ui.label(msg)),
            log::Level::Debug => Box::new(move |ui: &mut Ui| {
                ui.colored_label(Color32::from_rgb(255, 140, 0), msg)
            }),
            log::Level::Info => Box::new(move |ui: &mut Ui| ui.label(msg)),
            log::Level::Warn => {
                Box::new(move |ui: &mut Ui| ui.colored_label(Color32::YELLOW, msg))
            }
            log::Level::Error => {
                Box::new(move |ui: &mut Ui| ui.colored_label(Color32::RED, msg))
            }
        }) */
    }

    fn flush(&self) {
        self.queue.lock().clear()
    }
}

/// Shorten a path to a Rust source file.
///
/// Example input:
/// * `/Users/emilk/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.24.1/src/runtime/runtime.rs`
/// * `crates/rerun/src/main.rs`
/// * `/rustc/d5a82bbd26e1ad8b7401f6a718a9c57c96905483/library/core/src/ops/function.rs`
///
/// Example output:
/// * `tokio-1.24.1/src/runtime/runtime.rs`
/// * `rerun/src/main.rs`
/// * `core/src/ops/function.rs`
#[allow(dead_code)] // only used on web and in tests
fn shorten_file_path(file_path: &str) -> &str {
    if let Some(i) = file_path.rfind("/src/") {
        if let Some(prev_slash) = file_path[..i].rfind('/') {
            &file_path[prev_slash + 1..]
        } else {
            file_path
        }
    } else {
        file_path
    }
}

#[test]
fn test_shorten_file_path() {
    for (before, after) in [
        ("/Users/emilk/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.24.1/src/runtime/runtime.rs", "tokio-1.24.1/src/runtime/runtime.rs"),
        ("crates/rerun/src/main.rs", "rerun/src/main.rs"),
        ("/rustc/d5a82bbd26e1ad8b7401f6a718a9c57c96905483/library/core/src/ops/function.rs", "core/src/ops/function.rs"),
        ("/weird/path/file.rs", "/weird/path/file.rs"),
        ]
    {
        assert_eq!(shorten_file_path(before), after);
    }
}
