#![warn(missing_docs)]
//! You can use [`run_app_native`]/[`run_app_web`] for [`Command`]s created manually or generated from yaml and
//! [`run_derived_native`]/[`run_derived_web`] for [`Command`]s derived from a struct. Both of these functions take
//! a closure that contains the code that would normally be in `main`. They should be
//! the last thing you call in `main`.
//!
//! For example
//! ```no_run
//! # use clap::{Command, arg};
//! # use klask::Settings;
//! fn main() {
//!     let app = Command::new("Example").arg(arg!(--debug <VALUE>).short('d'));
//!     klask::run_app_native(app, Settings::default(), |matches| {
//!        println!("{:?}", matches.try_contains_id("debug"))
//!     });
//! }
//! ```
//! corresponds to
//! ```no_run
//! # use clap::{Command, arg};
//! fn main() {
//!     let app = Command::new("Example").arg(arg!(--debug <VALUE>).short('d'));
//!     let matches = app.get_matches();
//!     println!("{:?}", matches.try_contains_id("debug"))
//! }
//! ```
//!
#![doc = include_str!("../HowItWorks.md")]

/// App state
pub mod app_state;
mod arg_state;
mod child_app;
mod error;
#[cfg(target_arch = "wasm32")]
/// Logger that outputs all logs to the gui output.
pub mod logger;
/// Additional options for output like progress bars.
pub mod output;
/// Settings
pub mod settings;

use app_state::AppState;
use child_app::{ChildApp, StdinType};
use clap::{ArgMatches, Command, CommandFactory, FromArgMatches};
use eframe::{
    egui::{self, Button, Color32, Context, FontData, FontDefinitions, Grid, Style, TextEdit, Ui},
    CreationContext, Frame,
};
use error::ExecutionError;
use output::Output;
#[cfg(not(target_arch = "wasm32"))]
use rfd::FileDialog;
pub use settings::{Localization, Settings};
use std::{borrow::Cow, hash::Hash};
#[cfg(target_arch = "wasm32")]
use std::{future::Future, sync::Arc, task::Poll};

#[cfg(not(target_arch = "wasm32"))]
const CHILD_APP_ENV_VAR: &str = "KLASK_CHILD_APP";

/// Call with a [`Command`] and a closure that contains the code that would normally be in `main`.
/// ```no_run
/// # use clap::{Command, arg};
/// # use klask::Settings;
/// let app = Command::new("Example").arg(arg!(--debug <VALUE>).short('d'));
/// klask::run_app_native(app, Settings::default(), |matches| {
///    println!("{:?}", matches.try_contains_id("debug"))
/// });
/// ```
#[cfg(not(target_arch = "wasm32"))]
pub fn run_app_native(app: Command, settings: Settings, f: impl FnOnce(&ArgMatches)) {
    if std::env::var(CHILD_APP_ENV_VAR).is_ok() {
        std::env::remove_var(CHILD_APP_ENV_VAR);

        let matches = app
            .try_get_matches()
            .expect("Internal error, arguments should've been verified by the GUI app");

        f(&matches);
    } else {
        // During validation we don't pass in a binary name
        let app = app.no_binary_name(true);
        let app_name = app.get_name().to_string();

        // eframe::run_native requires that Box::new(klask) has 'static
        // lifetime, so we must leak here. But it never returns (return value !)
        // so it should be ok.
        let localization = Box::leak(Box::new(settings.localization));

        let mut klask = Klask {
            state: AppState::new(&app, localization, settings.prefer_long_about),
            tab: Tab::Arguments,
            env: settings.enable_env.map(|desc| (desc, vec![])),
            stdin: settings
                .enable_stdin
                .map(|desc| (desc, StdinType::Text(String::new()))),
            working_dir: settings
                .enable_working_dir
                .map(|desc| (desc, String::new())),
            output: Output::None,
            app,
            custom_font: settings.custom_font,
            localization,
            style: settings.style,
            platform_state: Native {},
        };
        let native_options = eframe::NativeOptions::default();
        eframe::run_native(
            app_name.as_str(),
            native_options,
            Box::new(|cc| {
                klask.setup(cc);
                Box::new(klask)
            }),
        )
        .unwrap();
    }
}

/// Can be used with a struct deriving [`clap::Parser`]. Call with a closure that contains the code that would normally be in `main`.
/// It's just a wrapper over [`run_app_native`].
/// ```no_run
/// # use clap::Parser;
/// # use klask::Settings;
/// #[derive(Parser)]
/// struct Example {
///     #[arg(short)]
///     debug: bool,
/// }
///
/// klask::run_derived_native::<Example, _>(Settings::default(), |example|{
///     println!("{}", example.debug);
/// });
/// ```
#[cfg(not(target_arch = "wasm32"))]
pub fn run_derived_native<C, F>(settings: Settings, f: F)
where
    C: CommandFactory + FromArgMatches,
    F: FnOnce(C),
{
    run_app_native(C::command(), settings, |m| {
        let matches = C::from_arg_matches(m)
            .expect("Internal error, C::from_arg_matches should always succeed");
        f(matches);
    });
}

/// Call with a [`Command`] and a closure that contains the code that would normally be in `main`.
/// Slightly more complicated to use then [`run_app_native`] because async closures don't exist yet.
/// ```no_run
/// let app = clap::Command::new("name of a valid html canvas id").arg(
///     clap::Arg::new("debug")
///         .short('d')
///         .long("debug")
///         .action(clap::ArgAction::SetTrue)
///         .help("turns on debugging mode"),
/// );
///
/// klask::run_app_web(app, klask::Settings::default(), |matches| {
///     let main = |matches: clap::ArgMatches| async move {
///         // (Optional) Set output level.
///         klask::logger::Logger::set_max_level(log::LevelFilter::Info);
///         log::info!("{}", matches.get_flag("debug"));
///         println!("{}", matches.get_flag("debug")); // prints don't work on wasm so this line doesn't do anything.
///     };
///     main(matches.clone())
/// });
/// ```
#[cfg(target_arch = "wasm32")]
pub fn run_app_web<F>(app: Command, settings: Settings, fut_factory: F)
where
    F: FutFactory + 'static,
    F::Fut: 'static,
{
    // Wasm doesn't pass binary name.
    let app = app.no_binary_name(true);
    let app_name = app.get_name().to_string();

    // eframe's WebRunner requires that Box::new(klask) has 'static
    // lifetime, so we must leak here. But it never returns (return value !)
    // so it should be ok.
    let localization = Box::leak(Box::new(settings.localization));

    let mut klask = Klask {
        state: AppState::new(&app, localization, settings.prefer_long_about),
        tab: Tab::Arguments,
        env: settings.enable_env.map(|desc| (desc, vec![])),
        stdin: settings
            .enable_stdin
            .map(|desc| (desc, StdinType::Text(String::new()))),
        working_dir: settings
            .enable_working_dir
            .map(|desc| (desc, String::new())),
        output: Output::None,
        app,
        custom_font: settings.custom_font,
        localization,
        style: settings.style,
        platform_state: Wasm {
            fut_factory,
            // Init without logging so eframe's setup output is ignored. Library user can use [`Logger::set_max_level`] to change.
            logger: logger::Logger::init(log::LevelFilter::Off).unwrap(),
        },
    };
    let web_options = eframe::WebOptions::default();

    // Spawn the webrunner so the gui renders.
    wasm_bindgen_futures::spawn_local(async move {
        eframe::WebRunner::new()
            .start(
                Box::leak(Box::new(app_name)), // Webrunner requires '&static str for some reason so have to leak. Should require String.
                web_options,
                Box::new(|cc| {
                    klask.setup(cc);
                    Box::new(klask)
                }),
            )
            .await
            .unwrap();
    });
}

/// Can be used with a struct deriving [`clap::Parser`]. Call with a closure that contains the code that would normally be in `async main`.
/// It's just a wrapper over [`run_app_web`].
/// Note: html canvas id in html must match the crate name when deriving
/// ```no_run
/// use clap::Parser;
/// use klask::Settings;
///
/// #[derive(Parser)]
/// struct Example {
///     #[arg(short)]
///     debug: bool,
/// }
///
/// klask::run_derived_web::<Example, _>(Settings::default(), |example| async move {
///     // (Optional) Set output level.
///     klask::logger::Logger::set_max_level(log::LevelFilter::Info);
///     log::info!("{}", example.debug);
///     println!("{}", example.debug); // prints don't work on wasm so this line doesn't do anything.
/// });
/// ```
#[cfg(target_arch = "wasm32")]
pub fn run_derived_web<C, Fut>(settings: Settings, mut f: impl FnMut(C) -> Fut + 'static)
where
    C: CommandFactory + FromArgMatches + 'static,
    Fut: Future<Output = ()> + 'static,
{
    run_app_web(C::command(), settings, move |m| {
        f(C::from_arg_matches(m)
            .expect("Internal error, C::from_arg_matches should always succeed"))
    });
}

/// Platform specific state for Klask on native.
#[cfg(not(target_arch = "wasm32"))]
struct Native {}
/// Platform specific state for Klask on wasm.
#[cfg(target_arch = "wasm32")]
struct Wasm<F> {
    /// The function given to klask to create futures which would act as an `async main` in a normal program.
    fut_factory: F,
    // A reference to the logger containing a queue of all unprocessed messages.
    logger: Arc<logger::Logger>,
}
/// State design pattern.
trait PlatformState {}
#[cfg(not(target_arch = "wasm32"))]
impl PlatformState for Native {}
#[cfg(target_arch = "wasm32")]
impl<F> PlatformState for Wasm<F> {}

/// Function that can repeatedly create futures.
#[cfg(target_arch = "wasm32")]
pub trait FutFactory: FnMut(&ArgMatches) -> Self::Fut {
    /// The type of future created by the function.
    type Fut: Future<Output = ()>;
}
/// Implement the [`FutFactory`]  where it can be implemented.
#[cfg(target_arch = "wasm32")]
impl<T: FnMut(&ArgMatches) -> FutType, FutType> FutFactory for T
where
    FutType: Future<Output = ()>,
{
    type Fut = FutType;
}

/// Common parts of wasm and non wasm klask
trait KlaskTrait<'s> {
    fn is_child_running(&self) -> bool;
    fn kill_child(&mut self);
    fn try_start_execution(&mut self, ctx: egui::Context) -> Result<ChildApp, ExecutionError>;
    fn update_stdin(&mut self, _: &mut Ui);
}

/// Main object representing the [`egui`] ui. The lifetime is approximitely static and annotated as `'s`
#[derive(Debug)]
struct Klask<'s, PlatformState> {
    state: AppState<'s>,
    tab: Tab,
    /// First string is a description
    env: Option<(String, Vec<(String, String)>)>,
    /// First string is a description
    stdin: Option<(String, StdinType)>,
    /// First string is a description
    working_dir: Option<(String, String)>,
    output: Output,
    app: Command,

    custom_font: Option<Cow<'static, [u8]>>,
    localization: &'s Localization,
    style: Style,

    #[allow(dead_code)]
    platform_state: PlatformState,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Tab {
    Arguments,
    Env,
    Stdin,
}

impl<'s, State> eframe::App for Klask<'s, State>
where
    Self: KlaskTrait<'s>,
{
    fn update(&mut self, ctx: &Context, _frame: &mut Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::vertical().show(ui, |ui| {
                // Tab selection
                let tab_count =
                    1 + usize::from(self.env.is_some()) + usize::from(self.stdin.is_some());

                if tab_count > 1 {
                    ui.columns(tab_count, |ui| {
                        let mut index = 0;
                        ui[index].selectable_value(
                            &mut self.tab,
                            Tab::Arguments,
                            &self.localization.arguments,
                        );
                        index += 1;

                        if self.env.is_some() {
                            ui[index].selectable_value(
                                &mut self.tab,
                                Tab::Env,
                                &self.localization.env_variables,
                            );
                            index += 1;
                        }
                        if self.stdin.is_some() {
                            ui[index].selectable_value(
                                &mut self.tab,
                                Tab::Stdin,
                                &self.localization.input,
                            );
                        }
                    });

                    ui.separator();
                }

                // Display selected tab
                match self.tab {
                    Tab::Arguments => {
                        ui.add(&mut self.state);

                        // Working dir
                        if let Some((ref desc, path)) = &mut self.working_dir {
                            if !desc.is_empty() {
                                ui.label(desc);
                            }

                            ui.horizontal(|ui| {
                                if ui.button(&self.localization.select_directory).clicked() {
                                    #[cfg(not(target_arch = "wasm32"))]
                                    if let Some(file) = FileDialog::new().pick_folder() {
                                        *path = file.to_string_lossy().into_owned();
                                    }
                                }
                                ui.add(
                                    TextEdit::singleline(path)
                                        .hint_text(&self.localization.working_directory),
                                )
                            });
                            ui.add_space(10.0);
                        }
                    }
                    Tab::Env => self.update_env(ui),
                    Tab::Stdin => self.update_stdin(ui),
                }

                // Run button row
                ui.horizontal(|ui| {
                    if ui
                        .add_enabled(
                            !self.is_child_running(),
                            Button::new(&self.localization.run),
                        )
                        .clicked()
                    {
                        match self.try_start_execution(ctx.clone()) {
                            Ok(child) => {
                                // Reset
                                self.state.update_validation_error("", "");
                                self.output = Output::new_with_child(child);
                            }
                            Err(err) => {
                                if let ExecutionError::ValidationError { name, message } = &err {
                                    self.state.update_validation_error(name, message);
                                }
                                self.output = Output::Err(err);
                            }
                        }
                    }

                    if self.is_child_running() && ui.button(&self.localization.kill).clicked() {
                        self.kill_child();
                    }

                    if self.is_child_running() {
                        let mut running_text = String::from(&self.localization.running);
                        for _ in 0..((2.0 * ui.input(|i| i.time)) as i32 % 4) {
                            running_text.push('.');
                        }
                        ui.label(running_text);
                    }
                });

                // If on wasm must poll the child to make progress.
                #[cfg(target_arch = "wasm32")]
                if let Some(ref mut child) = self.child_mut() {
                    match child.poll() {
                        Poll::Ready(()) => self.kill_child(),
                        Poll::Pending => (),
                    }
                };
                ui.add(&mut self.output);
            });
        });
    }
}

impl<'s, State> Klask<'s, State>
where
    Self: KlaskTrait<'s>,
{
    #[allow(dead_code)]
    fn child(&self) -> Option<&ChildApp> {
        match &self.output {
            Output::Child(child, _) => Some(child),
            _ => None,
        }
    }

    #[allow(dead_code)]
    fn child_mut(&mut self) -> Option<&mut ChildApp> {
        match &mut self.output {
            Output::Child(child, _) => Some(child),
            _ => None,
        }
    }

    fn setup(&mut self, cc: &CreationContext) {
        cc.egui_ctx.set_style(self.style.clone());

        if let Some(custom_font) = self.custom_font.take() {
            let font_name = String::from("custom_font");
            let mut fonts = FontDefinitions::default();

            fonts.font_data.insert(
                font_name.clone(),
                FontData {
                    font: custom_font,
                    index: 0,
                    tweak: Default::default(),
                },
            );

            fonts
                .families
                .entry(egui::FontFamily::Proportional)
                .or_default()
                .insert(0, font_name.clone());

            fonts
                .families
                .entry(egui::FontFamily::Monospace)
                .or_default()
                .push(font_name);

            cc.egui_ctx.set_fonts(fonts);
        }
    }

    fn update_env(&mut self, ui: &mut Ui) {
        let (ref desc, env) = self.env.as_mut().unwrap();

        if !desc.is_empty() {
            ui.label(desc);
        }

        if !env.is_empty() {
            let mut remove_index = None;

            Grid::new(Tab::Env)
                .striped(true)
                // We can't just divide by 2, without taking spacing into account
                // Instead we just set num_columns, and the second column will fill
                .min_col_width(ui.available_width() / 3.0)
                .num_columns(2)
                .show(ui, |ui| {
                    for (index, (key, value)) in env.iter_mut().enumerate() {
                        ui.horizontal(|ui| {
                            if ui.small_button("-").clicked() {
                                remove_index = Some(index);
                            }

                            if key.is_empty() {
                                set_error_style(ui);
                            }

                            ui.text_edit_singleline(key);

                            if key.is_empty() {
                                ui.reset_style();
                            }
                        });

                        ui.horizontal(|ui| {
                            ui.label("=");
                            ui.text_edit_singleline(value);
                        });

                        ui.end_row();
                    }
                });

            if let Some(remove_index) = remove_index {
                env.remove(remove_index);
            }
        }

        if ui.button(&self.localization.new_value).clicked() {
            env.push(Default::default());
        }

        ui.separator();
    }
}

#[cfg(target_arch = "wasm32")]
impl<'s, F> KlaskTrait<'s> for Klask<'s, Wasm<F>>
where
    F: FutFactory,
    <F as FutFactory>::Fut: 'static,
{
    fn is_child_running(&self) -> bool {
        // No child process to monitor. If it is stored and not killed it is "running".
        if let Some(child) = self.child() {
            child.is_running()
        } else {
            false
        }
    }

    fn kill_child(&mut self) {
        // No child process to kill. Just tell the child to die.
        self.child_mut().map(|child| child.kill());
    }

    fn try_start_execution(&mut self, ctx: egui::Context) -> Result<ChildApp, ExecutionError> {
        let args = self.state.get_cmd_args(vec![])?;

        // Check for validation errors and get matches for arguments.
        let matches = self.app.try_get_matches_from_mut(args.iter())?;

        if self
            .env
            .as_ref()
            .and_then(|(_, v)| v.iter().find(|(key, _)| key.is_empty()))
            .is_some()
        {
            return Err(self
                .localization
                .error_env_var_cant_be_empty
                .as_str()
                .into());
        }

        Ok(ChildApp::new(
            ctx,
            (self.platform_state.fut_factory)(&matches),
            self.platform_state.logger.clone(),
        ))
    }

    fn update_stdin(&mut self, _: &mut Ui) {
        unimplemented!("This is invalid for wasm klask. No stdin in wasm to direct to.")
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl<'s> KlaskTrait<'s> for Klask<'s, Native> {
    fn is_child_running(&self) -> bool {
        match &self.output {
            Output::Child(child, _) => child.is_running(),
            _ => false,
        }
    }

    fn kill_child(&mut self) {
        if let Output::Child(child, _) = &mut self.output {
            child.kill();
        }
    }

    fn try_start_execution(&mut self, ctx: egui::Context) -> Result<ChildApp, ExecutionError> {
        let args = self.state.get_cmd_args(vec![])?;

        // Check for validation errors
        self.app.try_get_matches_from_mut(args.iter())?;

        if self
            .env
            .as_ref()
            .and_then(|(_, v)| v.iter().find(|(key, _)| key.is_empty()))
            .is_some()
        {
            return Err(self
                .localization
                .error_env_var_cant_be_empty
                .as_str()
                .into());
        }

        ChildApp::run(
            args,
            self.env.clone().map(|(_, env)| env),
            self.stdin.clone().map(|(_, stdin)| stdin),
            self.working_dir.clone().map(|(_, dir)| dir),
            ctx,
        )
    }

    fn update_stdin(&mut self, ui: &mut Ui) {
        let (ref desc, stdin) = self.stdin.as_mut().unwrap();

        if !desc.is_empty() {
            ui.label(desc);
        }

        ui.columns(2, |ui| {
            if ui[0]
                .selectable_label(matches!(stdin, StdinType::Text(_)), &self.localization.text)
                .clicked()
                && matches!(stdin, StdinType::File(_))
            {
                *stdin = StdinType::Text(String::new());
            }
            if ui[1]
                .selectable_label(matches!(stdin, StdinType::File(_)), &self.localization.file)
                .clicked()
                && matches!(stdin, StdinType::Text(_))
            {
                *stdin = StdinType::File(String::new());
            }
        });

        match stdin {
            StdinType::File(path) => {
                ui.horizontal(|ui| {
                    if ui.button(&self.localization.select_file).clicked() {
                        #[cfg(not(target_arch = "wasm32"))]
                        if let Some(file) = FileDialog::new().pick_file() {
                            *path = file.to_string_lossy().into_owned();
                        }
                    }
                    ui.text_edit_singleline(path);
                });
            }
            StdinType::Text(text) => {
                ui.text_edit_multiline(text);
            }
        };
    }
}

fn set_error_style(ui: &mut Ui) {
    let style = ui.style_mut();
    style.visuals.widgets.inactive.bg_stroke.color = Color32::RED;
    style.visuals.widgets.inactive.bg_stroke.width = 1.0;
    style.visuals.widgets.hovered.bg_stroke.color = Color32::RED;
    style.visuals.widgets.active.bg_stroke.color = Color32::RED;
    style.visuals.widgets.open.bg_stroke.color = Color32::RED;
    style.visuals.widgets.noninteractive.bg_stroke.color = Color32::RED;
    style.visuals.selection.stroke.color = Color32::RED;
}

fn append_on_new_word(mut result: String, first_word: bool, character: char) -> String {
    if !first_word {
        result.push(' ');
    }
    if first_word {
        result.push(character.to_ascii_uppercase());
    } else {
        result.push(character.to_ascii_lowercase());
    }
    result
}

fn is_not_alphanumeric(character: char) -> bool {
    !character.is_alphanumeric()
}

/// Sentence case from <https://github.com/whatisinternet/Inflector>
pub fn to_sentence_case(convertable_string: &str) -> String {
    let mut new_word: bool = true;
    let mut first_word: bool = true;
    let mut last_char: char = ' ';
    let mut found_real_char: bool = false;
    let mut result: String = String::with_capacity(convertable_string.len() * 2);

    for character in convertable_string
        .trim_end_matches(is_not_alphanumeric)
        .chars()
    {
        if !character.is_alphanumeric() && found_real_char {
            new_word = true;
        } else if !found_real_char && !character.is_alphanumeric() {
            continue;
        } else if character.is_numeric() {
            found_real_char = true;
            new_word = true;
            result.push(character);
        } else if new_word
            || ((last_char.is_lowercase() && character.is_uppercase()) && (last_char != ' '))
        {
            found_real_char = true;
            new_word = false;
            result = append_on_new_word(result, first_word, character);
            first_word = false;
        } else {
            found_real_char = true;
            last_char = character;
            result.push(character.to_ascii_lowercase());
        }
    }
    result
}
