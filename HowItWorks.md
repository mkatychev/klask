How it works:
### Native
* The binary runs and there's no `CHILD_APP_ENV_VAR` environment variable ⇾ no user code runs, only the GUI is displayed.
* The "Run" button in the GUI is pressed ⇾ `CHILD_APP_ENV_VAR` is set, the binary is started again. Arguments are passed to `stdin` and `stdout` is intercepted for displaying output.
* The binary is run with `CHILD_APP_ENV_VAR` ⇾ the user-provided closure is run.
### Wasm
* The main gui runs.
* The "Run" button in the GUI is pressed ⇾ the asynchronous function provided is used to create a new future.
* This future is repeatedly polled. Each time it is polled the gui is also repainted and each time the gui is repainted the function is polled. When the asynchronous function provided returns [`core::task::Poll::Ready`] it is no longer polled.