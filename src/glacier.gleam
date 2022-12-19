import gleeunit
import glacier/glacier_helpers

pub fn main() {
  glacier_helpers.io_println(
    "Welcome to Glacier - An incremental test runner for Gleam - Usage:

1. In your app, run: gleam add glacier

2. In your app's ./test/YOUR_PROJECT_test.gleam module change:

  import gleeunit

  pub fn main() {
    gleeunit.main()
  }

to:

  import glacier

  pub fn main() {
    glacier.run()
  }

3. In your app run: gleam test --target erlang -- --only-recently-saved=3
   or run: gleam test --target javascript -- --only-recently-saved=3
",
  )
}

pub type InPath {
  InSrcPath
  InTestPath
}

pub fn run() {
  start_watcher(fn(in_path, full_file_path) {
    case in_path {
      InSrcPath -> glacier_helpers.io_println("src changed: " <> full_file_path)
      InTestPath ->
        glacier_helpers.io_println("test changed: " <> full_file_path)
      _any -> glacier_helpers.io_println("./unknown:" <> full_file_path)
    }

    gleeunit.main(False)
  })
}

pub fn start_watcher(
  event_handler_fn: fn(in_path, full_file_path) -> Nil,
) -> Nil {
  do_start_watcher(event_handler_fn)
  Nil
}

if erlang {
  external fn do_start_watcher(callback) -> Nil =
    "glacier_ffi" "start_watcher"
}
