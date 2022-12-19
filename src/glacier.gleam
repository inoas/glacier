import gleeunit
import gleam/io

pub fn main() {
  io.println(
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

type InPathAtoms {
  InSrcPath
  InTestPath
}

pub fn run() {
  io.println("Starting Glacier watcher…")
  start_watcher(fn(in_path: InPathAtoms, full_file_path: String) {
    case in_path {
      InSrcPath -> io.println("src changed: " <> full_file_path)
      InTestPath -> io.println("test changed: " <> full_file_path)
      _any -> io.println("./unknown:" <> full_file_path)
    }
    gleeunit.main(False)
    Nil
  })
}

fn start_watcher(file_change_handler: fn(InPathAtoms, String) -> Nil) -> Nil {
  do_start_watcher(file_change_handler)
  Nil
}

if erlang {
  external fn do_start_watcher(
    file_change_handler: fn(InPathAtoms, String) -> Nil,
  ) -> Nil =
    "glacier_ffi" "start_watcher"
}

if javascript {
  fn do_start_watcher(
    file_change_handler: fn(InPathAtoms, String) -> Nil,
  ) -> Nil {
    todo
  }
}
