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

type ModuleInPathAtom {
  ModuleInSrcPath
  ModuleInTestPath
}

pub fn run() {
  io.println("Starting Glacier watcher…")
  start_watcher(fn(module_in_path: ModuleInPathAtom, full_module_path: String) {
    let _test_modules = case module_in_path {
      ModuleInSrcPath ->
        detect_unique_import_module_dependencies(full_module_path)
        |> derive_test_modules_off_import_module_dependencies()
      ModuleInTestPath -> [full_module_path]
      _any -> {
        io.debug(#("unexpected", full_module_path))
        []
      }
    }
    // TODO: pass _test_modules to `gleam test`
    gleeunit.main(False)
    Nil
  })
}

fn start_watcher(
  file_change_handler: fn(ModuleInPathAtom, String) -> Nil,
) -> Nil {
  do_start_watcher(file_change_handler)
  Nil
}

if erlang {
  external fn do_start_watcher(
    file_change_handler: fn(ModuleInPathAtom, String) -> Nil,
  ) -> Nil =
    "glacier_ffi" "start_watcher"
}

if javascript {
  fn do_start_watcher(
    file_change_handler: fn(ModuleInPathAtom, String) -> Nil,
  ) -> Nil {
    todo
  }
}

fn detect_unique_import_module_dependencies(module_path: String) -> List(String) {
  io.debug(module_path)
  []
}

fn derive_test_modules_off_import_module_dependencies(
  module_paths: List(String),
) -> List(String) {
  io.debug(module_paths)
  []
}
