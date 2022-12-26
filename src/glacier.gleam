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

type ModuleKind {
  SrcModuleKind
  TestModuleKind
}

pub fn run() {
  io.println("Starting Glacier watcher…")
  file_change_watcher(fn(module_kind: ModuleKind, full_module_path: String) {
    let test_modules = case module_kind {
      SrcModuleKind ->
        detect_unique_import_module_dependencies(full_module_path)
        |> derive_test_modules_off_import_module_dependencies()
      TestModuleKind -> [full_module_path]
      unexpected_atom -> {
        io.debug(#("UNEXPECTED ATOM", unexpected_atom, "FOR", full_module_path))
        []
      }
    }
    io.debug(test_modules)
    // TODO: pass _test_modules to `gleam test`
    gleeunit.main(False)
    Nil
  })
}

fn file_change_watcher(
  file_change_handler: fn(ModuleKind, String) -> Nil,
) -> Nil {
  do_file_change_watcher(file_change_handler)
  Nil
}

if erlang {
  external fn do_file_change_watcher(
    file_change_handler: fn(ModuleKind, String) -> Nil,
  ) -> Nil =
    "glacier_ffi" "start_file_change_watcher"
}

if javascript {
  fn do_file_change_watcher(
    file_change_handler: fn(ModuleKind, String) -> Nil,
  ) -> Nil {
    todo
  }
}

fn detect_unique_import_module_dependencies(module_path: String) -> List(String) {
  // io.debug(module_path)
  []
}

fn derive_test_modules_off_import_module_dependencies(
  module_paths: List(String),
) -> List(String) {
  // io.debug(module_paths)
  []
}
