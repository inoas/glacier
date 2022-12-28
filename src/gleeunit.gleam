import gleam/list
import gleam/string

type Target {
  ErlangTarget
  JavaScriptTarget
}

/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  detect_test_modules()
  |> run_suit
}

/// Runs a specific list of test modules
///
pub fn run(for test_modules: List(String)) -> Nil {
  test_modules
  |> find_matching_test_module_files
  |> run_suit()
}

fn find_matching_test_module_files(test_modules) {
  test_modules
  |> list.map(fn(module_name) {
    let test_module_has_suffix = case target() {
      ErlangTarget ->
        string.ends_with(module_name, ".gleam") || string.ends_with(
          module_name,
          ".erl",
        )
      JavaScriptTarget ->
        string.ends_with(module_name, ".gleam") || string.ends_with(
          module_name,
          ".mjs",
        )
    }

    case test_module_has_suffix {
      True -> module_name
      False -> module_name <> ".gleam"
    }
  })
  |> list.filter(fn(module_name) {
    let absolute_module_file = get_cwd() <> "/test/" <> module_name
    file_exists(absolute_module_file)
  })
}

if erlang {
  import gleam/result
  import gleam/dynamic.{Dynamic}

  fn run_suit(test_modules: List(String)) -> Nil {
    let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

    let result =
      test_modules
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> result.unwrap(Error(dynamic.from(Nil)))

    let code = case result {
      Ok(_) -> 0
      Error(_) -> 1
    }
    halt(code)
  }

  external fn halt(Int) -> Nil =
    "erlang" "halt"

  fn gleam_to_erlang_module_name(path: String) -> String {
    path
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
  }

  fn detect_test_modules() -> List(String) {
    find_files(matching: "**/*.{erl,gleam}", in: "test")
  }

  external fn find_files(matching: String, in: String) -> List(String) =
    "gleeunit_ffi" "find_files"

  external type Atom

  type Encoding {
    Utf8
  }

  external fn dangerously_convert_string_to_atom(String, Encoding) -> Atom =
    "erlang" "binary_to_atom"

  type ReportModuleName {
    GleeunitProgress
  }

  type GleeunitProgressOption {
    Colored(Bool)
  }

  type EunitOption {
    Verbose
    NoTty
    Report(#(ReportModuleName, List(GleeunitProgressOption)))
  }

  external fn run_eunit(List(Atom), List(EunitOption)) -> Dynamic =
    "eunit" "test"

  external fn file_exists(absolute_file_name: String) -> Bool =
    "filelib" "is_regular"

  external fn get_cwd() -> String =
    "glacier_ffi" "get_cwd_as_binary"

  fn target() -> Target {
    ErlangTarget
  }
}

if javascript {
  external fn run_suit(test_modules: List(String)) -> Nil =
    "./gleeunit_ffi.mjs" "main"

  fn detect_test_modules() -> List(String) {
    // impl. node equivalent of find_files(matching: "**/*.{erl,gleam}", in: "test")
    todo
  }

  fn file_exists(absolute_file_name: String) -> Bool {
    // impl. node equivalent of "filelib" "is_regular"
    todo
  }

  fn get_cwd() -> String {
    // impl. node equivalent of "glacier_ffi" "get_cwd_as_binary"
    todo
  }

  fn target() -> Target {
    JavaScriptTarget
  }
}
