import gleam/io
import gleam/list
import gleam/function

/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  detect_all_test_modules()
  |> run_suite(halts_on_error: True)
}

/// Runs a specific list of test modules
///
pub fn run(for test_module_files: List(String)) -> Nil {
  test_module_files
  |> find_matching_test_module_files
  |> run_suite(halts_on_error: False)
}

fn find_matching_test_module_files(test_module_files) {
  test_module_files
  |> list.filter(fn(module_name) {
    let absolute_module_file_name = get_cwd() <> "/" <> module_name

    file_exists(absolute_module_file_name)
    |> function.tap(fn(exists) {
      case exists {
        True -> Nil
        // TODO: gleam 0.26 io.print_error
        // TODO: exit/halt via erlang/node
        False ->
          io.println("Error: Could not find " <> absolute_module_file_name)
      }
    })
  })
}

fn detect_all_test_modules() -> List(String) {
  do_detect_all_test_modules()
}

fn get_cwd() -> String {
  do_get_cwd()
}

fn file_exists(absolute_file_name: String) -> Bool {
  do_file_exists(absolute_file_name)
}

fn run_suite(
  test_module_files: List(String),
  halts_on_error halts_on_error: Bool,
) -> Nil {
  do_run_suite(test_module_files, halts_on_error)
}

if erlang {
  import gleam/dynamic.{Dynamic}
  import gleam/int
  import gleam/result
  import gleam/string

  fn do_run_suite(
    test_module_files: List(String),
    halts_on_error halts_on_error: Bool,
  ) -> Nil {
    let options = [
      Verbose,
      NoTty,
      Report(#(Gleeunit2Progress, [Colored(True)])),
    ]

    let result =
      test_module_files
      |> list.map(fn(test_module_file: String) {
        assert Ok(#(_test_prefix, test_module_file)) =
          string.split_once(test_module_file, "test/")
        test_module_file
      })
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> result.unwrap(Error(dynamic.from(Nil)))

    let exit_code = case result {
      Ok(_) -> 0
      Error(_) -> 1
    }

    case halts_on_error, exit_code {
      True, exit_code -> halt(exit_code)
      False, 0 -> Nil
      False, 1 -> Nil
      False, unhandled_exit_code -> {
        "Unexpected Error Code: " <> int.to_string(unhandled_exit_code)
        // TODO: Gleam 0.26 io.print_errorln
        |> io.println
        Nil
      }
    }
  }

  external fn halt(Int) -> Nil =
    "erlang" "halt"

  fn gleam_to_erlang_module_name(path: String) -> String {
    path
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
  }

  fn do_detect_all_test_modules() -> List(String) {
    find_files(matching: "**/*.{erl,gleam}", in: "test")
    |> list.map(fn(test_module_file_name: String) {
      "test/" <> test_module_file_name
    })
  }

  external fn find_files(matching: String, in: String) -> List(String) =
    "gleeunit2_ffi" "find_files"

  external type Atom

  type Encoding {
    Utf8
  }

  external fn dangerously_convert_string_to_atom(String, Encoding) -> Atom =
    "erlang" "binary_to_atom"

  type ReportModuleName {
    Gleeunit2Progress
  }

  type Gleeunit2ProgressOption {
    Colored(Bool)
  }

  type EunitOption {
    Verbose
    NoTty
    Report(#(ReportModuleName, List(Gleeunit2ProgressOption)))
  }

  external fn run_eunit(List(Atom), List(EunitOption)) -> Dynamic =
    "eunit" "test"

  external fn do_file_exists(absolute_file_name: String) -> Bool =
    "filelib" "is_regular"

  external fn do_get_cwd() -> String =
    "glacier_ffi" "get_cwd_as_binary"
}

if javascript {
  fn do_run_suite(
    test_modules: List(String),
    halts_on_error halts_on_error: Bool,
  ) -> Nil {
    find_matching_test_module_files(test_modules)
    |> do_run_suite_ffi(halts_on_error)
  }

  external fn do_run_suite_ffi(
    test_modules: List(String),
    halts_on_error: Bool,
  ) -> Nil =
    "./gleeunit2_ffi.mjs" "main"

  fn do_detect_all_test_modules() -> List(String) {
    find_files(exts: [".gleam"], in: "test")
  }

  external fn find_files(exts: List(String), in: String) -> List(String) =
    "./gleeunit2_ffi.mjs" "find_files_recursive"

  external fn do_file_exists(absolute_file_name: String) -> Bool =
    "./gleeunit2_ffi.mjs" "file_exists"

  external fn do_get_cwd() -> String =
    "./gleeunit2_ffi.mjs" "cwd"
}
