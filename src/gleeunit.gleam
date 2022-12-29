import gleam/io
import gleam/list
import gleam/string
import gleam/function

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
  detect_all_test_modules()
  |> run_suite(halts_on_error: True)
}

/// Runs a specific list of test modules
///
pub fn run(for test_modules: List(String)) -> Nil {
  test_modules
  |> find_matching_test_module_files
  |> run_suite(halts_on_error: False)
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
    let absolute_module_file = case target() {
      ErlangTarget -> get_cwd() <> "/test/" <> module_name
      JavaScriptTarget -> get_cwd() <> "/" <> module_name
    }
    file_exists(absolute_module_file)
    |> function.tap(fn(module_file_exists: Bool) {
      case module_file_exists {
        True -> Nil
        // TODO: gleam 0.26 io.print_error
        False -> io.print("Error: Could not find " <> absolute_module_file)
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

fn target() -> Target {
  do_target()
}

fn file_exists(absolute_file_name: String) -> Bool {
  do_file_exists(absolute_file_name)
}

fn run_suite(
  test_modules: List(String),
  halts_on_error halts_on_error: Bool,
) -> Nil {
  do_run_suite(test_modules, halts_on_error)
}

if erlang {
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
  import gleam/list
=======
  import gleam/int
>>>>>>> 2c161a5 (js wip)
  import gleam/result
  import gleam/string
=======
  import glacier/glacier_helpers
>>>>>>> 52d5260 (integrate gleeunit)
=======
  import gleam/list
  import gleam/string
  import gleam/int
  import gleam/io
>>>>>>> 851cdb8 (accept stdlib as dep)
=======
  import gleam/result
>>>>>>> 132c225 (cleanup)
  import gleam/dynamic.{Dynamic}
=======
  import gleam/dynamic.{Dynamic}
  import gleam/erlang
  import gleam/int
  import gleam/io
=======
>>>>>>> c36ab77 (polish)
  import gleam/list
  import gleam/result
  import gleam/string
<<<<<<< HEAD
>>>>>>> 0c23b2b (run specific test modules only)
=======
  import gleam/dynamic.{Dynamic}
>>>>>>> c36ab77 (polish)

  fn do_run_suite(
    test_modules: List(String),
    halts_on_error halts_on_error: Bool,
  ) -> Nil {
    let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

    let result =
<<<<<<< HEAD
<<<<<<< HEAD
      find_files(matching: "**/*.{erl,gleam}", in: "test")
<<<<<<< HEAD
<<<<<<< HEAD
=======
      determine_test_modules()
>>>>>>> 0c23b2b (run specific test modules only)
=======
      test_modules
>>>>>>> 493c0ca (first buggy alpha version for erlang runs :yippie:)
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> result.unwrap(Error(dynamic.from(Nil)))
<<<<<<< HEAD
=======
      |> glacier_helpers.list_map(gleam_to_erlang_module_name)
      |> glacier_helpers.list_map(dangerously_convert_string_to_atom(_, Utf8))
=======
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
>>>>>>> 851cdb8 (accept stdlib as dep)
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> fn(result) {
        case result {
          Ok(v) -> v
          Error(_) -> Error(dynamic.from(Nil))
        }
      }
>>>>>>> 52d5260 (integrate gleeunit)
=======
>>>>>>> c36ab77 (polish)

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
        |> io.println
        Nil
      }
    }
  }

  external fn halt(Int) -> Nil =
    "erlang" "halt"

  fn gleam_to_erlang_module_name(path: String) -> String {
    path
<<<<<<< HEAD
<<<<<<< HEAD
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
  }

  fn do_detect_all_test_modules() -> List(String) {
    find_files(matching: "**/*.{erl,gleam}", in: "test")
  }

  external fn find_files(matching: String, in: String) -> List(String) =
<<<<<<< HEAD
=======
    |> glacier_helpers.string_replace(".gleam", "")
    |> glacier_helpers.string_replace(".erl", "")
    |> glacier_helpers.string_replace("/", "@")
=======
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
>>>>>>> 851cdb8 (accept stdlib as dep)
  }

  external fn find_files(matching: a, in: String) -> List(String) =
>>>>>>> 52d5260 (integrate gleeunit)
=======
>>>>>>> c36ab77 (polish)
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

  external fn do_file_exists(absolute_file_name: String) -> Bool =
    "filelib" "is_regular"

  external fn do_get_cwd() -> String =
    "glacier_ffi" "get_cwd_as_binary"

  fn do_target() -> Target {
    ErlangTarget
  }
}

if javascript {
  fn do_run_suite(
    test_modules: List(String),
    halts_on_error halts_on_error: Bool,
  ) -> Nil {
    let test_modules = find_matching_test_module_files(test_modules)
    // io.debug(test_modules)
    do_run_suite_ffi(test_modules, halts_on_error)
  }

  external fn do_run_suite_ffi(
    test_modules: List(String),
    halts_on_error: Bool,
  ) -> Nil =
    "./gleeunit_ffi.mjs" "main"

  fn do_detect_all_test_modules() -> List(String) {
    find_files(exts: [".gleam"], in: "test")
  }

  external fn find_files(exts: List(String), in: String) -> List(String) =
    "./gleeunit_ffi.mjs" "find_files_recursive"

  external fn do_file_exists(absolute_file_name: String) -> Bool =
    "./gleeunit_ffi.mjs" "file_exists"

  external fn do_get_cwd() -> String =
    "./gleeunit_ffi.mjs" "cwd"

  fn do_target() -> Target {
    JavaScriptTarget
  }
}
