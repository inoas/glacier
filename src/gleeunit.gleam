/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  find_files(matching: "**/*.{erl,gleam}", in: "test")
  |> do_main()
}

pub fn run(for modules_list: List(String)) -> Nil {
  modules_list
  |> find_matching_test_modules
  |> do_main()
}

if javascript {
  external fn do_main() -> Nil =
    "./gleeunit_ffi.mjs" "main"
}

if erlang {
  import gleam/list
  import gleam/result
  import gleam/string
  import gleam/dynamic.{Dynamic}

  fn do_main(test_modules: List(String)) -> Nil {
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

  fn find_matching_test_modules(test_modules) {
    test_modules
    |> list.map(fn(module_name) {
      let test_module_has_prefix = string.ends_with(module_name, "test/")
      let test_module_has_suffix =
        string.ends_with(module_name, ".gleam") || string.ends_with(
          module_name,
          ".erl",
        )
      case test_module_has_prefix, test_module_has_suffix {
        True, True -> module_name
        True, False -> module_name <> ".gleam"
        False, True -> "test/" <> module_name
        False, False -> "test/" <> module_name <> ".gleam"
      }
    })
    |> list.filter(fn(module_name) {
      // io.debug(module_name)
      file_exists(module_name)
    })
  }
}
