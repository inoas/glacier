/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main(halts: Bool) -> Nil {
  do_main(halts)
}

if javascript {
  external fn do_main(halts: Bool) -> Nil =
    "./gleeunit_ffi.mjs" "main"
}

if erlang {
  import gleam/dynamic.{Dynamic}
  import gleam/erlang
  import gleam/int
  import gleam/io
  import gleam/list
  import gleam/string

  fn do_main(halts: Bool) -> Nil {
    let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

    let result =
      determine_test_modules()
      |> list.map(gleam_to_erlang_module_name)
      |> list.map(dangerously_convert_string_to_atom(_, Utf8))
      |> run_eunit(options)
      |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
      |> fn(result) {
        case result {
          Ok(v) -> v
          Error(_) -> Error(dynamic.from(Nil))
        }
      }

    let code = case result {
      Ok(_) -> 0
      Error(_) -> 1
    }

    case halts, code {
      True, code -> halt(code)
      False, 0 -> Nil
      False, code -> {
        int.to_string(code)
        |> io.println
        Nil
      }
    }
  }

  external fn halt(Int) -> Nil =
    "erlang" "halt"

  fn determine_test_modules() {
    io.debug(find_files(matching: "**/*.{erl,gleam}", in: "test"))
    let erlang_start_args = erlang.start_arguments()
    case erlang_start_args {
      [] -> find_files(matching: "**/*.{erl,gleam}", in: "test")
      erlang_start_args ->
        erlang_start_args
        |> list.map(fn(module_name) {
          let test_module_has_suffix =
            string.ends_with(module_name, ".gleam") || string.ends_with(
              module_name,
              ".erl",
            )
          case test_module_has_suffix {
            True -> module_name
            False -> module_name <> ".gleam"
          }
        })
        |> list.filter(fn(module_name) { file_exists("test/" <> module_name) })
        |> fn(module_names) {
          io.debug(#("Matching test modules: ", module_names))
          case module_names {
            [] -> find_files(matching: "**/*.{erl,gleam}", in: "test")
            _else -> module_names
          }
        }
    }
  }

  external fn file_exists(absolute_file_name: String) -> Bool =
    "filelib" "is_regular"

  fn gleam_to_erlang_module_name(path: String) -> String {
    // io.debug(path)
    path
    |> string.replace(".gleam", "")
    |> string.replace(".erl", "")
    |> string.replace("/", "@")
  }

  external fn find_files(matching: a, in: String) -> List(String) =
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
}
