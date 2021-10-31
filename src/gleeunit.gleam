import gleam/list
import gleam/result
import gleam/string
import gleam/dynamic.{Dynamic}

/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
pub fn main() -> Nil {
  let options = [
    Verbose,
    NoTty,
    Report(#(
      dangerously_convert_string_to_atom("gleeunit_progress"),
      [dynamic.from(#(dangerously_convert_string_to_atom("colored"), True))],
    )),
  ]

  let result =
    find_files(matching: "**/*.{erl,gleam}", in: "test")
    |> list.map(remove_extension)
    |> list.map(dangerously_convert_string_to_atom)
    |> run_eunit(options)
    |> dynamic.result
    |> result.unwrap(Error(dynamic.from(Nil)))

  let code = case result {
    Ok(_) -> 0
    Error(_) -> 1
  }
  halt(code)
}

external fn halt(Int) -> Nil =
  "erlang" "halt"

fn remove_extension(path: String) -> String {
  path
  |> string.replace(".gleam", "")
  |> string.replace(".erl", "")
  |> string.replace("/", "@")
}

external fn find_files(matching: String, in: String) -> List(String) =
  "gleeunit_ffi" "find_files"

external type Atom

external fn dangerously_convert_string_to_atom(String) -> Atom =
  "erlang" "binary_to_atom"

type EunitOption {
  Verbose
  NoTty
  Report(#(Atom, List(Dynamic)))
}

// NoTty
external fn run_eunit(List(Atom), List(EunitOption)) -> Dynamic =
  "eunit" "test"
