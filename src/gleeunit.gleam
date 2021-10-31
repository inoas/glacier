import gleam/list
import gleam/string
import gleam/dynamic.{Dynamic}

pub fn discover_and_run_tests() {
  find_files(matching: "**/*.{erl,gleam}", in: "test")
  |> list.map(remove_extension)
  |> list.map(dangerously_convert_string_to_atom)
  |> run_eunit([Verbose])
}

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
}

// NoTty
external fn run_eunit(List(Atom), List(EunitOption)) -> Dynamic =
  "eunit" "test"
