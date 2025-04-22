import gleam/io
import gleam/list
import gleam/string
import gleam/string_tree
import gleam_community/ansi
import gleam_community/colour
import gleeunit

/// Lets Gleam switch code based on the current target.
///
type Target {
  ErlangTarget
  JavaScriptTarget
}

/// Atom to internally differentiate between `src` and `test` modules.
/// Public because the FFIs rely on it.
///
pub type ModuleKind {
  SrcModuleKind
  TestModuleKind
}

fn light_cyan_ansi_colour() {
  let assert Ok(colour) = colour.from_rgb255(r: 156, g: 231, b: 255)
  colour
}

/// Runs either `glacier` or `gleeunit` bundled as `gleeunit`, depending on
/// the given command line arguments.
///
pub fn main() {
  let start_args = start_args()
  let is_incremental = list.contains(start_args, "--glacier")
  let is_empty_args = start_args == []
  case is_empty_args, is_incremental {
    True, _ -> gleeunit.main()
    _, True -> {
      "🏔 Glacier is watching for changes…"
      |> ansi.colour(light_cyan_ansi_colour())
      |> ansi.italic()
      |> io.println
      start_file_change_watcher(fn(modules: List(#(ModuleKind, String))) -> Nil {
        execute_tests(modules)
      })
    }
    _, _ -> gleeunit.run(start_args, halts_on_error: False)
  }
}

/// Executes tests
///
fn execute_tests(modules: List(#(ModuleKind, String))) {
  let test_modules =
    list.fold(
      over: modules,
      from: [],
      with: fn(test_modules_acc: List(String), module: #(ModuleKind, String)) {
        let module_kind = module.0
        let full_module_path = module.1
        let test_modules = case module_kind {
          SrcModuleKind ->
            detect_distinct_import_module_dependency_chain(
              [file_name_to_module_name(full_module_path, SrcModuleKind)],
              [],
            )
            |> derive_test_modules_from_src_import_dependencies()
            |> list.map(fn(test_module) { "test/" <> test_module <> ".gleam" })
          TestModuleKind -> [to_relative_path(full_module_path)]
        }
        list.append(test_modules_acc, test_modules)
      },
    )
    |> list.unique()

  case test_modules {
    [] -> {
      "🏔 Did not detect any matching test modules!"
      |> ansi.colour(light_cyan_ansi_colour())
      |> ansi.bold()
      |> io.println
      Nil
    }
    test_modules -> {
      list.map(test_modules, with: fn(test_module: String) {
        "🏔 " <> test_module
      })
      |> string.join("\n")
      |> ansi.colour(light_cyan_ansi_colour())
      |> io.println

      let args = [
        "test",
        "--target",
        case target() {
          ErlangTarget -> "erlang"
          JavaScriptTarget -> "javascript"
        },
        "--",
        ..test_modules
      ]

      shell_exec_print(args)
    }
  }
}

/// Starts the file watcher.
///
fn start_file_change_watcher(
  file_change_handler: fn(List(#(ModuleKind, String))) -> Nil,
) -> Nil {
  do_start_file_change_watcher(file_change_handler)
  Nil
}

/// Detects distinct import module dependency chain.
///
fn detect_distinct_import_module_dependency_chain(
  module_names: List(String),
  processed_module_names: List(String),
) -> List(String) {
  case module_names {
    [] -> processed_module_names
    [module_name, ..rest_module_names] ->
      case list.contains(processed_module_names, module_name) {
        True ->
          detect_distinct_import_module_dependency_chain(
            rest_module_names,
            processed_module_names,
          )
        False -> {
          let unchecked_module_names =
            module_name
            |> module_name_to_file_name(SrcModuleKind)
            |> parse_module_for_imports
            |> list.append(module_names)
            |> list.filter(keeping: fn(module_name) {
              list.contains(processed_module_names, module_name) == False
              && file_exists(module_name_to_file_name(
                module_name,
                SrcModuleKind,
              ))
            })
          detect_distinct_import_module_dependency_chain(
            list.append(rest_module_names, unchecked_module_names),
            list.append(processed_module_names, [module_name]),
          )
        }
      }
  }
}

/// Parses a module file for its import statements.
///
fn parse_module_for_imports(module_file_name: String) -> List(String) {
  module_file_name
  |> read_module_file()
  |> fn(result: Result(String, Nil)) -> List(String) {
    case result {
      Ok(text) ->
        text
        |> string.to_graphemes()
        |> parse_module_string([], ParseModeSearch, "")
        |> list.unique()
      Error(Nil) -> []
    }
  }
}

type ParseMode {
  ParseModeInComment
  ParseModeInString
  ParseModeSearch
}

/// Parses a module string for its import statements
///
fn parse_module_string(
  chars: List(String),
  imports: List(String),
  context: ParseMode,
  collected: String,
) -> List(String) {
  case chars {
    [] -> imports
    [char, ..rest_chars] ->
      case context, collected, char {
        // Found `/`: Continue Initial with / in collected
        ParseModeSearch, "", "/" ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "/")
        // Found `/` + `/`: Enter Comment
        ParseModeSearch, "/", "/" ->
          parse_module_string(rest_chars, imports, ParseModeInComment, "")
        // Found `"`: Enter String
        ParseModeSearch, _collected, "\"" ->
          parse_module_string(rest_chars, imports, ParseModeInString, "")
        // Collecting import keyword: Continue Initial
        ParseModeSearch, collected, char
          if collected == ""
          && char == "i"
          || collected == "i"
          && char == "m"
          || collected == "im"
          && char == "p"
          || collected == "imp"
          && char == "o"
          || collected == "impo"
          && char == "r"
          || collected == "impor"
          && char == "t"
        ->
          parse_module_string(
            rest_chars,
            imports,
            ParseModeSearch,
            collected <> char,
          )
        // Found `import` + whitespaceish: Enter Import
        ParseModeSearch, "import", char
          if char == " " || char == "\t" || char == "\n" || char == "\r\n"
        -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_tree.new())
          let new_import = string_tree.to_string(new_import)
          let updated_imports = [new_import, ..imports]
          parse_module_string(rest_chars, updated_imports, ParseModeSearch, "")
        }
        // Found `import\r` + "\n": Enter Import
        ParseModeSearch, "import\r", "\n" -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_tree.new())
          let imports = [string_tree.to_string(new_import), ..imports]
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        }
        // Found whitespaceish char: Continue Initial with empty collected
        ParseModeSearch, _collected, _char ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        // In Comment; found `\n`: Exit Comment
        ParseModeInComment, _collected, "\n" ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        // In Comment; found `\n`: Exit Comment
        ParseModeInComment, _collected, "\r" ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        ParseModeInComment, _collected, "\r\n" ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        // In Comment; found any other char: Continue Comment
        ParseModeInComment, _collected, _any ->
          parse_module_string(rest_chars, imports, ParseModeInComment, "")
        // In String; escape found char: Continue String
        ParseModeInString, "\\", _escaped ->
          parse_module_string(rest_chars, imports, ParseModeInString, "")
        // In String; found `"`: Exit String
        ParseModeInString, _collected, "\"" ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        // In String; found a single `\`: Continue String with collected set to `\`
        ParseModeInString, _collected, "\\" ->
          parse_module_string(rest_chars, imports, ParseModeInString, "\\")
        // In String; found any other char: Continue String with empty collected
        ParseModeInString, _collected, _char ->
          parse_module_string(rest_chars, imports, ParseModeInString, "")
      }
  }
}

/// Parses an import statement
///
fn parse_import_chars(
  chars: List(String),
  import_module: string_tree.StringTree,
) {
  // TODO: Try pop grapheme
  case chars {
    // Return if end of line
    [] -> #([], import_module)
    // Return if . - aka found unqualified import
    [".", ..rest_chars] -> #(rest_chars, import_module)
    // Whitespaces stop inmports and return
    [" ", ..rest_chars] -> #(rest_chars, import_module)
    // Return if \r\n
    ["\r\n", ..rest_chars] -> #(rest_chars, import_module)
    // Return if \n
    ["\n", ..rest_chars] -> #(rest_chars, import_module)
    // Ignore whitespaces
    [char, ..rest_chars]
      if char == "\t" || char == "\r" || char == "\n" || char == "\r\n"
    -> parse_import_chars(rest_chars, import_module)
    // Append for any other character
    [char, ..rest_chars] ->
      parse_import_chars(rest_chars, string_tree.append(import_module, char))
  }
}

/// Derives test modules from its src import dependencies
///
fn derive_test_modules_from_src_import_dependencies(
  src_modules: List(String),
) -> List(String) {
  let project_test_files = find_project_files(in: "test")
  let all_test_modules =
    project_test_files
    |> list.map(fn(module_name_dot_gleam) {
      let assert Ok(#(module_name, _dot_gleam)) =
        string.split_once(module_name_dot_gleam, ".gleam")
      module_name
    })
  let dirty_test_modules =
    all_test_modules
    |> list.filter(fn(test_module) {
      let test_module_imports = derive_src_imports_off_test_module(test_module)
      list.any(in: src_modules, satisfying: fn(src_module) {
        test_module_imports
        |> list.contains(src_module)
      })
    })
  dirty_test_modules
}

/// Derives src imports from test module
///
fn derive_src_imports_off_test_module(test_module_name) {
  test_module_name
  |> module_name_to_file_name(TestModuleKind)
  |> parse_module_for_imports
}

/// Converts a module name to a module file name
///
fn module_name_to_file_name(
  module_name: String,
  module_kind: ModuleKind,
) -> String {
  case module_kind {
    SrcModuleKind -> get_src_dir() <> "/" <> module_name <> ".gleam"
    TestModuleKind -> get_test_dir() <> "/" <> module_name <> ".gleam"
  }
}

/// Converts a module file name to a module name
///
fn file_name_to_module_name(module_name: String, module_kind: ModuleKind) {
  let assert Ok(#(_base_path, module_name_dot_gleam)) = case module_kind {
    SrcModuleKind -> string.split_once(module_name, get_src_dir() <> "/")
    TestModuleKind -> string.split_once(module_name, get_test_dir() <> "/")
  }
  case string.ends_with(module_name, ".erl") {
    True -> {
      let assert Ok(#(module_name, _dot_gleam)) =
        string.split_once(module_name_dot_gleam, ".erl")
      module_name
    }
    False -> {
      let assert Ok(#(module_name, _dot_gleam)) =
        string.split_once(module_name_dot_gleam, ".gleam")
      module_name
    }
  }
}

/// Checks if a given absolute file path exists
///
fn file_exists(absolute_file_name: String) -> Bool {
  do_file_exists(absolute_file_name)
}

/// Finds files in project sub directory
///
fn find_project_files(in sub_directory: String) -> List(String) {
  do_find_project_files(sub_directory)
}

/// Gets the target at runtime
///
fn target() -> Target {
  do_target()
}

/// Gets the start arguments sometimes called argv.
///
fn start_args() -> List(String) {
  do_start_args()
}

/// Gets the current project directory.
///
fn get_cwd() -> String {
  do_get_cwd()
}

/// Gets the project's `src` directory.
///
fn get_src_dir() -> String {
  get_cwd() <> "/src"
}

/// Gets the project's `test` directory.
///
fn get_test_dir() -> String {
  get_cwd() <> "/test"
}

/// Cuts off the base path from the project directory.
///
fn to_relative_path(absolute_file_path path: String) -> String {
  let assert Ok(#(_pre_path, relative_file_name)) =
    string.split_once(path, get_cwd() <> "/")
  relative_file_name
}

@target(erlang)
import shellout
@target(erlang)
import simplifile as file

@target(erlang)
fn do_target() -> Target {
  ErlangTarget
}

@target(erlang)
import argv

@target(erlang)
fn do_start_args() -> List(String) {
  argv.load().arguments
}

@external(erlang, "glacier_ffi", "start_file_change_watcher")
@external(javascript, "./glacier_ffi.mjs", "start_file_change_watcher")
fn do_start_file_change_watcher(
  file_change_handler file_change_handler: fn(List(#(ModuleKind, String))) ->
    Nil,
) -> Nil

@target(erlang)
fn read_module_file(module_path: String) -> Result(String, Nil) {
  case file.read(module_path) {
    Ok(text) -> Ok(text)
    Error(file_reason) -> {
      io.debug(#("Could not read file", module_path, "with reason", file_reason))
      Error(Nil)
    }
  }
}

@external(erlang, "glacier_ffi", "get_cwd_as_binary")
@external(javascript, "./glacier_ffi.mjs", "cwd")
fn do_get_cwd() -> String

@external(erlang, "filelib", "is_regular")
@external(javascript, "./glacier_ffi.mjs", "file_exists")
fn do_file_exists(absolute_file_name absolute_file_name: String) -> Bool

@target(erlang)
fn do_find_project_files(in: String) -> List(String) {
  do_find_files_recursive(in: in, matching: "**/*.{gleam}")
}

@target(erlang)
@external(erlang, "glacier_ffi", "find_files_recursive")
fn do_find_files_recursive(
  in in: String,
  matching matching: String,
) -> List(String)

@target(erlang)
fn shell_exec_print(args: List(String)) -> Nil {
  case
    shellout.command(run: "gleam", with: args, in: ".", opt: [
      shellout.LetBeStderr,
    ])
  {
    Ok(msg) -> {
      io.print(msg)
      Nil
    }
    Error(_error_tuple) -> Nil
  }
}

@target(javascript)
fn do_target() -> Target {
  JavaScriptTarget
}

@target(javascript)
fn do_start_args() -> List(String) {
  start_args_ffi()
  // // This is a work-around for a bug introduced in Gleam 0.26.0:
  // |> list.filter(fn(arg) {
  //   arg != "--" && string.ends_with(arg, "/gleam.main.mjs") == False
  // })
}

@target(javascript)
@external(javascript, "./glacier_ffi.mjs", "start_args")
fn start_args_ffi() -> List(String)

@target(javascript)
fn read_module_file(module_path: String) -> Result(String, Nil) {
  do_read_module_file(module_path)
}

@target(javascript)
@external(javascript, "./glacier_ffi.mjs", "read_file")
fn do_read_module_file(module_path module_path: String) -> Result(String, Nil)

@target(javascript)
fn do_find_project_files(dir: String) -> List(String) {
  do_find_files_recursive(dir, [".gleam"])
  |> list.map(fn(file_name) {
    let assert Ok(#(_test_prefix, file_name)) =
      string.split_once(file_name, "test/")
    file_name
  })
}

@target(javascript)
@external(javascript, "./glacier_ffi.mjs", "find_files_recursive_by_exts")
fn do_find_files_recursive(
  in in: String,
  file_ext file_ext: List(String),
) -> List(String)

@target(javascript)
@external(javascript, "./glacier_ffi.mjs", "shell_exec_print")
fn shell_exec_print(args args: List(String)) -> Nil
