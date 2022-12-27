import gleam/erlang
import gleam/io
import gleam/list
import gleam/string
import gleam/string_builder
import gleam/function
import gleeunit

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

3. In your app run: gleam test --target erlang
   or run: gleam test --target javascript
",
  )
}

type ModuleKind {
  SrcModuleKind
  TestModuleKind
}

pub fn run() {
  let erlang_start_args = erlang.start_arguments()
  case erlang_start_args {
    [] -> {
      io.println("Starting Glacier watcher…")
      file_change_watcher(fn(module_kind: ModuleKind, full_module_path: String) {
        let test_modules = case module_kind {
          SrcModuleKind ->
            detect_unique_import_module_dependencies(
              [file_name_to_module_name(full_module_path, SrcModuleKind)],
              [],
            )
            |> derive_test_modules_off_import_module_dependencies()
          TestModuleKind -> [
            file_name_to_module_name(full_module_path, TestModuleKind),
          ]
          unexpected_atom -> {
            io.debug(#(
              "UNEXPECTED ATOM",
              unexpected_atom,
              "FOR",
              full_module_path,
            ))
            []
          }
        }
        "gleam test -- " <> string.join(test_modules, " ")
        |> io.debug
        |> shell_exec
        |> function.tap(fn(shell_exec_return) {
          let #(_exit_code, message) = shell_exec_return
          io.println(message)
        })
        Nil
      })
    }
    erlang_start_args -> {
      io.debug(#("Running tests", erlang_start_args))
      gleeunit.run_test_modules(erlang_start_args, True)
    }
  }
  // io.debug(test_modules)
}

external fn shell_exec(cmd: String) -> #(Int, String) =
  "glacier_ffi" "shell_exec"

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

fn detect_unique_import_module_dependencies(
  module_names: List(String),
  processed_module_names: List(String),
) -> List(String) {
  case module_names {
    [] -> processed_module_names
    [module_name, ..rest_module_names] ->
      case list.contains(processed_module_names, module_name) {
        True ->
          detect_unique_import_module_dependencies(
            rest_module_names,
            processed_module_names,
          )
        False -> {
          let unchecked_module_names =
            module_name
            |> module_name_to_file_name(SrcModuleKind)
            |> parse_module_for_imports
            |> list.append(module_names)
            |> list.filter(for: fn(module_name) {
              processed_module_names
              |> list.contains(module_name) == False && module_name_to_file_name(
                module_name,
                SrcModuleKind,
              )
              |> file_exists
            })
          detect_unique_import_module_dependencies(
            list.append(rest_module_names, unchecked_module_names),
            list.append(processed_module_names, [module_name]),
          )
        }
      }
  }
}

fn parse_module_for_imports(module_file_name: String) -> List(String) {
  module_file_name
  |> read_module_file()
  |> string.to_graphemes()
  |> do_parse_module([], ParseModeSearch, "")
  |> list.unique()
}

type ParseMode {
  ParseModeSearch
  ParseModeInComment
  ParseModeInString
}

fn do_parse_module(
  chars: List(String),
  imports: List(String),
  context: ParseMode,
  collected: String,
) -> List(String) {
  case chars {
    [] -> imports
    [char, ..rest_chars] ->
      // io.debug(#(context, collected, char))
      case context, collected, char {
        // Found `/`: Continue Initial with / in collected
        ParseModeSearch, "", "/" ->
          do_parse_module(rest_chars, imports, ParseModeSearch, "/")
        // Found `/` + `/`: Enter Comment
        ParseModeSearch, "/", "/" ->
          do_parse_module(rest_chars, imports, ParseModeInComment, "")
        // Found `"`: Enter String
        ParseModeSearch, _collected, "\"" ->
          do_parse_module(rest_chars, imports, ParseModeInString, "")
        // Collecting import keyword: Continue Initial
        ParseModeSearch, collected, char if collected == "" && char == "i" || collected == "i" && char == "m" || collected == "im" && char == "p" || collected == "imp" && char == "o" || collected == "impo" && char == "r" || collected == "impor" && char == "t" ->
          do_parse_module(
            rest_chars,
            imports,
            ParseModeSearch,
            collected <> char,
          )
        // Found `import` + whitespaceish: Enter Import
        ParseModeSearch, "import", char if char == " " || char == "\t" || char == "\r" || char == "\n" -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_builder.new())
          let new_imports = [string_builder.to_string(new_import), ..imports]
          do_parse_module(rest_chars, new_imports, ParseModeSearch, "")
        }
        // Found `import\r` + "\n": Enter Import
        ParseModeSearch, "import\r", "\n" -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_builder.new())
          let imports = [string_builder.to_string(new_import), ..imports]
          do_parse_module(rest_chars, imports, ParseModeSearch, "")
        }
        // Found whitespaceish char: Continue Initial with empty collected
        ParseModeSearch, _collected, _char ->
          do_parse_module(rest_chars, imports, ParseModeSearch, "")
        // In Comment; found `\n`: Exit Comment
        ParseModeInComment, _collected, "\n" ->
          do_parse_module(rest_chars, imports, ParseModeSearch, "")
        // In Comment; found any other char: Continue Comment
        ParseModeInComment, _collected, _any ->
          do_parse_module(rest_chars, imports, ParseModeInComment, "")
        // In String; escape found char: Continue String
        ParseModeInString, "\\", _escaped ->
          do_parse_module(rest_chars, imports, ParseModeInString, "")
        // In String; found `"`: Exit String
        ParseModeInString, _collected, "\"" ->
          do_parse_module(rest_chars, imports, ParseModeSearch, "")
        // In String; found a single `\`: Continue String with collected set to `\`
        ParseModeInString, _collected, "\\" ->
          do_parse_module(rest_chars, imports, ParseModeInString, "\\")
        // In String; found any other char: Continue String with empty collected
        ParseModeInString, _collected, _char ->
          do_parse_module(rest_chars, imports, ParseModeInString, "")
      }
  }
}

fn parse_import_chars(
  chars: List(String),
  import_module: string_builder.StringBuilder,
) {
  case chars {
    // Return if end of line
    [] -> #([], import_module)
    // Return if \n
    ["\n", ..rest_chars] -> #(rest_chars, import_module)
    // Ignore whitespaces
    [char, ..rest_chars] if char == " " || char == "\t" || char == "\r" ->
      parse_import_chars(rest_chars, import_module)
    // Append for any other character
    [char, ..rest_chars] ->
      parse_import_chars(rest_chars, string_builder.append(import_module, char))
  }
}

fn derive_test_modules_off_import_module_dependencies(
  src_modules: List(String),
) -> List(String) {
  // io.debug(modules)
  let all_test_modules =
    find_project_files(matching: "**/*.{gleam}", in: "test")
    |> list.map(fn(module_name_dot_gleam) {
      assert Ok(#(module_name, _dot_gleam)) =
        string.split_once(module_name_dot_gleam, ".gleam")
      module_name
    })

  // FIXME: Should be using a set instead of lists
  let dirty_test_modules =
    all_test_modules
    |> list.filter(fn(test_module) {
      let test_module_imports = derive_src_imports_off_test_module(test_module)
      list.any(
        in: src_modules,
        satisfying: fn(src_module) {
          test_module_imports
          |> list.contains(src_module)
        },
      )
    })

  // io.debug(#("dirty_test_modules", dirty_test_modules))
  dirty_test_modules
}

fn derive_src_imports_off_test_module(test_module_name) {
  // TODO: build sha1 and only re-derive imports if sha1 differs, to improve speed
  test_module_name
  |> module_name_to_file_name(TestModuleKind)
  |> parse_module_for_imports
}

fn module_name_to_file_name(
  module_name: String,
  module_kind: ModuleKind,
) -> String {
  case module_kind {
    SrcModuleKind -> get_cwd() <> "/src/" <> module_name <> ".gleam"
    TestModuleKind -> get_cwd() <> "/test/" <> module_name <> ".gleam"
  }
}

fn file_name_to_module_name(module_name: String, module_kind: ModuleKind) {
  assert Ok(#(_base_path, module_name_dot_gleam)) = case module_kind {
    SrcModuleKind -> string.split_once(module_name, get_cwd() <> "/src/")
    TestModuleKind -> string.split_once(module_name, get_cwd() <> "/test/")
  }

  assert Ok(#(module_name, _dot_gleam)) =
    string.split_once(module_name_dot_gleam, ".gleam")

  module_name
}

fn file_exists(absolute_file_name: String) -> Bool {
  do_file_exists(absolute_file_name)
}

fn find_project_files(matching matching: String, in in: String) -> List(String) {
  do_find_project_files(matching, in)
}

if erlang {
  import gleam/erlang/file

  fn read_module_file(module_path: String) -> String {
    // io.debug("Reading module file " <> module_path)
    assert Ok(contents) = file.read(module_path)
    contents
  }

  external fn get_cwd() -> String =
    "glacier_ffi" "get_cwd_as_binary"

  external fn do_file_exists(absolute_file_name: String) -> Bool =
    "filelib" "is_regular"

  external fn do_find_project_files(
    matching: String,
    in: String,
  ) -> List(String) =
    "glacier_ffi" "find_project_files"
}

if javascript {

}
