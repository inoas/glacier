import gleam/function
import gleam/io
import gleam/list
import gleam/map
import gleam/string
import gleam/string_builder
import gleeunit
import shellout

type Target {
  ErlangTarget
  JavaScriptTarget
}

type ModuleKind {
  SrcModuleKind
  TestModuleKind
}

pub const shellout_lookups: shellout.Lookups = [
  #(
    ["color", "background"],
    [#("pink", ["255", "175", "243"]), #("lightblue", ["156", "231", "255"])],
  ),
]

pub fn main() {
  "Glacier · Gleam Incremental Interactive Unit Testing"
  |> shellout.style(
    with: shellout.display(["bold"])
    |> map.merge(shellout.color(["lightblue"])),
    custom: shellout_lookups,
  )
  |> io.println

  "\nUsage:

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
3. Run: gleam test --target erlang -- --glacier
   Or run: gleam test --target javascript -- --glacier"
  |> shellout.style(
    with: shellout.color(["lightblue"]),
    custom: shellout_lookups,
  )
  |> io.println
}

pub fn run() {
  let start_args = get_start_args()
  let is_incremental = list.contains(start_args, "--glacier")
  let is_empty_args = start_args == []
  case is_empty_args, is_incremental {
    True, _ -> gleeunit.main()
    _, True -> {
      "🏔 Glacier is watching for changes..."
      |> shellout.style(
        with: shellout.display(["italic"])
        |> map.merge(shellout.color(["lightblue"])),
        custom: shellout_lookups,
      )
      |> io.println
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
        case test_modules {
          [] -> {
            "🏔 Did not detect any matching test modules!"
            |> shellout.style(
              with: shellout.display(["bold"])
              |> map.merge(shellout.color(["lightblue"])),
              custom: shellout_lookups,
            )
            |> io.println
            Nil
          }
          test_modules -> {
            "🏔 Detected test modules:"
            |> shellout.style(
              with: shellout.display(["bold"])
              |> map.merge(shellout.color(["lightblue"])),
              custom: shellout_lookups,
            )
            |> io.println
            list.map(
              test_modules,
              with: fn(test_module: String) { "  ❄ " <> test_module },
            )
            |> string.join("\n")
            |> shellout.style(
              with: shellout.color(["lightblue"]),
              custom: shellout_lookups,
            )
            |> io.println
            let cmd = case target() {
              ErlangTarget -> "gleam test --target erlang -- "
              JavaScriptTarget -> "gleam test --target javascript -- "
            }
            // io.debug(cmd)
            let cmd = cmd <> string.join(test_modules, " ")
            cmd
            |> shell_exec
            |> function.tap(fn(shell_exec_return) {
              let #(_exit_code, message) = shell_exec_return
              message
              |> io.print()
            })
            Nil
          }
        }
      })
    }
    _, _ -> gleeunit.run(for: start_args)
  }
  // io.debug(test_modules)
}

fn shell_exec(cmd: String) -> #(Int, String) {
  do_shell_exec(cmd)
}

fn file_change_watcher(
  file_change_handler: fn(ModuleKind, String) -> Nil,
) -> Nil {
  do_file_change_watcher(file_change_handler)
  Nil
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
              list.contains(processed_module_names, module_name) == False && file_exists(module_name_to_file_name(
                module_name,
                SrcModuleKind,
              ))
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
  |> parse_module_string([], ParseModeSearch, "")
  |> list.unique()
}

type ParseMode {
  ParseModeSearch
  ParseModeInComment
  ParseModeInString
}

fn parse_module_string(
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
          parse_module_string(rest_chars, imports, ParseModeSearch, "/")
        // Found `/` + `/`: Enter Comment
        ParseModeSearch, "/", "/" ->
          parse_module_string(rest_chars, imports, ParseModeInComment, "")
        // Found `"`: Enter String
        ParseModeSearch, _collected, "\"" ->
          parse_module_string(rest_chars, imports, ParseModeInString, "")
        // Collecting import keyword: Continue Initial
        ParseModeSearch, collected, char if collected == "" && char == "i" || collected == "i" && char == "m" || collected == "im" && char == "p" || collected == "imp" && char == "o" || collected == "impo" && char == "r" || collected == "impor" && char == "t" ->
          parse_module_string(
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
          parse_module_string(rest_chars, new_imports, ParseModeSearch, "")
        }
        // Found `import\r` + "\n": Enter Import
        ParseModeSearch, "import\r", "\n" -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_builder.new())
          let imports = [string_builder.to_string(new_import), ..imports]
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        }
        // Found whitespaceish char: Continue Initial with empty collected
        ParseModeSearch, _collected, _char ->
          parse_module_string(rest_chars, imports, ParseModeSearch, "")
        // In Comment; found `\n`: Exit Comment
        ParseModeInComment, _collected, "\n" ->
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

  case string.ends_with(module_name, ".erl") {
    True -> {
      assert Ok(#(module_name, _dot_gleam)) =
        string.split_once(module_name_dot_gleam, ".erl")
      module_name
    }
    False -> {
      assert Ok(#(module_name, _dot_gleam)) =
        string.split_once(module_name_dot_gleam, ".gleam")
      module_name
    }
  }
}

fn file_exists(absolute_file_name: String) -> Bool {
  do_file_exists(absolute_file_name)
}

fn find_project_files(matching matching: String, in in: String) -> List(String) {
  do_find_project_files(matching, in)
}

if erlang {
  import gleam/erlang
  import gleam/erlang/file

  fn target() -> Target {
    ErlangTarget
  }

  fn get_start_args() -> List(String) {
    erlang.start_arguments()
  }

  external fn do_file_change_watcher(
    file_change_handler: fn(ModuleKind, String) -> Nil,
  ) -> Nil =
    "glacier_ffi" "start_file_change_watcher"

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

  external fn do_shell_exec(cmd: String) -> #(Int, String) =
    "glacier_ffi" "shell_exec"
}

if javascript {
  fn target() -> Target {
    JavaScriptTarget
  }

  fn get_start_args() -> List(String) {
    todo
  }

  fn do_file_change_watcher(
    file_change_handler: fn(ModuleKind, String) -> Nil,
  ) -> Nil {
    todo
  }

  fn read_module_file(module_path: String) -> String {
    // io.debug("Reading module file " <> module_path)
    todo
  }

  fn get_cwd() -> String {
    todo
  }

  fn do_file_exists(absolute_file_name: String) -> Bool {
    todo
  }

  fn do_find_project_files(matching: String, in: String) -> List(String) {
    todo
  }

  fn do_shell_exec(cmd: String) -> #(Int, String) {
    todo
  }
}
