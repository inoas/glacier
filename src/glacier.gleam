import gleam/io
import gleam/list
import gleam/string
import gleam/string_builder
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

3. In your app run: gleam test --target erlang -- --only-recently-saved=3
   or run: gleam test --target javascript -- --only-recently-saved=3
",
  )
}

type ModuleKind {
  SrcModuleKind
  TestModuleKind
}

pub fn run() {
  io.println("Starting Glacier watcher…")
  file_change_watcher(fn(module_kind: ModuleKind, full_module_path: String) {
    let _test_modules = case module_kind {
      SrcModuleKind ->
        detect_unique_import_module_dependencies(full_module_path)
        |> derive_test_modules_off_import_module_dependencies()
      TestModuleKind -> [full_module_path]
      unexpected_atom -> {
        io.debug(#("UNEXPECTED ATOM", unexpected_atom, "FOR", full_module_path))
        []
      }
    }
    // io.debug(test_modules)
    // TODO: pass _test_modules to `gleam test`
    gleeunit.main(False)
    Nil
  })
}

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

fn detect_unique_import_module_dependencies(module_path: String) -> List(String) {
  read_module_file(module_path)
  |> string.to_graphemes()
  |> parse_module([], ParseModeInitial, "")
  |> list.unique()
}

type ParseMode {
  ParseModeInitial
  ParseModeComment
  ParseModeString
}

fn parse_module(
  chars: List(String),
  imports: List(String),
  context: ParseMode,
  collected: String,
) -> List(String) {
  case chars {
    [] -> imports
    [char, ..rest_chars] -> {
      io.debug(#(context, collected, char))
      case context, collected, char {
        // Found `/` + `/`: Enter Comment
        ParseModeInitial, "/", "/" ->
          parse_module(rest_chars, imports, ParseModeComment, "")
        // Found `"`: Enter String
        ParseModeInitial, _collected, "\"" ->
          parse_module(rest_chars, imports, ParseModeString, "")
        // Found `import` + whitespaceish: Enter Import
        ParseModeInitial, "import", char if char == " " || char == "\t" || char == "\r" || char == "\n" -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_builder.new())
          let imports = [string_builder.to_string(new_import), ..imports]
          parse_module(rest_chars, imports, ParseModeInitial, "")
        }
        // Found `import\r` + "\n": Enter Import
        ParseModeInitial, "import\r", char if char == "\n" -> {
          let #(rest_chars, new_import) =
            parse_import_chars(rest_chars, string_builder.new())
          let imports = [string_builder.to_string(new_import), ..imports]
          parse_module(rest_chars, imports, ParseModeInitial, "")
        }
        // Found whitespaceish char: Continue Initial with out adding to collected
        ParseModeInitial, collected, char if char == " " || char == "\t" || char == "\r" || char == "\n" ->
          parse_module(rest_chars, imports, ParseModeInitial, collected)
        // Found other char: Continue Initial
        ParseModeInitial, collected, char ->
          parse_module(rest_chars, imports, ParseModeInitial, collected <> char)
        // In Comment; found `\n`: Exit Comment
        ParseModeComment, _collected, "\n" ->
          parse_module(rest_chars, imports, ParseModeInitial, "")
        // In Comment; found any other char: Continue Comment
        ParseModeComment, _collected, _any ->
          parse_module(rest_chars, imports, ParseModeComment, "")
        // In String; found `"`: Exit String
        ParseModeString, _collected, "\"" ->
          parse_module(rest_chars, imports, ParseModeInitial, "")
        // In String; found `\` + `\`: Continue String with empty collected
        ParseModeString, "\\", "\\" ->
          parse_module(rest_chars, imports, ParseModeString, "")
        // In String; found a single `\`: Continue String with collected set to `\`
        ParseModeString, _collected, "\\" ->
          parse_module(rest_chars, imports, ParseModeString, "\\")
        // In String; found any other char: Continue String with empty collected
        ParseModeString, _collected, _char ->
          parse_module(rest_chars, imports, ParseModeString, "")
      }
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
  module_paths: List(String),
) -> List(String) {
  io.debug(module_paths)
  []
}

if erlang {
  import gleam/erlang/file

  fn read_module_file(module_path: String) -> String {
    assert Ok(contents) = file.read(module_path)
    contents
  }
}

if javascript {
  fn read_module_file(module_path: String) -> String {
    todo
    ""
  }
}
