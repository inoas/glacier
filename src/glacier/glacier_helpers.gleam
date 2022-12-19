/// Replaces stdlib's io.println
/// so that there are no deps on Glacier.
///
pub fn io_println(text: String) {
  do_io_println(text)
}

if erlang {
  external fn do_io_println(text: String) -> Nil =
    "glacier_ffi" "stdout_println"
}

if javascript {
  external fn do_io_println(text: String) -> Nil =
    "../glacier_ffi.mjs" "stdout_println"
}

/// Replaces stdlib's list.map
/// so that there are no deps on Glacier.
///
pub fn list_map(list: List(a), with fun: fn(a) -> b) -> List(b) {
  do_list_map(list, fun)
}

if erlang {
  external fn do_list_map(list: List(a), callable: fn(a) -> b) -> List(b) =
    "glacier_ffi" "list_map"
}

if javascript {
  external fn do_list_map(list: List(a), callable: fn(a) -> b) -> List(b) =
    "../glacier_ffi.mjs" "list_map"
}

/// Replaces stdlib's string.replace
/// so that there are no deps on Glacier.
///
pub fn string_replace(s: String, pattern: String, replacement: String) -> String {
  do_string_replace(s, pattern, replacement)
}

if erlang {
  external fn do_string_replace(
    s: String,
    pattern: String,
    replacement: String,
  ) -> String =
    "glacier_ffi" "string_replace"
}

if javascript {
  external fn do_string_replace(
    s: String,
    pattern: String,
    replacement: String,
  ) -> String =
    "../glacier_ffi.mjs" "string_replace"
}

pub fn int_to_string(int: Int) -> String {
  do_int_to_string(int)
}

if erlang {
  external fn do_int_to_string(Int) -> String =
    "glacier_ffi" "int_to_string"
}

if javascript {
  external fn identity(a) -> b =
    "../glacier_ffi.mjs" "identity"
}
//
// /// `Dynamic` data is data that we don't know the type of yet.
// /// From stdlib:
// /// We likely get data like this from interop with Erlang, or from
// /// IO with the outside world.
// ///
// pub external type Dynamic

// /// Replaces stdlib's dynamic.from
// /// so that there are no deps on Glacier.
// ///
// pub fn dynamic_from(a) -> Dynamic {
//   do_dynamic_from(a)
// }

// /// Error returned when unexpected data is encountered
// ///
// pub type DecodeError {
//   DecodeError(expected: String, found: String, path: List(String))
// }

// /// Decodes a `Dynamic` value from a `Dynamic` value.
// ///
// /// This function doesn't seem very useful at first, but it can be convenient
// /// when you need to give a decoder function but you don't actually care what
// /// the to-decode value is.
// ///
// pub fn dynamic_dynamic(value: Dynamic) -> Result(Dynamic, List(DecodeError)) {
//   Ok(value)
// }
