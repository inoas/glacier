//// module doc import in_module_docblock should be ignored

import glacier_demo/glacier_demo_module_a
import glacier_demo/glacier_demo_module_c

/// function doc import in_fn_docblock should be ignored
pub fn function_3() {
  // regular comment import in_comment should be ignored
  let _foo = "import string in string_1 should be ignored"
  let _bar = "in escaped \" string import string_2 should be ignored"
  let _quux = "in escaped \\ string import string_3 should be ignored"
  glacier_demo_module_a.function_1() + 2
}

pub fn function_4() {
  glacier_demo_module_c.function_5() - 1
}

@target(erlang)
import gleam/string

@target(erlang)
pub fn a() {
  string.inspect("a")
}

@target(javascript)
import gleam/string

@target(javascript)
pub fn a() {
  string.inspect("a")
}
