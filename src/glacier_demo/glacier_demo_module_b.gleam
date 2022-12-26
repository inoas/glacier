//// module doc import string1 should be ignored

import glacier_demo/glacier_demo_module_a
import glacier_demo/glacier_demo_module_c

/// function doc import string2 should be ignored
pub fn function_3() {
  // regular comment import string3 should be ignored
  let _foo = "import string in string4 should be ignored"
  let _bar = "in escaped \" string import string5 should be ignored"
  let _quux = "in escaped \\ string import string6 should be ignored"
  glacier_demo_module_a.function_1() + 2
}

pub fn function_4() {
  glacier_demo_module_a.function_2() + 2
}
