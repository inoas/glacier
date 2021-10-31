import gleeunit

pub fn main() {
  gleeunit.discover_and_run_tests()
}

pub fn some_test() {
  assert 2 = 1 + 1
}

pub fn some_other_test() {
  assert 1 = 1 - 0
}
