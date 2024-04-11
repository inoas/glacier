//// A proxy module into gleeunit/should
////
//// If you are consistently using glacier (which wraps gleeunit) instead of
//// gleeunit you may replace all `import gleeunit/should` with
//// `import glacier/should`.

import gleam/option.{type Option}
import gleeunit/should as gleeunit_should

pub fn equal(a: any, b: any) -> Nil {
  gleeunit_should.equal(a, b)
}

pub fn not_equal(a: any, b: any) -> Nil {
  gleeunit_should.not_equal(a, b)
}

pub fn be_ok(result: Result(a, b)) -> a {
  gleeunit_should.be_ok(result)
}

pub fn be_error(result: Result(a, b)) -> b {
  gleeunit_should.be_error(result)
}

pub fn be_some(a: Option(a)) -> a {
  gleeunit_should.be_some(a)
}

pub fn be_none(a: Option(a)) -> Nil {
  gleeunit_should.be_none(a)
}

pub fn be_true(actual: Bool) -> Nil {
  gleeunit_should.be_true(actual)
}

pub fn be_false(actual: Bool) -> Nil {
  gleeunit_should.be_false(actual)
}

pub fn fail() -> Nil {
  gleeunit_should.fail()
}
