//// A proxy module into gleeunit/should
////

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
