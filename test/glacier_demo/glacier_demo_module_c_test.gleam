import gleeunit/should
import glacier_demo/glacier_demo_module_c

pub fn function_5_test() {
  glacier_demo_module_c.function_5()
  |> should.equal(5)
}

pub fn function_6_test() {
  glacier_demo_module_c.function_6()
  |> should.equal(6)
}
