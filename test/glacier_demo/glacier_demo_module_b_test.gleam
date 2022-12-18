import gleeunit/should
import glacier_demo/glacier_demo_module_b

pub fn function_3_test() {
  glacier_demo_module_b.function_3()
  |> should.equal(3)
}

pub fn function_4_test() {
  glacier_demo_module_b.function_4()
  |> should.equal(4)
}
