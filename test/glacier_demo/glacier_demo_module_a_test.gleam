import glacier/should
import glacier_demo/glacier_demo_module_a

pub fn function_1_test() {
  glacier_demo_module_a.function_1()
  |> should.equal(1)
}

pub fn function_2_test() {
  glacier_demo_module_a.function_2()
  |> should.equal(2)
}
