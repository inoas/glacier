# gleeunit

Gleam bindings to the Erlang EUnit test framework.

## Usage

Add this package to your `gleam.toml` dependencies:

```toml
[dev-dependencies]
gleeunit = "~> 0.1.0"
```

And then call the `discover_and_run_tests` function from your test main function.

```rust
// In test/yourapp_test.gleam
import gleeunit

pub fn main() {
  gleeunit.discover_and_run_tests()
}
```

Now any public function with a name ending in `_test` in the `test` directory
will be found and run as a test.

```rust
pub fn the_universe_test() {
  assert 1 = 1
}
```
