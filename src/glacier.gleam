import gleeunit
import glacier/glacier_helpers

pub fn main() {
  glacier_helpers.io_println(
    "Welcome to Glacier - An incremental test runner for Gleam - Usage:

1. In your app, run: gleam add glacier

2. In your app's ./test/YOUR_PROJECT_test.gleam module change:

  import gleeunit

  pub fn main() {
    gleeunit.main()
  }

to:

  import glacier

  pub fn main() {
    glacier.run()
  }

3. In your app run: gleam test
",
  )
}

pub fn run() {
  gleeunit.main()
}
