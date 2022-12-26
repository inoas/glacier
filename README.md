<<<<<<< HEAD
# gleeunit

Gleam bindings to the Erlang EUnit test framework.

A custom test runner is included for when compiled to JavaScript.

Documentation is available on [HexDocs](https://hexdocs.pm/gleeunit/index.html).

## Usage

Add this package to your Gleam project.

```sh
gleam add gleeunit --dev
```

And then call the `gleeunit.main` function from your test main function.

```rust
// In test/yourapp_test.gleam
import gleeunit

pub fn main() {
  gleeunit.main()
}
```

Now any public function with a name ending in `_test` in the `test` directory
will be found and run as a test.

```rust
pub fn the_universe_test() {
  assert 1 = 1
}
```

Run the tests by entering `gleam test` in the command line.
=======
# Glacier - Gleam Incremental Unit Testing

<!-- [![Package Version](https://img.shields.io/hexpm/v/glacier)](https://hex.pm/packages/glacier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glacier/) -->

A Gleam project

## Quick start

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## Status

![Under construction](https://web.archive.org/web/20090829023556im_/http://geocities.com/okitsugu/underconstruction.gif)

<!-- ## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add glacier
```

and its documentation can be found at <https://hexdocs.pm/glacier>. -->
>>>>>>> 52d5260 (integrate gleeunit)
