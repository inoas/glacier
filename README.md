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

![Under construction](./resources/glacier-logo.png)

<!-- [![Package Version](https://img.shields.io/hexpm/v/glacier)](https://hex.pm/packages/glacier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glacier/) -->

**Glacier** brings incremental unit testing to **Gleam**.
It is used as a drop-in replacement for [**Gleeunit**](https://github.com/lpil/gleeunit) and relies on it, internally.

Glacier differs insofar, that it let's you:

1. Pass in an incremental flag `gleam test -- --incremental`.
2. Pass in a specific unit test module to rerun `gleam test -- my_app_test.gleam`.
3. If `gleam test` is passed without any `--`-arguments it behaves the same as `Gleeunit`.
4. You can still pass in `--target erlang` or `--target javascript`.

## Status

![Under construction](https://web.archive.org/web/20090829023556im_/http://geocities.com/okitsugu/underconstruction.gif)

## How does it work?

- `gleam test` enters the main test module and there `gleeunit.main()` needs to be replaced with `glacier.main()`.
- In `glacier.main()` it is checked if any args were given when `gleam test` got called, such as for example `gleam test -- foo bar`
  1. If there are args, in this case `foo` and `bar`, then these are passed into `gleeunit.run_test_modules(erlang_start_args)` which is the same as `gleeunit.main()` except that it does not call `find_files(matching: "**/*.{erl,gleam}", in: "test")` but instead checks if the args given exist as either `.gleam` or `.erl` test module files and then runs the tests on those.
  2. If there are no args then a file watcher starts which upon changes in files in `./test` just passes those through as `gleam test -- changed_test_module`(so re-saving test files executes the single test), and if a file in `./src` got changed it parses that changed mofule file for any imported modules and puts the module and all chained imported modules in a distinct list of modules that should be tested. Then all test files are read and imports of those are gathered one by one and cross matched against that list (filtered). The result is a list of test modules to be run, which then gets done by calling gleam test -- detected_test_module_a detected_test_module_b (goes to 2.)

## TODO

- Restore previous default behavior, add a flag such as `gleam test -- --incremental` to toggle the new behavior.
- Handling of JavaScript src and test modules for node and deno:
  - NodeJS <https://nodejs.org/docs/latest/api/fs.html#fspromiseswatchfilename-options>
  - Deno <https://deno.land/api@v1.29.1?s=Deno.watchFs>
- Handling of Elixir src and test modules.
- Introduce some delay between the file watcher picking up a change and the test running, so that if you find-replace-save-all via an editor it does not try to run the tests 10 times.
- Save import lists per module into an in-memory hash-table. Before a file gets parsed for its imports, build the hash and check if a cached version already exists.
- Use set instead of lists in some places?
- Once gleam does only recompile changed modules: Do not test dependencies if they had been tested recently, aka the first time a module it saved all its dependencies are tested but afterwards only if they changed by keeping a cache table with the module name and an mtime it last run the tests for those modules.
- Backport gleeunit changes.

## Installation & Usage

For Erlang this library depends on [fs](https://hexdocs.pm/fs/). Because of this on Linux you will need [`inotify-tools` to be installed](https://github.com/synrc/fs#backends). On Mac and Windows it should work out of the box.

If available on Hex this package can be added to your Gleam project:

1. Run `gleam add glacier`
2. Open `./test/YOUR_PROJECT.gleam` and replace `gleeunit.main()` with `glacier.main()`
3. Run `gleam test`
   - save any test module (within `./test`) file to re-run that single test
   - save any src module (within `./src`) to run all associated tests. Associated tests are test modules where the module is imported or where any of the module's imports and their import's imports (import chain) are imported into.

<<<<<<< HEAD
<!--
and its documentation can be found at <https://hexdocs.pm/glacier>. -->
>>>>>>> 52d5260 (integrate gleeunit)
=======
... and its documentation can be found at <https://hexdocs.pm/glacier>.
<<<<<<< HEAD
>>>>>>> 6b9b162 (readme)
=======

## Quick start

```sh
git clone https://github.com/inoas/glacier.git
cd glacier
gleam run   # Show instructions how to use the library stand alone
gleam test  # Run the tests on this library
```
>>>>>>> 3df3ccd (readme)
