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

## How does it work?

- `gleam test` enters the main test module and there `gleeunit.main()` needs to be replaced with `glacier.main()`.
- in `glacier.main()` I am checking if there are any args when `gleam test` got called, such as for example `gleam test -- foo bar`
  1. if there are args, in this case `foo` and `bar`, then these are passed into `gleeunit.run_test_modules(erlang_start_args)` which is the same as `gleeunit.main()` except that it does not call `find_files(matching: "**/*.{erl,gleam}", in: "test")` but instead checks if the args given exist as either `.gleam` or `.erl` test module files and then runs the tests on those.
  2. if there are no args then a file watcher starts which upon changes in files in `./test` just passes those through as `gleam test -- changed_test_module`(so re-saving test files executes the single test), and if a file in `./src` got changed it parses that changed mofule file for any imported modules and puts the module and all chained imported modules in a distinct list of modules that should be tested. Then all test files are read and imports of those are gathered one by one and cross matched against that list (filtered). The result is a list of test modules to be run, which then gets done by calling gleam test -- detected_test_module_a detected_test_module_b (goes to 2.)

## TODO

- Restore previous default behavior, add a flag such as `gleam test -- --incremental` to toggle the new behavior.
- Introduce some delay between the file watcher picking up a change and the test running, so that if you find-replace-save-all via an editor it does not try to run the tests 10 times.
- Save import lists per module into an in-memory hash-table. Before a file gets parsed for its imports, build the hash and check if a cached version already exists.
- Use set instead of lists?
- Handling of Elixir src and test modules
- Handling of JavaScript src and test modules for node and deno
- Once gleam does only recompile changed modules: Do not test dependencies if they had been tested recently, aka the first time a module it saved all its dependencies are tested but afterwards only if they changed by keeping a cache table with the module name and an mtime it last run the tests for those modules.

## Installation & Usage

For Erlang this library depends on [fs](https://hexdocs.pm/fs/). Because of this on Linux you will need [`inotify-tools` to be installed](https://github.com/synrc/fs#backends). On Mac and Windows it should work out of the box.

If available on Hex this package can be added to your Gleam project:

1. run `gleam add glacier`
2. open `./test/YOUR_PROJECT.gleam` and replace `gleeunit.main()` with `glacier.main()`
3. run `gleam test`
   - save any test module (within `./test`) file to re-run that single test
   - save any src module (within `./src`) to run all associated tests. Associated tests are test modules where the module is imported or where any of the module's imports and their import's imports (import chain) are imported into.

... and its documentation can be found at <https://hexdocs.pm/glacier>.
