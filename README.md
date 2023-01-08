# Glacier · Gleam Incremental Interactive Unit Testing

[![Hex Package](https://img.shields.io/hexpm/v/glacier?color=ffaff3&label=%F0%9F%93%A6)](https://hex.pm/packages/glacier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3?label=%F0%9F%93%9A)](https://hexdocs.pm/glacier/)
[![Licence](https://img.shields.io/hexpm/l/glacier?color=ffaff3&label=%F0%9F%93%83)](https://github.com/inoas/glacier/blob/main/LICENCE)

**Glacier** brings incremental interactive unit testing to [Gleam](https://gleam.run).
It is meant as a drop-in replacement for [Gleeunit](https://hexdocs.pm/gleeunit) and depends on a fork of it.

<figure>
	<img src="https://raw.githubusercontent.com/inoas/glacier/main/glacier-logo.png" alt="Glacier Logo" style="max-height: 33vh; width: auto; height: auto" width="480" height="480"/>
  <figcaption><i><small>Glacier: <a href="https://en.wikipedia.org/wiki/Glacier">«A persistent body of dense ice that is constantly moving under its own weight.»</a></small></i></figcaption>
</figure>

## Quick start

1. Run `gleam add glacier --dev`.
2. Open `gleam.toml` and remove `gleeunit` from the dependencies.
3. Open `./test/YOUR_PROJECT.gleam` and replace `import gleeunit` with `import glacier` and `gleeunit.main()` with `glacier.main()`.
4. Run `gleam test --target erlang -- --glacier` or `gleam test --target javascript -- --glacier`.
5. Save a `src` or `test` module and watch associated tests to re-run.

*Notice: On Linux [**inotify**](https://en.wikipedia.org/wiki/Inotify) must be installed.*

## Introduction

**Glacier** differs from **Gleeunit** insofar, that it let's you:

1. Pass in the `glacier` flag like so: `gleam test -- --glacier`, save a module and only related tests will rerun.
2. Or Pass in a specific unit test modules to rerun `gleam test -- test/my_module_test.gleam`.
3. If `gleam test` is passed without any `--`-arguments it behaves the same as **Gleeunit**.
4. You can still pass in `--target erlang` or `--target javascript` like so `gleam test --target erlang -- --glacier`, or like `gleam test --target javascript -- test/my_module_test.gleam`.

To enable this behavior, all you have to do is add **Glacier** as a dev dependency, aka `gleam add glacier --dev`, open `./test/YOUR_PROJECT.gleam` and replace `gleeunit.main()` with `glacier.main()`.

*Note: `gleam test` must only be executed from the base project directory!*

### Testing against Erlang & JavaScript

Run `gleam test --target erlang -- --glacier` and `gleam test --target javascript -- --glacier` in two terminals side by side.

## Caveats, Requirements & Installation

### Caveats

- `./src` and `./test` source files with white spaces will not pass detection.
- Behavior towards `./src` and `./test` source files with non-alphanumeric characters is not tested, and generally unsupported.

### Requirements

Requires Gleam 0.25 or later.

#### Target Erlang

Development and testing only happens on very recent stable Erlang/OTP versions, and thus may or may not run on previous versions.

Depends on [fs](https://hexdocs.pm/fs/):

- Linux [relies](https://github.com/synrc/fs#backends) on [**inotify**](https://en.wikipedia.org/wiki/Inotify).
- Mac: Should work out of the box.
- Windows: Should work out of the box.

#### Target JavaScript/NodeJS

Development and testing only happens on very recent NodeJS LTS versions, and thus may or may not run on previous versions.

Depends on [`fsPromises.watch`](https://nodejs.org/api/fs.html#fspromiseswatchfilename-options):

- Linux: [relies](https://nodejs.org/docs/latest-v18.x/api/fs.html#fs_caveats) on [**inotify**](https://en.wikipedia.org/wiki/Inotify).
- Mac: Should work out of the box.
- Windows: Should work out of the box.

[**Deno**](https://deno.land) is not yet supported.

### Installation

This package is [available on hex.pm](https://hex.pm/packages/glacier) can be added to your Gleam project:

Run `gleam add glacier --dev`.

## Usage

1. Open `./test/YOUR_PROJECT.gleam` and replace `import gleeunit` with `import glacier` and `gleeunit.main()` with `glacier.main()`.
2. Run `gleam test -- --glacier`, then:
   - Save any test module (within `./test`) file to re-run that single test
   - Save any src module (within `./src`) to run all associated tests. Associated tests are test modules where the module is imported or where any of the module's imports and their import's imports (import chain) are imported into.
3. Optional: You may find and replace all `import gleeunit/should` with `import glacier/should` and remove `gleeunit` from your dependencies in `gleam.toml`.

## Documentation

Documentation can be found at <https://hexdocs.pm/glacier>.

### How does it work?

1. `gleam test` passes through `glacier.main()` and simply executes `gleeunit.main()` as if **Gleeunit** was used directly.
2. `gleam test -- test_module_a test_module_b` passes through `glacier.main()` and executes `gleeunit.test_modules(modules_list)` where `modules_list` is `["foo", "bar"]`. The given modules are checked if they exist as either `.gleam` or `.erl` test module files and then **Gleeunit** runs these test modules.
3. `gleam test -- --glacier` enters `glacier.main()` and starts a file watcher: Upon changes in module files in `./test` it just passes those through as `gleam test -- changed_test_module`(so re-saving test files executes the single test), and if a module file in `./src` got changed it parses that changed module file for any imported modules and puts the module and all chained imported modules in a distinct list of modules that should be tested. Then all test module files are read and imports of those are gathered one by one and cross matched against that list. The result is a list of test modules that need to be run, which then gets done by executing a shell call similar to `gleam test -- detected_test_module_a detected_test_module_b detected_test_module_c etc`, aka jump to step `2`.

## Developing Glacier

```sh
git clone https://github.com/inoas/glacier.git
cd glacier
```

See all [open issues](https://github.com/inoas/glacier/issues) on GitHub if you want to help out.

### Demo for target Erlang

```sh
# Traditional test runs
gleam test --target erlang
gleam test --target erlang -- test/glacier_demo/glacier_demo_module_a_test.gleam

# Incremental interactive test
gleam test --target erlang -- --glacier
# Re-save ./src/glacier_demo/glacier_demo_module_a.gleam
```

### Demo for target JavaScript/NodeJS

```sh
# Traditional test runs
gleam test --target javascript
gleam test --target javascript -- test/glacier_demo/glacier_demo_module_a_test.gleam

# Incremental interactive test
gleam test --target javascript -- --glacier
# Re-save ./src/glacier_demo/glacier_demo_module_a.gleam
```

## License

[Apache 2.0](./LICENCE)
