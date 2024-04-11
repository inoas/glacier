# Glacier · Contribution Guide

## Improving Glacier

```shell
git clone https://github.com/inoas/glacier.git
cd glacier
```

See all [open issues](https://github.com/inoas/glacier/issues) on GitHub if you want to help out.

## How does it work?

1. `gleam test` passes through `glacier.main()` and simply executes `gleeunit.main()` as if **gleeunit** was used directly.
2. `gleam test -- test_module_a test_module_b` passes through `glacier.main()` and executes `gleeunit.test_modules(modules_list)` where `modules_list` is `["foo", "bar"]`. The given modules are checked if they exist as either `.gleam` or `.erl` test module files and then **gleeunit** runs these test modules.
3. `gleam test -- --glacier` enters `glacier.main()` and starts a file watcher: Upon changes in module files in `./test` it just passes those through as `gleam test -- changed_test_module`(so re-saving test files executes the single test), and if a module file in `./src` got changed it parses that changed module file for any imported modules and puts the module and all chained imported modules in a distinct list of modules that should be tested. Then all test module files are read and imports of those are gathered one by one and cross matched against that list. The result is a list of test modules that need to be run, which then gets done by executing a shell call similar to `gleam test -- detected_test_module_a detected_test_module_b detected_test_module_c etc`, aka jump to step `2`.

### Demo for target Erlang

```shell
# Traditional test runs
gleam test --target erlang
gleam test --target erlang -- test/glacier_demo/glacier_demo_module_a_test.gleam

# Incremental interactive test
gleam test --target erlang -- --glacier
# Re-save ./src/glacier_demo/glacier_demo_module_a.gleam
```

### Demo for target JavaScript on NodeJS

```shell
# Traditional test runs
gleam test --target javascript --runtime node
gleam test --target javascript --runtime node -- test/glacier_demo/glacier_demo_module_a_test.gleam

# Incremental interactive test
gleam test --target javascript --runtime node -- --glacier
# Re-save ./src/glacier_demo/glacier_demo_module_a.gleam
```

Notice: You may omit `--runtime node` as it is currently set as the default runtime for target `javascript`.

### Demo for target JavaScript on Deno

```shell
# Traditional test runs
gleam test --target javascript --runtime deno
gleam test --target javascript --runtime deno -- test/glacier_demo/glacier_demo_module_a_test.gleam

# Incremental interactive test
gleam test --target javascript --runtime deno -- --glacier
# Re-save ./src/glacier_demo/glacier_demo_module_a.gleam
```

#### Privileges required for Deno

Do not forget to edit `gleam.toml` to add deno privileges:

```toml
[javascript.deno]
allow_read = ["./"]
allow_net = ["deno.land"]
allow_run = ["gleam"]
```

### Caveats

In line with gleam module name requirements:

- `./src` and `./test` source files with white spaces will not pass detection.
- Behavior towards `./src` and `./test` source files with non-alphanumeric characters is not tested, and generally unsupported.
