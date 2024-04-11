# Glacier · Gleam Incremental Interactive Unit Testing

[![Hex Package](https://img.shields.io/hexpm/v/glacier?color=ffaff3&label=%F0%9F%93%A6)](https://hex.pm/packages/glacier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3?label=%F0%9F%93%9A)](https://hexdocs.pm/glacier/)
[![Licence](https://img.shields.io/hexpm/l/glacier?color=ffaff3&label=%F0%9F%93%83)](https://github.com/inoas/glacier/blob/main/LICENCE)

**Glacier** brings incremental interactive unit testing to
[Gleam](https://gleam.run). It is meant as a drop-in replacement for
[gleeunit](https://hexdocs.pm/gleeunit) and depends on a fork of it.

<figure>
	<img src="https://raw.githubusercontent.com/inoas/glacier/main/glacier-logo.png" alt="Glacier Logo" style="max-height: 33vh; width: auto; height: auto" width="480" height="480"/>
  <figcaption><i><small>Glacier: <a href="https://en.wikipedia.org/wiki/Glacier">«A persistent body of dense ice that is constantly moving under its own weight.»</a></small></i></figcaption>
</figure>

## Installation

1. Run:

   ```shell
   gleam remove gleeunit
   gleam add glacier --dev
   gleam clean
   ```

2. Open `YOUR_GlEAM_PROJECT/test/YOUR_GLEAM_PROJECT.gleam` and replace `import gleeunit` with
  `import glacier` and `gleeunit.main()` with `glacier.main()` (or alias glacier as gleeunit).
3. If you want to run on **Deno 1.30+** open `gleam.toml` and add:

   ```toml
   [javascript.deno]
   allow_read = ["./"]
   allow_net = ["deno.land"]
   allow_run = ["gleam"]
   ```

## Usage

1. Run any one of these:
   - `gleam test --target erlang -- --glacier`
   - `gleam test --target javascript --runtime deno -- --glacier`
   - `gleam test --target javascript --runtime node -- --glacier`
2. Save gleam module within your projects `src` or `test` gleam directory and
   watch associated tests to re-run.

*Notice: On Linux [**inotify**](https://en.wikipedia.org/wiki/Inotify) must be installed.*

## Upgrading Glacier

Make sure to run `gleam clean` after upgrading Glacier as a dependency.

## Improvements over gleeunit

**Glacier** differs from **gleeunit** insofar, that it let's you:

1. Pass in the `glacier` flag like so: `gleam test -- --glacier`, save a gleam
   module and only related test modules will rerun.
2. Or pass in a specific unit test modules to rerun, for example:
   `gleam test -- test/my_module_test.gleam`.
3. If `gleam test` is passed without any `--`-arguments it behaves the same as
   **gleeunit**.
4. You can still pass in target or runtime flags, such as `--target javascript`
   `--runtime deno`:
   - `gleam test --target javascript --runtime deno -- --glacier`
   - `gleam test --target javascript --tuntime deno -- test/my_module_test.gleam`.

*Note: `gleam test` must only be executed from the base project directory!*

### Testing against Erlang, NodeJS and Deno simultaneously

Run these in 3 terminals side by side:

- `gleam test --target erlang -- --glacier`
- `gleam test --target javascript --runtime node -- --glacier`
- `gleam test --target javascript --runtime deno -- --glacier`

## Requirements & Installation

### Requirements

Requires Gleam 1.0.0 or later.

#### Target specific requirements

- Erlang/OTP 25 (lower may or may not run)
- NodeJS 18 LTS+ (lower may or may not run)
- Deno v1.30.0+ (lower does not run)

#### Target Erlang

Development and testing only happens on very recent stable Erlang/OTP versions, and thus may or may not run on previous versions.

Depends on [fs](https://hexdocs.pm/fs/):

- Linux [relies](https://github.com/synrc/fs#backends) on [**inotify**](https://en.wikipedia.org/wiki/Inotify).
- Mac: Should work out of the box.
- Windows: Should work out of the box.

#### Target JavaScript/NodeJS/Deno

Development and testing only happens on very recent NodeJS LTS versions, and thus may or may not run on previous versions.

Depends on [NodeJS:`fsPromises.watch`](https://nodejs.org/api/fs.html#fspromiseswatchfilename-options) or [`Deno.watchFs`](https://deno.land/api@v1.30.0?s=Deno.watchFs):

- Linux: [relies](https://nodejs.org/docs/latest-v18.x/api/fs.html#fs_caveats) on [**inotify**](https://en.wikipedia.org/wiki/Inotify).
- Mac: Should work out of the box.
- Windows: Should work out of the box.

Optional: You may find and replace all `import gleeunit/should` with `import glacier/should` and remove `gleeunit` from your dependencies in `gleam.toml`.

## Documentation

Documentation can be found at <https://hexdocs.pm/glacier>.

## Improving Glacier

See [IMPROVE_GLACIER.md](./DEVELOPMENT.md).

## License

[Apache 2.0](./LICENCE)
