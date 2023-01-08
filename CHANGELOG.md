# Changelog

## Unreleased - 0.3.0

- Fix detection of unqualified imports.
- Fix detection of aliased imports.
- Fix detection of renames.
- Files with white spaces are now ignored.
- Improved readme.
- Force rebased on `lpil/gleeunit` to keep all contributor history alive.
  - This was required because initially `Glacier` was supposed to depend on
    a patched `Gleeunit`, but the idea of patching `Gleeunit` a bit was
    rejected by the `Gleeunit` team. While this may not be strictly necessary
    this honors previous contributions transparently.
  - This was a quick and dirty process where I saved a lot of time by _not_
    fixing any merges and instead just adding them without touching them
    including the diff noise. As a consequence single commits if checked out
    might not run any more. After the dirty rebase I've commited the
    source code state equvialent to `0.2.7` to restore functionality.
- Proper BE noun instead of an AE one for the `LICENCE` file and naming.

## 0.2.7

- Fix `MaxListenersExceededWarning: Possible EventEmitter memory leak detected` (#4).
- Cleanup `README.md`.

## 0.2.6

- Instruct to use `gleam add glacier --dev` to add as a dev dependency, only.

## 0.2.5

- Readme: Explain that `gleeunit` is optional if `glacier` is installed if `gleeunit/should` is replaced by `glacier/should`.

## 0.2.4

- More compatibility changes to run `glacier` alongside `gleeunit`:
  Added `glacier/should` which is a renamed `gleeunit/should`.

## 0.2.3

- Remove code that did nothing.

## 0.2.2

- Fixed `README.md`.

## 0.2.1

- Fixed `README.md`.

## 0.2.0

- Added `CHANGELOG.md`.
- Renamed internal gleeunit fork to gleeunit2 to avoid collisions.
- JavaScript: File renames are picked up by the watcher.
- Replaced `glacier.run()` with `glacier.main()` as latter did not make sense before.
- Fixed `README.md`.
- Fixed the logo to work across hexpm and github and shields badges.

## 0.1.0

- _Ex post_ rebased on `lpil/gleeunit`

### Gleeunit v0.8

- `should.be_ok` and `should.be_error` now unwrap the result argument

### Gleeunit v0.7.2 - 2022-11-19

- Update for Gleam v0.25.0.

### Gleeunit v0.7.1 - 2022-11-11

- Fixed a bug where project names containing numbers would not run correctly on
  JavaScript.

### Gleeunit v0.7.0 - 2022-09-24

- Line numbers are printed on JS for assertions.

### Gleeunit v0.6.2 - 2022-07-15

- Fixed a bug where assertions in JavaScript tests could fail to report an
  error due to them being async.

### Gleeunit v0.6.1 - 2022-01-26

- Fixed a bug where failed tests on the JavaScript target would crash the `main`
  function.
- Fixed a bug where tests on the JavaScript target could succeed regardless of
  return value.

### Gleeunit v0.6.0 - 2022-01-09

- Added support for OTP versions below 23.
- Added support for running tests on the JavaScript target.

### Gleeunit v0.5.0 - 2021-12-05

- Updated for Gleam v0.18.0.
- Added `gleeunit/should` module containing assertions.

### Gleeunit v0.4.0 - 2021-11-01

- Slightly improved failure format.

### Gleeunit v0.3.0 - 2021-10-31

- Fixed Hex package which was missing some files.

### Gleeunit v0.2.0 - 2021-10-31

- `gleeunit.discover_and_run_tests` removed.
- `gleeunit.main` added.

### Gleeunit v0.1.0 - 2021-10-31

- Initial release.
