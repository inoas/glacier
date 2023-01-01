# Changelog

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
## v0.8.0 - 2022-12-17

- `should.be_ok` and `should.be_error` now return the contained value when the
  assertion passes.

## v0.7.2 - 2022-11-19

- Update for Gleam v0.25.0.

## v0.7.1 - 2022-11-11

- Fixed a bug where project names containing numbers would not run correctly on
  JavaScript.

## v0.7.0 - 2022-09-24

- Line numbers are printed on JS for assertions.

## v0.6.2 - 2022-07-15

- Fixed a bug where assertions in JavaScript tests could fail to report an
  error due to them being async.

## v0.6.1 - 2022-01-26

- Fixed a bug where failed tests on the JavaScript target would crash the `main`
  function.
- Fixed a bug where tests on the JavaScript target could succeed regardless of
  return value.

## v0.6.0 - 2022-01-09

- Added support for OTP versions below 23.
- Added support for running tests on the JavaScript target.

## v0.5.0 - 2021-12-05

- Updated for Gleam v0.18.0.
- Added `gleeunit/should` module containing assertions.

## v0.4.0 - 2021-11-01

- Slightly improved failure format.

## v0.3.0 - 2021-10-31

- Fixed Hex package which was missing some files.

## v0.2.0 - 2021-10-31

- `gleeunit.discover_and_run_tests` removed.
- `gleeunit.main` added.

## v0.1.0 - 2021-10-31

- Initial release.
=======
=======
=======
=======
=======
## 0.2.4

- More compatibility changes to run `glacier` alongside `gleeunit`:
  Added `glacier/should` which is a renamed `gleeunit/should`.

>>>>>>> d33c986 (0.2.4)
## 0.2.3

- Remove code that did nothing.

>>>>>>> be11c70 (0.2.3)
## 0.2.2

- Fixed `README.md`.

>>>>>>> 2af773c (readme)
## 0.2.1

- Fixed `README.md`.

>>>>>>> 69cbfa0 (fixed readme)
## 0.2.0

<<<<<<< HEAD
<<<<<<< HEAD
- Renamed internal gleeunit fork to gleeunit2 to avoid colissions
- Fixedup readme
- Added logo and shields
>>>>>>> 562c9cc (readme)
=======
- Added changelog file.
=======
- Added `CHANGELOG.md`.
>>>>>>> a630773 (changelog)
- Renamed internal gleeunit fork to gleeunit2 to avoid collisions.
- JavaScript: File renames are picked up by the watcher.
- Replaced `glacier.run()` with `glacier.main()` as latter did not make sense before.
- Fixed `README.md`.
<<<<<<< HEAD
- Added logo and shields badges.
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> b41980d (changelog)
=======
- JavaScript: File renames are picked up by the watcher.
>>>>>>> bcecbbb (changelog)
=======
>>>>>>> e9b4180 (changeog)
=======
- Fixed the logo to work across hexpm and github and shields badges.
>>>>>>> 2906b35 (changelog)
