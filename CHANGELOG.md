# Changelog

## Unreleased - 0.2.8

- Force rebased on `lpil/gleeunit` and then reverted any changes to the
`0.2.6` main commit state to keep all contributor history alive. This was
required because initially `Glacier` was supposed to depend on a patched
`Gleeunit`, but the idea of patching `Gleeunit` a bit was rejected by the
`Gleeunit` team. While this may not be strictly necessary this honors
previous contributions transparently.
- Fixed readme
- Proper BE noun instead of an AE one for the `LICENCE` file.

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
