# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v2.0.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v2.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#35, #43)
- Dropped `Map`'s `Semigroup` and `Monoid` instances and provide unbiased instances via a `SemigroupMap` newtype instead (#38)
- Updated the `Show` instances for (non empty) sets (#46)

New features:
- Added `Apply` instance for `Map` (#16)
- Added `Alt` and `Plus` instances for `Map` (#38)
- Added `catMaybes` for maps and sets (#25)
- Added `toMap` and `fromMap` to `Data.Set` (#31)

Bugfixes:

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#37)
- Tests: Add parens to account for changed precedence of `::` in PureScript 0.13 (#23)
- Tests: Converted from `NonEmpty Array` to `NonEmptyArray` to accommodate QuickCheck changes (#42)
- Added a changelog and pull request template (#44)

## [v1.6.1](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.6.1) - 2019-04-12

- Fixed doc comments for `Data.Map.toUnfoldableUnordered` (@bernhard-herzog)
- Fixed a syntax issue in preparation for upcoming parser changes in PureScript v0.13.0 (@natefaubion)

## [v1.6.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.6.0) - 2019-02-14

- Added `intersection` and `intersectionWith` for `Map` (@karshan)

## [v1.5.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.5.0) - 2019-02-07

- Added `insertWith` for `Map`

## [v1.4.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.4.0) - 2018-10-05

- Added `fromFoldableWithIndex` for `Map`

## [v1.3.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.3.0) - 2018-10-05

- Added `mapMaybe` functions for all types
- Added `filter` for `Set` types

## [v1.2.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.2.0) - 2018-10-04

- Added `difference` for `Map` (@mschristiansen)

## [v1.1.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.1.0) - 2018-09-06

- Added `NonEmptySet`

## [v1.0.0](https://github.com/purescript/purescript-ordered-collections/releases/tag/v1.0.0) - 2018-05-24

- Initial release
