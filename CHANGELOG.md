## Upcoming

Bug fixes:

* Avoid requiring `TypeApplications` when using `get` quasiquoter ([#16](https://github.com/LeapYear/aeson-schemas/issues/16))

## 1.2.0

New features:

* Add support for phantom keys
* Add support for `Try` schemas

## 1.1.0

New features:

* Added support for unions
* Added `ToJSON` instance for enums generated with `mkEnum`

## 1.0.3

Support GHC 8.8

## 1.0.2

Bundle test data files in release tarball

## 1.0.1

Add support with `first-class-families-0.6.0.0`

## 1.0.0

Initial release:

* Defining JSON schemas with the `schema` quasiquoter
* Extract JSON data using the `get` quasiquoter
* Extracting intermediate schemas with the `unwrap` quasiquoter
* Include `mkGetter` helper function for generating corresponding `get` and
  `unwrap` expressions.
