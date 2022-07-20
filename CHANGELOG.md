# Unreleased

* Drop support for GHC < 8.10
* Drop support for megaparsec < 7

# v1.3.5.1

* Fix benchmarks for `aeson-2`

# v1.3.5

* Support `aeson-2.0.0.0`

# v1.3.4

* Support `template-haskell-2.17.0.0` for GHC 9

# v1.3.3

* Fix test failure in newer Stack snapshots

# v1.3.2

Performance:

* Optimized including other schemas in a schema, which previously caused a huge slowdown, and possibly even out-of-memory errors.

# v1.3.1

Bug fixes:

* Update extra-source-files with files needed for testing

# v1.3.0

Breaking changes:

* Refactored types to be correct by construction. Namely, the `schema` parameter in `Object schema` now has kind `Schema` instead of `SchemaType`, which prevents the possibility of a non-object schema stored in an `Object`. This means that any schemas previously annotated with the `SchemaType` kind should now be annotated as `Schema`.
* Instead of using `IsSchemaObject` is obviated because of this change, so it's been removed. You may use the new `IsSchema` instead, if you need it.
* `SchemaResult` has been removed from the export list of `Data.Aeson.Schema`. You probably won't need this in typical usage of this library, but if you need it, you can always get it from `Data.Aeson.Schema.Internal`.

New features:

* Add support for unwrapping into included schemas
* Add `toMap`
* Re-export `showSchema` in `Data.Aeson.Schema`

Bug fixes:

* Avoid requiring `TypeApplications` when using `get` quasiquoter ([#16](https://github.com/LeapYear/aeson-schemas/issues/16))
* Allow optional quotes around keys, both in getter-expressions and in schema definitions
* Allow `//` at the beginning of phantom keys (were previously parsed as comments)

Performance:

* We've added benchmarks! To view performance metrics, you can clone the repo and run `stack bench`. You may also view the benchmark statistics in CI, but due to Circle CI's memory limitations, we're forced to run them with `--fast`, so it'll be a factor slower than it would actually be at runtime.
* Fixed the `Show` instance from being `O(n^2)` to `O(n)`, where `n` is the depth of the object.
* In order to fix some bugs and implement new features, the `schema` quasiquoter took a performance hit. The biggest slowdown occurs if you're including other schemas like:

    ```
    {
        user: #UserSchema
    }
    ```

    If this causes your build to be noticeably slower, please open an issue. Thanks!

Miscellaneous changes:

* The `Show` instance for objects added some whitespace, from `{"foo": 0}` to `{ "foo": 0 }`

# v1.2.0

New features:

* Add support for phantom keys
* Add support for `Try` schemas

# v1.1.0

New features:

* Added support for unions
* Added `ToJSON` instance for enums generated with `mkEnum`

# v1.0.3

Support GHC 8.8

# v1.0.2

Bundle test data files in release tarball

# v1.0.1

Add support with `first-class-families-0.6.0.0`

# v1.0.0

Initial release:

* Defining JSON schemas with the `schema` quasiquoter
* Extract JSON data using the `get` quasiquoter
* Extracting intermediate schemas with the `unwrap` quasiquoter
* Include `mkGetter` helper function for generating corresponding `get` and
  `unwrap` expressions.
