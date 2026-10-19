# NPNkorea 0.1.1

* `toNPN()` now accepts `sf` and `sfc` POINT geometries, including coordinates stored in a CRS other than EPSG:4326, and numeric longitude/latitude vectors. Data-frame input is unchanged, including `toNPN(df, "abc")` and abbreviated names such as `out`. An `sf` object is read from its POINT geometry, not from `lon`/`lat` columns.
* Added `is_npn()` to test NPN strings without stopping on malformed values.
* Added `format_npn()` to normalize Hangul and romanized NPN strings, with optional compact output.
