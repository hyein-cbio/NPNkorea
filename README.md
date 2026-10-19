Can convert in both directions of Korea's National Point Number and longitude and latitude coordinates in WGS84 (GPS).
WGS84 기반 위경도 좌표와 국가지점번호 표기 상호간 변환이 가능합니다.

## Installation

GitHub에서 `remotes`를 사용해 설치할 수 있습니다.

```r
install.packages("remotes")
remotes::install_github("hyein-cbio/NPNkorea")
```

설치 후 패키지를 불러옵니다.

```r
library(NPNkorea)
```

국가지점번호로
```R
# 서울시청
toNPN(data.frame(lon=126.978422, lat=37.566742))
[1] "다사 5393 5205"

# Romanized output
toNPN(data.frame(lon=126.978422, lat=37.566742), output="abc")
[1] "DaSa 5393 5205"

# The second positional argument remains the output alphabet
toNPN(data.frame(lon=126.978422, lat=37.566742), "abc")

# Numeric longitude and latitude vectors; pass output by name
toNPN(126.978422, 37.566742, output = "abc")

# sf / sfc POINT geometries; the geometry is used, not attribute columns
pts = sf::st_as_sf(
  data.frame(lon = 126.978422, lat = 37.566742),
  coords = c("lon", "lat"),
  crs = 4326
)
toNPN(pts)

```

GPS 경도, 위도로
```R
# 서울시청
toWGS("다사 5396 5207")
                 lon      lat
다사 5396 5207 126.9787 37.56684

# Compact input without spaces also works
toWGS("DaSa53965207")
               lon      lat
DaSa53965207 126.9787 37.56684

```

문자열이 국가지점번호인지 확인하고, 표기를 한글로 또는 로마자로 정리할 수 있습니다.

```R
is_npn(c("다사 5396 5207", "dasa53965207", "not an npn", NA))
[1] TRUE TRUE FALSE NA

format_npn("dasa53965207")
[1] "다사 5396 5207"

format_npn("다사 5396 5207", output = "abc", spaced = FALSE)
[1] "DaSa53965207"
```

## Command line usage

After installation, the package also includes a small command line wrapper at
`system.file("exec", "npnkorea", package = "NPNkorea")`.

Convert WGS84 coordinates to NPN:

```bash
Rscript "$(Rscript -e 'cat(system.file("exec", "npnkorea", package = "NPNkorea"))')" \
  to-npn --lon 126.978422 --lat 37.566742
```

Expected tab-separated output:

```text
npn
다사 5393 5205
```

Convert NPN strings to WGS84:

```bash
Rscript "$(Rscript -e 'cat(system.file("exec", "npnkorea", package = "NPNkorea"))')" \
  to-wgs --code "다사 5396 5207" --code "MaRa43478827"
```

Expected tab-separated output:

```text
lon	lat
126.97866380715097	37.56684471509263
129.0756266828708	35.17949162224057
```

## Internal lookup table

국가지점번호의 앞 두 글자는 100 km 격자 축을 나타내는 문자 코드입니다. 이 패키지는
숫자 축 번호와 문자 코드를 서로 바꾸기 위해 내부 lookup table을 사용합니다.

Inside the package, this mapping is stored as an internal object called
`digits.df`, saved in `R/sysdata.rda`.

- `toNPN()` uses this table to convert numeric X/Y grid bands into Hangul or romanized labels.
- `toWGS()` uses the same table to parse Hangul or romanized labels back into numeric grid bands.

The internal table has three columns:

- `digits`: numeric grid-band index
- `abc`: romanized label
- `han`: Hangul label

| digits | abc | han |
| --- | --- | --- |
| 1 | Ga | 가 |
| 2 | Na | 나 |
| 3 | Da | 다 |
| 4 | Ra | 라 |
| 5 | Ma | 마 |
| 6 | Ba | 바 |
| 7 | Sa | 사 |
| 8 | A | 아 |
| 9 | Ja | 자 |
| 10 | Cha | 차 |
| 11 | Ka | 카 |
| 12 | Ta | 타 |
| 13 | Pa | 파 |
| 14 | Ha | 하 |

For example, `다사` means X band `다` (`3`) and Y band `사` (`7`), while the
romanized form `DaSa` represents the same pair of grid bands.

## License

This package uses the MIT license.

- `LICENSE` is the R package metadata file required by `License: MIT + file LICENSE` in `DESCRIPTION`.
- `LICENSE.md` contains the full MIT license text for repository readers and is excluded from the built package via `.Rbuildignore`.
