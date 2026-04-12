Can convert in both directions of Korea's National Point Number and longitude and latitude coordinates in WGS84 (GPS).
WGS84 기반 위경도 좌표와 국가지점번호 표기 상호간 변환이 가능합니다.

국가지점번호로
```R
# 서울시청
toNPN(data.frame(lon=126.978422, lat=37.566742))
[1] "다사 5393 5205"

# Romanized output
toNPN(data.frame(lon=126.978422, lat=37.566742), output="abc")
[1] "DaSa 5393 5205"

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
