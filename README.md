Can convert in both directions of Korea's National Point Number and longitude and latitude coordinates in WGS84 (GPS).
WGS84 기반 위경도 좌표와 국가지점번호 표기 상호간 변환이 가능합니다.

국가지점번호로
```R
# 서울시청
toNPN(data.frame(lon=126.978422, lat=37.566742))
[1] "다사 5393 5205"

```

GPS 경도, 위도로
```R
# 서울시청
toWGS("다사 5396 5207")
                   [,1]     [,2]
다사 5396 5207 126.9787 37.56684

```

