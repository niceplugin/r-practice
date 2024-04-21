library(MASS)

# 기본 산점도
example1 <- function() {
  # 'Cars93' 데이터셋에서 'Price'와 'MPG.city'의 관계를 플로팅합니다.
  with(
    Cars93,
    plot(
      Price, MPG.city,
      main = "Price vs MPG.city",  # 그래프 제목
      xlab = "Price",              # x축 레이블
      ylab = "MPG in City",        # y축 레이블
      pch = 19                   # 사용할 포인트의 형태 지정
    )
  )

  # 'Cars93' 데이터셋을 사용하여 'Price'에 대한 'MPG.city'의 선형 회귀선을 추가합니다.
  with(
    Cars93,
    abline(
      lm(MPG.city ~ Price),     # 선형 모델 회귀선
      col = "red",               # 선의 색상
      lwd = 2                  # 선의 너비
    )
  )

  # 'Cars93' 데이터셋에 대해 'Price'와 'MPG.city'를 사용하여 lowess 스무딩 라인을 그립니다.
  with(
    Cars93,
    lines(
      lowess(Price, MPG.city),   # lowess 스무딩
      col = "blue",             # 라인 색상
      lwd = 2                 # 라인 너비
    )
  )

  # 범례를 추가하여 어떤 라인이 회귀선이고 어떤 라인이 lowess 스무딩인지 표시합니다.
  legend(
    40, 40,                                 # 범례의 위치
    lty = 1,                                # 선 유형
    col = c("red", "blue"),                 # 선 색상
    c('regression', 'lowess'),              # 범례 설명
    lwd = 2,                                # 선 너비
    bty = 'n'                              # 범례 상자 테두리 없음
  )
}

# install.packages("vcd")
library(vcd)

# 모자이크 그림 1
example2 <- function() {
  # 'Arthritis' 데이터셋에 대한 요약 통계를 출력합니다.
  summary(Arthritis)

  # 'Arthritis' 데이터셋에서 성별이 여성인 경우에 대해 'Treatment'와 'Improved'의 교차 테이블을 생성합니다.
  art <- xtabs(~Treatment + Improved, data = Arthritis, subset = Sex == "Female")

  # 생성된 교차 테이블 'art'에 대한 요약 통계를 출력합니다.
  summary(art)

  # 교차 테이블 'art'를 사용하여 모자이크 플롯을 생성하고, 최대 셰이딩을 적용합니다.
  mosaic(art, gp = shading_max)

  # 같은 교차 테이블을 사용하여 모자이크 플롯을 생성하고, HCL 색상 셰이딩을 적용합니다.
  # 'interpolate' 파라미터를 사용해 색상 간격을 조정합니다.
  mosaic(art, gp = shading_hcl, gp_args = list(interpolate = c(1, 1.8)))
}

# 모자이크 그림 2 - 명목형 변수가 여러 개 존재할 때 사용할 수 있는 코드
example3 <- function() {
  # 'Titanic' 데이터셋을 사용하여 'Sex', 'Age', 'Survived' 변수 간의 관계를 모자이크 플롯으로 시각화합니다.
  mosaic(
    ~Sex + Age + Survived, data = Titanic,
    main = "Survival on the Titanic",  # 플롯의 주제목 설정
    shade = TRUE,                      # 셀 간의 상관관계를 색상의 차이로 시각적으로 표현
    legend = TRUE                     # 범례를 표시하여 변수의 카테고리를 설명
  )
}

# 다중산점도
example4 <- function() {
  # 'Cars93' 데이터셋에서 선택한 변수들('Min.Price', 'Price', 'Max.Price', 'MPG.city', 'MPG.highway')로 구성된 새로운 데이터 프레임 'dat1'을 생성합니다.
  dat1 <- subset(Cars93, select = c(Min.Price, Price, Max.Price, MPG.city, MPG.highway))

  # 'dat1' 데이터셋의 모든 변수 쌍에 대해 산점도 행렬을 생성합니다.
  # 이 플롯은 변수 간의 관계를 시각적으로 탐색하는 데 사용됩니다.
  pairs(dat1)
}

# 단순 산점도 - 1
example5 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Price'와 'MPG.city' 사이의 관계를 나타내는 산점도를 생성합니다.
  with(
    Cars93,
    plot(
      Price, MPG.city,
      xlab = 'Price',       # x축 레이블 설정
      ylab = 'MPG in City', # y축 레이블 설정
      main = 'Mileage'    # 그래프의 제목 설정
    )
  )
}

# 단순 산점도 - 2 (그룹별 산점도)
example6 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Price'와 'MPG.city'의 산점도를 그립니다. 그러나 이 단계에서는 점을 실제로 표시하지 않고 ('type = 'n') 플롯 설정만 준비합니다.
  with(Cars93, plot(Price, MPG.city, xlab = 'Price', ylab = 'MPG in City', type = 'n'))

  # 'Cars93' 데이터셋에서 'DriveTrain'이 'Front'인 차량만을 대상으로 점을 'orange' 색상으로 표시합니다.
  with(subset(Cars93, DriveTrain == 'Front'), points(Price, MPG.city, col = 'orange', pch = 19))

  # 'DriveTrain'이 'Rear'인 차량에 대해 'firebrick' 색상의 점을 표시합니다.
  with(subset(Cars93, DriveTrain == 'Rear'), points(Price, MPG.city, col = 'firebrick', pch = 17))

  # 'DriveTrain'이 '4WD'인 차량에 대해 'black' 색상의 점을 표시합니다.
  with(subset(Cars93, DriveTrain == '4WD'), points(Price, MPG.city, col = 'black', pch = 8))

  # 범례를 추가하여 각 'DriveTrain' 유형('Front', 'Rear', '4WD')을 색상과 점 스타일로 구분합니다. 범례 테두리는 없앱니다 ('bty = 'n').
  legend("topright", legend = c('Front', 'Rear', '4WD'), col = c('orange', 'firebrick', 'black'), pch = c(19, 17, 8), bty = 'n')
}

# 단순산점도에 회귀선 추가하기
example7 <- function() {
  # 'Cars93' 데이터셋에서 'DriveTrain'이 'Front'인 데이터에 대해 'Price'에 따른 'MPG.city'의 선형 회귀 모델을 적합합니다.
  fit1 <- with(subset(Cars93, DriveTrain == 'Front'), lm(MPG.city ~ Price))

  # 'DriveTrain'이 'Rear'인 데이터에 대해 동일한 선형 회귀 모델을 적합합니다.
  fit2 <- with(subset(Cars93, DriveTrain == 'Rear'), lm(MPG.city ~ Price))

  # 'DriveTrain'이 '4WD'인 데이터에 대해 동일한 선형 회귀 모델을 적합합니다.
  fit3 <- with(subset(Cars93, DriveTrain == '4WD'), lm(MPG.city ~ Price))

  # 각 'DriveTrain' 범주('Front', 'Rear', '4WD')에 대한 'Price' 데이터를 추출합니다.
  xx1 <- subset(Cars93, DriveTrain == 'Front')$Price
  xx2 <- subset(Cars93, DriveTrain == 'Rear')$Price
  xx3 <- subset(Cars93, DriveTrain == '4WD')$Price

  # 회귀선 계산: 각각의 회귀 모델로부터 추정된 계수를 사용하여 예측된 'MPG.city' 값을 계산합니다.
  yy1 <- fit1$coef[1] + fit1$coef[2] * xx1
  yy2 <- fit2$coef[1] + fit2$coef[2] * xx2
  yy3 <- fit3$coef[1] + fit3$coef[2] * xx3

  # example6 과 동일한 코드
  with(Cars93, plot(Price, MPG.city, xlab = 'Price', ylab = 'MPG in City', type = 'n'))
  with(subset(Cars93, DriveTrain == 'Front'), points(Price, MPG.city, col = 'orange', pch = 19))
  with(subset(Cars93, DriveTrain == 'Rear'), points(Price, MPG.city, col = 'firebrick', pch = 17))
  with(subset(Cars93, DriveTrain == '4WD'), points(Price, MPG.city, col = 'black', pch = 8))
  legend("topright", legend = c('Front', 'Rear', '4WD'), col = c('orange', 'firebrick', 'black'), pch = c(19, 17, 8), bty = 'n')

  # 각 'DriveTrain' 범주에 대해 계산된 회귀선을 그래프에 추가합니다. 각 라인에는 다른 색상과 선 두께를 지정합니다.
  lines(xx1, yy1, col = 'orange', lwd = 2)
  lines(xx2, yy2, col = 'firebrick', lwd = 2)
  lines(xx3, yy3, col = 'black', lwd = 2)
}

# 여러 개의 그림을 동시에 표현하기
example8 <- function() {
  # 그래픽 파라미터 설정: 2x2의 그리드에 플롯을 배열합니다.
  par(mfrow = c(2, 2))

  # 'DriveTrain'이 'Front'인 차량의 'Price'와 'MPG.city' 관계를 플로팅합니다.
  with(
    subset(Cars93, DriveTrain == 'Front'),
    plot(
      Price, MPG.city,
      col = 'orange',    # 점 색상
      pch = 19,          # 점 스타일
      main = 'Front'   # 플롯 제목
    )
  )

  # 'DriveTrain'이 'Rear'인 차량의 'Price'와 'MPG.city' 관계를 플로팅합니다.
  with(
    subset(Cars93, DriveTrain == 'Rear'),
    plot(
      Price, MPG.city,
      col = 'firebrick', # 점 색상
      pch = 17,          # 점 스타일
      main = 'Rear'    # 플롯 제목
    )
  )

  # 'DriveTrain'이 '4WD'인 차량의 'Price'와 'MPG.city' 관계를 플로팅합니다.
  with(
    subset(Cars93, DriveTrain == '4WD'),
    plot(
      Price, MPG.city,
      col = 'black',     # 점 색상
      pch = 8,           # 점 스타일
      main = '4WD'     # 플롯 제목
    )
  )
}

# ggplot2 패키지 이용 (qplot)
# install.packages("ggplot2")
library(ggplot2)

# 그룹별 산점도
example9 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Wheelbase'와 'Width'의 관계를 산점도로 표현합니다.
  qplot(
    Wheelbase, Width, data = Cars93,
    shape = Type,     # 차량 유형별로 점의 모양을 다르게 설정
    color = Type,     # 차량 유형별로 점의 색상을 다르게 설정
    facets = Origin ~ AirBags, # 'Origin'과 'AirBags'의 조합에 따라 다른 패널로 플롯을 분할
    size = I(2),       # 점의 크기를 2로 고정
    xlab = "Wheelbase", # x축 레이블
    ylab = "Car Width" # y축 레이블
  )
}

# treemap 패키지 이용
# install.packages("treemap")
library(treemap)

# 나무지도 그림(Treemap) - 1
example10 <- function() {
  # 'GNI2014' 데이터셋을 불러옵니다.
  data(GNI2014)

  # 'GNI2014' 데이터셋을 사용하여 트리맵을 생성합니다.
  treemap(
    GNI2014,
    index = c("continent", "iso3"),  # 'continent'와 'iso3'를 기반으로 계층적 트리맵을 구성
    vSize = "population",            # 각 사각형의 크기는 'population' 변수에 의해 결정
    vColor = "GNI",                  # 각 사각형의 색상은 'GNI' 변수의 값에 따라 달라짐
    type = "value"                  # 색상을 결정할 때 숫자값을 직접 사용
  )
}

# 나무지도 그림 - 2
example11 <- function() {
  # 'Cars93' 데이터셋을 사용하여 트리맵을 생성합니다.
  treemap(
    Cars93,
    index = c("Manufacturer", "Make"),  # 'Manufacturer'와 'Make'를 기반으로 계층적 트리맵을 구성
    vSize = "Price",                    # 각 사각형의 크기는 'Price' 변수에 의해 결정
    vColor = "AirBags",                 # 각 사각형의 색상은 'AirBags' 변수의 범주에 따라 달라짐
    type = "categorical"               # 색상을 결정할 때 범주형 데이터를 사용
  )
}

# gplots 패키지 이용
# install.packages("gplots")
library(gplots)
# 'Cars93' 데이터셋을 사용하여 'AirBags'와 'Type' 변수의 교차 테이블을 생성합니다.
dt <- with(Cars93, xtabs(~AirBags + Type))

# 풍선그림(Ballon Plot) - 1
example12 <- function() {
  # 생성된 교차 테이블 'dt'를 사용하여 풍선 플롯을 그립니다.
  balloonplot(
    dt,
    main = "Airbags by Car type",  # 플롯의 주 제목 설정
    xlab = "",                     # x축 레이블을 비워 둠
    ylab = "",                     # y축 레이블을 비워 둠
    label = FALSE,                 # 셀 레이블을 표시하지 않음
    show.margins = FALSE          # 마진 수를 표시하지 않음
  )
}

# 풍선그림 - 2
example13 <- function() {
  # 풍선 플롯을 그리기 위해, 이전에 생성된 'dt' 교차 테이블을 사용합니다.
  balloonplot(
    dt,
    main = "Airbags by Car type",  # 플롯의 주 제목 설정
    xlab = "",                     # x축 레이블을 비워 둠
    ylab = "",                     # y축 레이블을 비워 둠
    label = TRUE,                  # 각 셀에 레이블을 표시
    show.margins = TRUE           # 마진 합계를 표시
  )
}

library(graphics)  # graphics 패키지는 base 패키지 설치 필요하지 않음

# graphics 패키지를 활용한 두 변수 모자이크 그림
example14 <- function() {
  # 'dt' 데이터셋을 사용하여 모자이크 플롯을 생성합니다. 이는 'AirBags'와 'Car Type' 변수의 관계를 보여줍니다.
  mosaicplot(
    dt,
    color = TRUE,         # 색상을 사용하여 다른 카테고리를 구분
    las = 1,              # 축 레이블의 방향을 수평으로 설정
    main = "Airbags by Car type"  # 플롯의 주 제목 설정
  )

  # 'Cars93' 데이터셋을 사용하여 'DriveTrain', 'AirBags', 'Origin' 변수의 관계를 보여주는 모자이크 플롯을 생성합니다.
  mosaicplot(
    ~DriveTrain + AirBags + Origin,
    data = Cars93,
    color = TRUE,         # 색상을 사용하여 다른 카테고리를 구분
    las = 1,              # 축 레이블의 방향을 수평으로 설정
    main = "Drive Train by Airbags and Origin",  # 플롯의 주 제목 설정
    ylab = 'Airbag type', # y축 레이블 설정
    xlab = 'Drive Train' # x축 레이블 설정
  )
}
