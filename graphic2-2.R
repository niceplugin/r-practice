# install.packages("doBy")
library(doBy)
library(MASS)
# install.packages("fmsb")
library(fmsb)

# 육각 레이더 차트
example1 <- function() {
  # 'Cars93' 데이터셋에 대해 각 차량 유형('Type')별로 주요 변수들의 평균을 계산합니다.
  mean_by_Type2 <- summaryBy(
    MPG.highway +
      RPM +
      Horsepower +
      Weight +
      Length +
      Price ~ Type,
    data = Cars93,
    FUN = c(mean)
  )

  # 평균 데이터셋에서 특정 열만 선택하여 새로운 데이터프레임 'df2'를 생성합니다.
  df2 <- mean_by_Type2[, c(2:7)]

  # 데이터 프레임을 입력으로 받아 각 변수의 최대값과 최소값을 포함한 새로운 데이터 프레임을 반환하는 함수 'df_radarchart'를 정의합니다.
  df_radarchart <- function(df) {
    df <- data.frame(df)
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    as.data.frame(rbind(dfmax, dfmin, df))
  }

  # 'df2' 데이터프레임을 사용하여 레이더 차트에 필요한 데이터를 준비합니다.
  mean_by_Type <- df_radarchart(df2)
  # 행 이름을 'max', 'min' 및 차량 유형별로 설정합니다.
  row.names(mean_by_Type) <- c('max', 'min', names(table(Cars93$Type)))

  # 준비된 데이터를 바탕으로 레이더 차트를 그립니다.
  radarchart(
    mean_by_Type,  # 사용할 데이터 프레임
    seg = 6,       # 각 축의 세그먼트 수
    pty = 16,      # 데이터 포인트의 모양을 지정 (16은 닫힌 원)
    pcol = 1:6,    # 데이터 색상
    plty = 1,      # 라인 유형
    plwd = 2,      # 라인 너비
    title = c("Radar chart by Car Types")  # 차트 제목
  )

  # 레이더 차트의 상단 왼쪽에 범례를 추가합니다. 범례는 차량 유형('Type')을 나타내며, 각 유형에 대해 다른 색상을 사용합니다.
  legend(
    "topleft",                    # 범례의 위치를 플롯의 상단 왼쪽에 배치
    legend = mean_by_Type2$Type,  # 범례의 텍스트는 'mean_by_Type2' 데이터 프레임의 'Type' 열에서 가져옴
    col = c(1:6),                 # 범례의 색상을 1부터 6까지의 색상 코드로 설정
    lty = 1,                      # 라인 유형을 실선으로 설정
    lwd = 2                      # 라인의 너비를 2로 설정
  )
}

# 오각 레이더 차트
example2 <- function() {
  # 'Cars93' 데이터셋의 2~6행에서 몇 가지 열을 선택하여 'dat' 데이터 프레임을 생성합니다.
  dat <- Cars93[2:6, c('Price', 'Horsepower', 'Turn.circle', 'Rear.seat.room', 'Luggage.room')]

  # 각 열에 대해 최대값을 계산합니다.
  datmax <- apply(dat, 2, max)

  # 각 열에 대해 최소값을 계산합니다.
  datmin <- apply(dat, 2, min)

  # 계산된 최대값, 최소값 및 원본 데이터를 행으로 결합합니다.
  dat <- rbind(datmax, datmin, dat)

  # 레이더 차트를 그리며, 여러 시각적 매개변수를 설정합니다.
  radarchart(
    dat,
    seg = 5,  # 각 축에 대한 세그먼트 수는 5
    plty = 1,  # 라인 유형은 실선
    vlabels = c('Price', 'Horsepower', 'U-turn space\n(feet)', 'Rear seat room\n(inches)', 'Luggage capacity\n(cubic feet)'),  # 변수 레이블
    title = "5 segments, with specified vlabels",  # 차트 제목
    vlcex = 0.8,  # 레이블의 문자 크기 조정
    pcol = rainbow(5)  # 데이터 포인트 및 선의 색상을 무지개 색상으로 설정
  )

  # 차트에 범례를 추가합니다. 범례는 'Cars93' 데이터셋의 'Make' 열에서 가져온 차량 이름을 표시합니다.
  legend(
    "topleft",  # 범례 위치는 플롯의 상단 왼쪽
    legend = Cars93[2:6, 'Make'],  # 범례 텍스트
    col = rainbow(5),  # 범례의 색상
    lty = 1,  # 라인 유형은 실선
    lwd = 1  # 라인 너비
  )
}

library(ggplot2)

# 상자그림 고밀도 산점도 - 1
example3 <- function() {
  # 'diamonds' 데이터셋에 대한 요약 통계를 출력합니다.
  summary(diamonds)

  # 'diamonds' 데이터셋을 사용하여 'carat'과 'price'의 관계를 이차원 빈 플롯으로 시각화합니다.
  # bins 매개변수를 사용하여 빈의 수를 25로 설정하고, 빈의 경계 색상을 회색으로 지정합니다.
  ggplot(diamonds, aes(carat, price)) + stat_bin2d(bins = 25, colour = "grey50")
}

# 상자그림 고밀도 산점도 - 2
example4 <- function() {
  # 'diamonds' 데이터셋을 사용하여 'carat'과 'price'의 관계를 이차원 빈 플롯으로 시각화합니다.
  # 이 플롯은 다이아몬드의 캐럿 수에 따른 가격 분포를 표현하며, 빈의 수를 40으로 지정하고, 빈의 경계 색상을 회색으로 설정합니다.
  ggplot(diamonds, aes(carat, price)) +
    # 이차원 빈 플롯을 그리며, 빈의 수와 경계 색상 지정
    stat_bin2d(bins = 40, colour = "grey50") +
    # x축의 연속적인 스케일을 조정하며, x축의 제한을 0에서 6까지로 설정
    scale_x_continuous(limits = c(0, 6))
}

# 상자그림 고밀도 산점도 - 3
example5 <- function() {
  # 'diamonds' 데이터셋을 사용하여 'carat'과 'price'의 관계를 이차원 빈 플롯으로 시각화합니다.
  # 플롯은 다이아몬드의 캐럿 수에 따른 가격 분포를 표현하며, 빈의 수를 40으로 설정하고, 빈의 경계 색상을 회색으로 지정합니다.
  # 또한, 색상 그라데이션을 적용하여 데이터의 밀도에 따라 색상이 변하도록 합니다.
  ggplot(diamonds, aes(carat, price)) +
    # 이차원 빈 플롯을 그리며, 빈의 수와 경계 색상 지정
    stat_bin2d(bins = 40, colour = "grey50") +
    # x축의 연속적인 스케일을 조정하며, x축의 제한을 0에서 6까지로 설정
    scale_x_continuous(limits = c(0, 6)) +
    # 플롯의 제목을 설정
    ggtitle("Price vs Carat") +
    # 플롯 제목의 수평 위치를 중앙으로 조정
    theme(plot.title = element_text(hjust = 0.5)) +
    # 밀도에 따라 색상 그라데이션 적용
    scale_fill_gradientn(colours = c('yellow', 'green', 'blue', 'red')) +
    # x축과 y축 레이블을 설정
    labs(x = "Carat", y = "Price")
}

# install.packages("SwissAir")
library(SwissAir)

# 고밀도 자료에 대한 일반 산점도
example6 <- function() {
  # 'AirQual' 데이터셋의 차원을 출력합니다. 이는 데이터셋에 있는 행과 열의 수를 확인하는데 사용됩니다.
  dim(AirQual)

  # 'AirQual' 데이터셋을 사용하여 오존(O3) 수치와 풍속(WS) 간의 관계를 산점도로 표시합니다.
  with(
    AirQual,
    plot(
      ad.WS ~ ad.O3,
      xlab = 'O3',   # x축 레이블을 'O3'로 설정
      ylab = 'WS'  # y축 레이블을 'WS'로 설정
    )
  )
}

# Smoothed density 방법을 이용한 산점도 (smoothScatter 함수를 이용)
example7 <- function() {
  # 'AirQual' 데이터셋을 사용하여 풍속(WS)과 오존(O3) 수치 간의 관계를 스무드 스캐터 플롯으로 표시합니다.
  # 이 방법은 데이터의 밀도를 기반으로 점을 부드럽게 표현하여, 높은 데이터 밀도 영역을 더 잘 시각화합니다.
  with(
    AirQual,
    smoothScatter(
      ad.WS, ad.O3,
      main = 'Scatter plot by smoothed densities',  # 플롯의 주 제목 설정
      xlab = 'WS',  # x축 레이블을 'WS'로 설정
      ylab = 'O3' # y축 레이블을 'O3'로 설정
    )
  )
}

# install.packages("hexbin")
library(hexbin)

# 고밀도 자료에 대한 육면 상자 그림(Hexagonal Binning Plot) - 1
example8 <- function() {
  # 'AirQual' 데이터셋을 사용하여 오존(O3) 수치와 풍속(WS) 간의 관계를 헥사고날 빈닝을 통해 시각화합니다.
  # 헥사고날 빈닝은 데이터 포인트를 육각형 구역으로 그룹화하여, 데이터의 밀도를 색상으로 표현하는 방법입니다.
  with(
    AirQual,
    plot(
      hexbin(ad.O3, ad.WS, xbins = 100),  # 헥사고날 빈을 사용하고 빈의 수를 100으로 설정
      main = 'Hexagonal binning(bins=100)',  # 플롯의 주 제목 설정
      xlab = 'O3',  # x축 레이블을 'O3'로 설정
      ylab = 'WS' # y축 레이블을 'WS'로 설정
    )
  )
}

# 고밀도 자료에 대한 육면 상자 그림(Hexagonal Binning Plot) - 2
example9 <- function() {
  # 'AirQual' 데이터셋을 사용하여 오존(O3) 수치와 풍속(WS) 간의 관계를 헥사고날 빈닝을 통해 시각화합니다.
  # 헥사고날 빈닝은 데이터 포인트를 육각형 구역으로 그룹화하여, 데이터의 밀도를 색상으로 표현하는 방법입니다.
  with(
    AirQual,
    plot(
      hexbin(ad.O3, ad.WS, xbins = 30),  # 헥사고날 빈을 사용하고 빈의 수를 30으로 설정
      main = 'Hexagonal binning(bins=30)',  # 플롯의 주 제목 설정
      xlab = 'O3',  # x축 레이블을 'O3'로 설정
      ylab = 'WS' # y축 레이블을 'WS'로 설정
    )
  )
}

# install.packages("IDPmisc")
library(IDPmisc)

# 이미지 산점도(Image Scatter Plot)
example10 <- function() {
  # 'AirQual' 데이터셋을 사용하여 오존(O3) 수치와 풍속(WS) 간의 관계를 이미지 스캐터 플롯으로 시각화합니다.
  # 이미지 스캐터 플롯은 데이터의 밀도에 따라 색상을 사용하여 데이터 포인트를 표현합니다.
  with(
    AirQual,
    iplot(
      ad.O3, ad.WS,
      xlab = 'O3',  # x축 레이블을 'O3'로 설정
      ylab = 'WS',  # y축 레이블을 'WS'로 설정
      main = 'Image Scatter Plot with \n Color Indicating Density'  # 플롯의 주 제목 설정
    )
  )
}

# 다변수 고밀도 자료에 대한 이미지 산점도 (ipairs 함수 사용)
example11 <- function() {
  # 'AirQual' 데이터셋에서 선택된 변수들('ad.O3', 'ad.WS', 'ad.WD')을 사용하여 상호 산점도를 그립니다.
  # 이를 통해 변수 간의 관계를 다양한 산점도를 통해 시각적으로 비교할 수 있습니다.
  ipairs(subset(AirQual, select = c(ad.O3, ad.WS, ad.WD)))
}
