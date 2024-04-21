# install.packages("magrittr")
library(magrittr)
# install.packages("corrr")
library(corrr)

# 상관도 그림
example1 <- function() {
  # 'mtcars' 데이터셋의 처음 몇 행을 출력합니다.
  mtcars %>% head

  # 'mtcars' 데이터셋에 대해 tan, cos, sin 함수를 순차적으로 적용하고, 결과의 처음 몇 행을 출력한 후 소수점 두 자리로 반올림합니다.
  mtcars %>%
    tan %>%
    cos %>%
    sin %>%
    head %>%
    round(2)

  # 'mtcars' 데이터셋에서 모든 변수들 간의 상관 계수를 계산하고, 'fashion' 함수를 사용하여 결과를 깔끔하게 출력합니다.
  mtcars %>% correlate() %>% fashion()

  # 'mtcars' 데이터셋의 상관 계수를 계산하고, 'rplot' 함수를 사용하여 결과를 시각화합니다.
  mtcars %>% correlate() %>% rplot()
}

# 상관도 네트워크 그림 (Correlation Network Plot)
example2 <- function() {
  # 'mtcars' 데이터셋에 대해 상관 계수를 계산하고,
  # 상관 계수가 0.3 이상인 경우에 대한 네트워크 플롯을 생성합니다.
  mtcars %>%
    correlate() %>%
    network_plot(min_cor = 0.3)
}

library(MASS)
library(Hmisc)

# 변수 군집 그림(Variable Clustering Plot)
example3 <- function() {
  # 'Cars93' 데이터셋에서 특정 변수들('Price', 'MPG.city', 'Horsepower', 'RPM', 'Length', 'Wheelbase')을 선택하여 'temp' 데이터 프레임을 생성합니다.
  temp <- Cars93[, c('Price', 'MPG.city', 'Horsepower', 'RPM', 'Length', 'Wheelbase')]

  # 'temp' 데이터 프레임에 대해 변수 클러스터링을 수행하고, 스피어만 상관 계수를 사용하여 유사성을 계산합니다.
  # 계산된 클러스터 구조를 플롯으로 시각화합니다.
  plot(v <- varclus(~., data = temp, similarity = "spear"))
}

# install.packages("jpeg")
library(jpeg)

# JPEG 그림 불러오기
example4 <- function() {
  # 'sales.amount' 벡터를 생성합니다. 이는 각 월의 판매액(백만 달러)을 나타냅니다.
  sales.amount <- c(1.5, 2.3, 5.4, 7.5, 9, 8)

  # JPEG 이미지 파일을 불러옵니다.
  img <- readJPEG("./data/car.jpg")

  # 빈 플롯 영역을 설정합니다. 축은 표시하지 않으며, x축과 y축 레이블의 크기를 1.3으로 설정합니다.
  plot(c(0.5, 6.5), c(0, 10), axes = F, cex.lab = 1.3, type = 'n', xlab = 'Months', ylab = 'Sales (in million dollars)')

  # x축을 사용자 정의합니다. 1부터 6까지의 위치에 월 이름을 레이블로 표시하고, 레이블 크기를 1.2로 설정합니다.
  axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c('January', 'February', 'March', 'April', 'May', 'June'), cex.axis = 1.2)

  # y축을 사용자 정의합니다. 0부터 10까지 2 단위로 레이블을 표시하고, 레이블 크기를 1.2로 설정합니다.
  axis(2, at = c(0, 2, 4, 6, 8, 10), labels = c('0', '2', '4', '6', '8', '10'), cex.axis = 1.2)

  # 월별 판매액 데이터에 대한 선을 그립니다. 선의 색상은 'orange'이며, 선의 너비는 2입니다.
  lines(1:6, sales.amount, lwd = 2, col = 'orange')

  # 각 데이터 포인트에 이미지를 플롯합니다. 이미지는 각 데이터 포인트 주위에 배치됩니다.
  for (jj in 1:6) {
    rasterImage(img, jj - 0.3, sales.amount[jj] - 0.3, jj + 0.3, sales.amount[jj] + 0.3)
  }
}

# install.packages("png")
library(png)

# PNG 그림 불러오기
example5 <- function() {
  # PNG 이미지 파일을 불러옵니다.
  img <- readPNG("./data/car2.png")

  # 빈 플롯 영역을 설정합니다. 축은 표시하지 않으며, x축과 y축 레이블의 크기를 1.3으로 설정합니다.
  plot(c(1, 6), c(15, 46), cex.lab = 1.3, axes = F, type = 'n', xlab = 'Engine Size', ylab = 'MPG in City')

  # 전체 플롯 영역에 이미지를 배치합니다.
  rasterImage(img, 0.7, 14.5, 6.1, 46.0)

  # x축을 사용자 정의합니다. 1부터 6까지의 위치에 엔진 크기를 레이블로 표시하고, 레이블 크기를 1.2로 설정합니다.
  axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c('1', '2', '3', '4', '5', '6'), cex.axis = 1.2)

  # y축을 사용자 정의합니다. 15에서 45까지 5 단위로 레이블을 표시하고, 레이블 크기를 1.2로 설정합니다.
  axis(2, at = seq(15, 45, by = 5), labels = seq(15, 45, by = 5), cex.axis = 1.2)

  # 'Cars93' 데이터셋에서 'Origin'이 'non-USA'인 차량의 'EngineSize'와 'MPG.city' 데이터를 점으로 표시합니다.
  with(subset(Cars93, Origin == 'non-USA'), points(EngineSize, MPG.city, col = 2, pch = 16))

  # 'Origin'이 'USA'인 차량의 데이터를 다른 색상으로 점으로 표시합니다.
  with(subset(Cars93, Origin == 'USA'), points(EngineSize, MPG.city, col = 4, pch = 16))

  # 범례를 추가하여 'non-USA'와 'USA'의 차량을 구분합니다.
  legend('topright', bty = 'n', c('non-USA', 'USA'), col = c(2, 4), lwd = 2, pch = 16)

  # 특정 지점에 텍스트를 추가하여 차량의 특성에 대해 설명합니다.
  text(2.3, 35, pos = 4, 'Cars in USA have low MPGs in City, \nwhile having large engines compared \nto non-USA.', col = 1)
}

# 그림 저장
example6 <- function() {
  # JPEG 이미지 파일을 생성하고 설정합니다.
  jpeg(file = "./data/mpg_engine_size.jpg", width = 6, height = 6, units = 'in', res = 600, bg = "white")

  # PNG 이미지 파일을 불러옵니다.
  img <- readPNG("./data/car2.png")

  # 빈 플롯을 설정합니다.
  plot(c(1, 6), c(15, 46), cex.lab = 1.3, axes = F, type = 'n', xlab = 'Engine Size', ylab = 'MPG in City')

  # 이미지를 플롯 영역에 배치합니다.
  rasterImage(img, 0.7, 14.5, 6.1, 46.0)

  # x축과 y축을 사용자 정의합니다.
  axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c('1', '2', '3', '4', '5', '6'), cex.axis = 1.2)
  axis(2, at = seq(15, 45, by = 5), labels = seq(15, 45, by = 5), cex.axis = 1.2)

  # 차량의 원산지에 따라 점을 표시합니다.
  with(subset(Cars93, Origin == 'non-USA'), points(EngineSize, MPG.city, col = 2, pch = 16))
  with(subset(Cars93, Origin == 'USA'), points(EngineSize, MPG.city, col = 4, pch = 16))

  # 범례를 추가합니다.
  legend('topright', bty = 'n', c('non-USA', 'USA'), col = c(2, 4), lwd = 2, pch = 16)

  # 설명 텍스트를 추가합니다.
  text(2.3, 35, pos = 4, 'Cars in USA have low MPGs in City, \nwhile having   large engines compared \nto non-USA.', col = 1)

  # 이미지 파일 저장을 완료하고 장치를 닫습니다.
  dev.off()

  # PDF 파일을 생성하고 설정합니다.
  pdf(file = './data/mpg_engine_size.pdf', width = 6, height = 6, bg = "white", paper = 'special')

  # 동일한 이미지 파일을 불러옵니다.
  img <- readPNG("./data/car2.png")

  # 동일한 플롯을 설정합니다.
  plot(c(1, 6), c(15, 46), cex.lab = 1.3, axes = F, type = 'n', xlab = 'Engine Size', ylab = 'MPG in City')
  rasterImage(img, 0.7, 14.5, 6.1, 46.0)
  axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c('1', '2', '3', '4', '5', '6'), cex.axis = 1.2)
  axis(2, at = seq(15, 45, by = 5), labels = seq(15, 45, by = 5), cex.axis = 1.2)

  # 동일한 점 표시와 범례를 추가합니다.
  with(subset(Cars93, Origin == 'non-USA'), points(EngineSize, MPG.city, col = 2, pch = 16))
  with(subset(Cars93, Origin == 'USA'), points(EngineSize, MPG.city, col = 4, pch = 16))
  legend('topright', bty = 'n', c('non-USA', 'USA'), col = c(2, 4), lwd = 2, pch = 16)

  # 동일한 설명 텍스트를 추가합니다.
  text(2.3, 35, pos = 4, 'Cars in USA have low MPGs in City, \nwhile having   large engines compared \nto non-USA.', col = 1)

  # PDF 파일 저장을 완료하고 장치를 닫습니다.
  dev.off()
}

# install.pacakges("ggplot2")
library(ggplot2)

# Subgroup 존재하는 경우 산점도
example7 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 산점도로 표현합니다.
  # 산점도에서는 'AirBags' 변수를 기반으로 점의 색상과 크기를 조정합니다.
  qplot(
    Horsepower, Price, data = Cars93,
    colour = AirBags,  # 'AirBags' 범주에 따라 점의 색상을 다르게 표시
    size = AirBags    # 'AirBags' 범주에 따라 점의 크기를 다르게 표시
  )
}

# Subgroup 존재하는 경우 산점도
example8 <- function() {
  # 'Cars93' 데이터셋에 'manual' 열을 추가합니다. 이 열은 'Man.trans.avail' 값에 따라 'Manual_Trans_No' 또는 'Manual_Trans_Yes' 값을 가집니다.
  Cars93$manual <- with(Cars93, ifelse(Man.trans.avail == 'No', 'Manual_Trans_No', 'Manual_Trans_Yes'))

  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 산점도로 표현하며,
  # 'manual'과 'Origin'에 따라 다른 패널로 구분하여 플롯합니다.
  with(Cars93, qplot(Horsepower, Price, data = Cars93, facets = manual ~ Origin))
}

# qplot을 이용한 확률밀도 함수 그림
example9 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Fuel.tank.capacity' 변수의 밀도를 플롯합니다.
  # 이 밀도 플롯은 차량의 'Origin'에 따라 색상이 구분되며, 투명도를 설정하여 겹치는 부분의 가시성을 높입니다.
  qplot(
    Fuel.tank.capacity, data = Cars93, geom = "density",
    fill = Origin,            # 'Origin'에 따라 다른 채움색을 사용
    alpha = I(.2),            # 투명도를 0.2로 설정
    main = "Fuel tank capacity by Origin",  # 메인 타이틀
    xlab = "Fuel tank capacity (US gallons)", # x축 레이블
    ylab = "Density"                         # y축 레이블
  ) +
    theme(plot.title = element_text(hjust = 0.5))  # 플롯 타이틀의 수평 위치 조정
}

# 회귀선 추가 산점도 1
example10 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 ggplot2를 이용해 시각화합니다.
  ggplot(Cars93, aes(x = Horsepower, y = Price)) +  # 기본 플롯 설정: x축에는 'Horsepower', y축에는 'Price'
    geom_point(shape = 16) +                        # 점 플롯 추가, 점의 모양은 shape 16
    geom_smooth(method = lm)                        # 선형 회귀선을 추가하여 데이터의 추세를 보여줌
}

# 회귀선 추가 산점도 2
example11 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 ggplot2를 이용해 시각화합니다.
  ggplot(Cars93, aes(x = Horsepower, y = Price)) +  # 기본 플롯 설정: x축에는 'Horsepower', y축에는 'Price'
    geom_point(shape = 16) +                        # 점 플롯 추가, 점의 모양은 shape 16
    geom_smooth(method = lm, se = FALSE)            # 선형 회귀선을 추가하여 데이터의 추세를 보여줌
  # se = FALSE 옵션으로 95% confidence region을 제외한다.
}

# Smoothing Line 산점도
example12 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 산점도와 스무딩 곡선으로 표현합니다.
  # 데이터는 'Origin'에 따라 색상이 다르게 표현됩니다.
  qplot(
    Horsepower, Price, data = Cars93,
    geom = c("point", "smooth"),  # 점과 스무딩 곡선 두 가지 기하 객체를 사용
    color = Origin,               # 점의 색상을 'Origin'에 따라 다르게 설정
    main = "Price vs Horsepower by Origin",  # 플롯 제목
    xlab = "Horsepower",          # x축 레이블
    ylab = "Price"               # y축 레이블
  ) +
    theme(plot.title = element_text(hjust = 0.5))  # 플롯 제목의 가로 정렬을 중앙으로 조정
}

# 연속형 변수가 추가된 산점도 - 1
example13 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 시각화합니다.
  # 점의 색상은 'Width' 변수의 값을 기반으로 하며, 색상은 노란색에서 빨간색으로 그라데이션됩니다.
  ggplot(Cars93, aes(x = Horsepower, y = Price, color = Width)) +  # 기본 플롯 설정, 색상은 'Width'에 매핑
    geom_point(shape = 16) +                                     # 점 플롯 추가, 점의 모양은 shape 16
    scale_color_gradient(low = "yellow", high = "red")           # 색상 그라데이션 설정, 낮은 값은 노란색, 높은 값은 빨간색
}

# 연속형 변수가 추가된 산점도 - 2
example14 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 시각화합니다.
  # 점의 색상은 'Width' 변수의 값에 따라 설정되며, 무지개 색상 스펙트럼을 사용하여 다양한 색상으로 표현됩니다.
  ggplot(Cars93, aes(x = Horsepower, y = Price, color = Width)) +  # 기본 플롯 설정, 색상은 'Width'에 매핑
    geom_point(shape = 16) +                                     # 점 플롯 추가, 점의 모양은 shape 16
    scale_color_gradientn(colours = rainbow(5))                  # 무지개 색상의 그라데이션을 적용, 5개의 색상 사용
}

# install.packages("RColorBrewer")
library(RColorBrewer)

# 명목형 변수가 추가되는 경우의 산점도 - 1
example15 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 시각화합니다.
  # 점의 모양과 색상은 'AirBags' 변수에 따라 설정됩니다.
  ggplot(
    Cars93,
    # 기본 플롯 설정, 모양과 색상은 'AirBags'에 매핑
    aes(x = Horsepower, y = Price, shape = AirBags, color = AirBags
    )) +
    # 점 플롯 추가, 점의 크기는 3
    geom_point(size = 3) +
    # 점의 모양을 수동으로 지정
    scale_shape_manual(values = c(16, 17, 18)) +
    # ColorBrewer 팔레트 'Dark2'를 사용하여 색상 지정
    scale_color_brewer(palette = "Dark2")
}

# 명목형 변수가 추가되는 경우의 산점도 - 2
example16 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 시각화합니다.
  # 점의 모양과 색상은 'AirBags' 변수에 따라 설정됩니다. 추가적으로 선형 회귀선을 포함합니다.
  ggplot(
    Cars93,
    # 기본 플롯 설정, 모양과 색상은 'AirBags'에 매핑
    aes(x = Horsepower, y = Price, shape = AirBags, color = AirBags)
  ) +
    # 점 플롯 추가, 점의 크기는 3
    geom_point(size = 3) +
    # 점의 모양을 수동으로 지정
    scale_shape_manual(values = c(16, 17, 18)) +
    # ColorBrewer 팔레트 'Dark2'를 사용하여 색상 지정
    scale_color_brewer(palette = "Dark2") +
    # 선형 회귀선 추가, 표준 오차 영역은 표시하지 않음
    geom_smooth(method = lm, se = FALSE)
}

# 명목형 변수가 추가되는 경우의 산점도 - 3
example17 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Horsepower'와 'Price'의 관계를 시각화합니다.
  # 점의 모양과 색상은 'AirBags' 변수에 따라 설정됩니다. 추가적으로 전체 범위에 걸친 선형 회귀선을 포함합니다.
  ggplot(
    # 기본 플롯 설정, 모양과 색상은 'AirBags'에 매핑
    Cars93, aes(x = Horsepower, y = Price, shape = AirBags, color = AirBags)
  ) +
    # 점 플롯 추가, 점의 크기는 3
    geom_point(size = 3) +
    # 점의 모양을 수동으로 지정
    scale_shape_manual(values = c(16, 17, 18)) +
    # ColorBrewer 팔레트 'Dark2'를 사용하여 색상 지정
    scale_color_brewer(palette = "Dark2") +
    # 전체 x 범위에 걸쳐 선형 회귀선 추가, 표준 오차 영역은 표시하지 않음
    geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
}
