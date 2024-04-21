library(MASS)
# head(Cars93)

# 상자그림 (범주별 그림)
example1 <- function() {
  # Cars93 데이터셋 내에서 'Min.Price' 변수와 'AirBags' 변수 사이의 관계를 박스 플롯으로 시각화합니다.
  # 'Min.Price'는 y축에, 'AirBags'의 각 범주(카테고리)는 x축에 표시됩니다.
  boxplot(Min.Price ~ AirBags, data = Cars93)
}

# 상자그림 개별값 확인
example2 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'Min.Price' (최소 가격)와 'AirBags' (에어백 수) 간의 박스 플롯을 생성합니다.
  # 이 때, 박스 플롯 객체를 생성하고 바로 통계적 요약 정보를 추출합니다.
  # '$stats'는 생성된 박스 플롯 객체에서 통계적 요약을 포함하는 부분을 선택합니다.

  boxplot(Min.Price ~ AirBags, data = Cars93)$stats
}

# Driver only 그룹만 추출하여 분포 살펴보기
example3 <- function() {
  # 선택된 'Min.Price' 데이터에 대한 요약 통계를 계산합니다.
  # summary 함수는 최소값, 최대값, 중앙값, 평균, 제1사분위수, 제3사분위수를 포함한 통계적 요약을 콘솔로 제공합니다.
  summary(subset(Cars93, AirBags == 'Driver only')$Min.Price)
}

# 색깔로 구분하여 상자그림 그리기
example4 <- function() {
  boxplot(
    data = Cars93,                                           # 'Cars93' 데이터에서
    Min.Price ~ AirBags,                                     # 'Min.Price'와 'AirBags' 변수에 대한 박스 플롯을 생성합니다.
    names = c("Driver & Passenger", "Driver only", "None"),  # x축의 카테고리 이름을 지정합니다.
    col = c("orange", "cyan", 'yellow'),                     # 각 박스의 색상을 지정합니다.
    ylab = "Minimum Price",                                  # y축 레이블을 'Minimum Price'로 설정합니다.
    xlab = "Airbag",                                         # x축 레이블을 'Airbag'으로 설정합니다.
    ylim = c(0, 50),                                         # y축의 범위를 0에서 50으로 설정합니다.
    boxwex = 0.25                                            # 박스의 너비를 기본값 대비 0.25로 조정합니다.
  )
}

# 상자그림 순서 변경
example5 <- function() {
  # 박스 플롯의 위치, 색상, 레이블 및 기타 시각적 요소들을 상세하게 설정합니다.
  boxplot(
    data = Cars93,                                          # 'Cars93' 데이터셋에서
    Min.Price ~ AirBags,                                    # 'Min.Price'와 'AirBags' 변수에 대한 박스 플롯을 생성합니다.
    at = c(3, 2, 1),                                        # 박스의 x축 위치를 명시적으로 지정합니다.
    names = c("Driver & Passenger", "Driver only", "None"), # x축의 각 박스에 대한 이름을 설정합니다.
    col = c("orange", "cyan", 'yellow'),                    # 각 박스의 색상을 지정합니다.
    ylab = "Minimum Price",                                 # y축 레이블을 'Minimum Price'로 설정합니다.
    xlab = "Airbag",                                        # x축 레이블을 'Airbag'으로 설정합니다.
    ylim = c(0, 50),                                        # y축의 범위를 0에서 50으로 설정합니다.
    boxwex = 0.25                                           # 박스의 너비를 기본값 대비 0.25로 조정합니다.
  )
}

# ggplot2 패키지
# install.packages("ggplot2")
library(ggplot2)

# 실제 데이터 관측치를 포함한 상자그림
example6 <- function() {
  # ggplot2 패키지의 qplot 함수를 사용하여 복합 그래프를 생성합니다.
  qplot(
    AirBags, Min.Price, data = Cars93,
    geom = c("boxplot", "jitter"), # 사용할 기하 객체(geom)를 박스 플롯과 지터(점 플롯)로 지정합니다.
    fill = AirBags,                # 박스 플롯의 채움색을 'AirBags' 변수의 범주에 따라 다르게 설정합니다.
    ylab = "Minimum Price",        # y축 레이블을 'Minimum Price'로 설정합니다.
    xlab = "Airbags",              # x축 레이블을 'Airbags'로 설정합니다.
    alpha = I(.2)               # 채움색의 투명도를 0.2로 설정합니다. 'I' 함수는 리터럴 값을 강제로 인용합니다.
  )
}

# ggplot 함수를 이용하여 상자그림 그리기 + 연산자의 활용
example7 <- function() {
  # ggplot 객체를 생성합니다. 'Cars93' 데이터셋과 aes 함수를 사용해 시각화합니다.
  p <- ggplot(Cars93, aes(x = AirBags, y = Min.Price)) +
    geom_boxplot(aes(fill = AirBags)) + # 박스 플롯을 추가하고, 'AirBags' 범주에 따라 박스의 채움색을 지정합니다.
    scale_fill_viridis_d()              # 'viridis' 팔레트에서 불연속 색상 스케일을 사용해 채움색을 지정합니다.

  # 범례를 생략하기 위해 theme 함수를 사용하고, 축 레이블을 설정합니다.
  p +
    theme(legend.position = "none") + # 범례의 위치를 'none'으로 설정하여 범례를 표시하지 않습니다.
    xlab("Airbags") +            # x축 레이블을 'Airbags'로 설정합니다.
    ylab("Minimum Price")        # y축 레이블을 'Minimum Price'로 설정합니다.
}

#points, symbols representing the raw data (jittered horizontally)
# bar, a vertical bar showing central tendencies
# bean, a smoothed density (inspired by Kampstra and others (2008)) representing a smoothed density
# inf, a rectangle representing an inference interval (e.g.; Bayesian Highest Density Interval or frequentist confidence interval)

# install.packages("yarrr")
library(yarrr)

# pirate plot
example8 <- function() {
  # 'Cars93' 데이터셋을 사용하여 'MPG.city'에 대한 'Origin' 및 'DriveTrain'별로 pirateplot을 생성합니다.
  pirateplot(
    formula = MPG.city ~ Origin + DriveTrain,    # formula를 사용하여 독립 변수 'Origin'과 'DriveTrain'에 따른 'MPG.city'의 종속 변수를 지정합니다.
    point.o = 0.5,                               # 포인트의 투명도를 0.5로 설정합니다. 이는 점의 시각적 중첩을 줄이면서도 데이터 포인트를 볼 수 있게 합니다.
    data = Cars93,                               # 데이터로 'Cars93' 데이터셋을 사용합니다.
    main = "City MPG by Origin and Drive Train", # 그래프의 주 제목을 설정합니다.
    inf.method = 'iqr'                           # 이상치를 결정하는 방법으로 IQR(사분위수 범위)를 사용합니다.
  )
}

# 그룹별 밀도함수
example9 <- function() {
  # ggplot 객체를 생성합니다. 'Cars93' 데이터셋과 aes 함수를 사용해 x축에 'MPG.highway' 변수를 지정합니다.
  ggplot(Cars93, aes(x = MPG.highway)) +
    # geom_density를 사용하여 밀도 플롯을 추가합니다.
    geom_density(aes(group = Type, colour = Type)) + # 'Type' 변수를 기반으로 그룹화하고 색상을 다르게 지정합니다.
    labs(x = "MPG.highway", y = "Density") +       # x축과 y축에 레이블을 지정합니다.
    ggtitle("Density of MPG in Highway by Type") + # 그래프의 제목을 설정합니다.
    theme(plot.title = element_text(hjust = 0.5))  # 그래프 제목의 수평 정렬을 가운데로 조정합니다.
}

# theme_bw() 로 배경색 제거
example10 <- function() {
  # ggplot 객체를 초기화하고 'Cars93' 데이터셋을 사용합니다. x축에는 'MPG.highway'를 설정합니다.
  ggplot(Cars93, aes(x = MPG.highway)) +
    theme_bw() +  # 검은색과 흰색을 기반으로 한 테마를 설정하여 시각적 클린함을 제공합니다.
    geom_density(aes(group = Type, colour = Type)) +  # geom_density를 사용하여 밀도 플롯을 추가하고,
    # 'Type'별로 그룹화 및 색상 적용하여 각 차량 유형의 밀도를 나타냅니다.
    labs(x = "MPG.highway", y = "Density") +  # x축과 y축의 레이블을 지정하여 축의 의미를 명확히 합니다.
    ggtitle("Density of MPG in Highway by Type") +  # 플롯의 제목을 설정합니다.
    theme(plot.title = element_text(hjust = 0.5))  # 제목의 위치를 플롯 중앙으로 조정합니다.
}

# gridExtra 패키지 사용하여 다중 그림 그리기
# install.packages("gridExtra")
library(gridExtra)

# 호흡곡선 Spinogram
example11 <- function() {
  p1 <- ggplot(Cars93, aes(x = MPG.highway)) +
    theme_bw() +
    geom_density(aes(group = Type, colour = Type)) +
    labs(x = "MPG.highway", y = "Density") +
    ggtitle("Density of MPG in Highway by Type") +
    theme(plot.title = element_text(hjust = 0.5))
  p2 <- ggplot(Cars93, aes(x = MPG.highway)) +
    theme_bw() +
    geom_density(aes(group = Origin, colour = Origin)) +
    labs(x = "MPG.highway", y = "Density") +
    ggtitle("Density of MPG in Highway by Origin") +
    theme(plot.title = element_text(hjust = 0.5))

  grid.arrange(p1, p2, ncol = 2)
}

# install.packages("vcd")
library(vcd)

# 나이 사분위 구간에 따라 분포 none, some, marked : spine 함수는 vcd 패키지 필요
example12 <- function() {
  # 'Arthritis' 데이터셋을 사용하며, 'spine' 플롯을 생성합니다.
  with(
    Arthritis,
    spine(
      Improved ~ Age,                # 'Improved'를 종속 변수, 'Age'를 독립 변수로 설정합니다.
      breaks = quantile(Age)       # 'Age' 변수의 분위수를 기준으로 데이터를 나눕니다.
    )
  )
}

# 세분화
example13 <- function() {
  # 'Arthritis' 데이터셋에서 'Improved' 상태에 대해 'Age'를 독립 변수로 사용하여 스파인 플롯을 생성합니다.
  spine(
    Improved ~ Age,
    data = Arthritis,                  # 데이터셋을 명시합니다.
    breaks = 'Scott'                  # 데이터를 나누는 기준으로 Scott의 규칙을 사용합니다.
  )
}

# 조건부 밀도함수 (기본 graphics)
example14 <- function() {
  # Arthritis 데이터셋을 사용하여 'Improved'의 조건부 밀도를 'Age'에 따라 시각화합니다.
  cdplot(
    Improved ~ Age,
    data = Arthritis   # 데이터셋을 지정합니다.
  )
}

# 실제값 추가 (rug 함수 : 기본 graphics)
example15 <- function() {
  # 'Arthritis' 데이터셋을 사용하여 'Age' 변수에 대한 러그 플롯을 생성합니다.
  with(
    Arthritis,
    rug(
      jitter(Age),       # 'Age' 변수에 작은 무작위성(jitter)을 추가하여 데이터 포인트들이 겹치지 않게 합니다.
      col = 'white',     # 러그 플롯의 색상을 흰색으로 설정합니다.
      quiet = TRUE     # 추가적인 메시지 출력을 억제합니다.
    )
  )
}
