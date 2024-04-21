# MASS 패키지를 불러옵니다. 이 패키지에는 Cars93 데이터셋이 포함되어 있습니다.
library(MASS)
# 콘솔에서 Cars93의 데이터 구조가 어떻게 되어있는지 몇 줄 출력
head(Cars93)

#####
# 단변수 범주형 자료
#####

# 기본 막대 그림
example1 <- function() {
  # Cars93 데이터셋의 'Type' 열을 사용하여 데이터의 유형별로 빈도수를 계산합니다.
  # 'with' 함수는 특정 데이터 프레임 내에서 표현식을 평가하는 데 사용됩니다.
  (tab <- with(Cars93, table(Type)))

  # 막대 그래프를 그립니다.
  barplot(
    tab,  # 'tab'은 유형별 자동차 수를 포함하고 있는 테이블입니다.
    main = 'Type of Car',  # 그래프의 제목을 설정합니다.
    xlab = 'Type',  # x축 레이블을 'Type'으로 설정합니다.
    ylab = 'Number of Car',  # y축 레이블을 'Number of Car'로 설정합니다.
    col = 1:6,  # 막대의 색상을 1부터 6까지의 색으로 설정합니다.

    # 범례를 추가합니다. 'legend'는 각 막대가 나타내는 자동차 유형의 이름을 지정합니다.
    legend = c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'),

    # 'names.arg'는 x축에 표시될 각 막대의 레이블을 설정합니다.
    # 이 예에서는 실제 데이터의 'Type' 값을 사용자가 읽기 쉬운 형태로 조정합니다.
    names.arg = c('compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'),
    # side형 막대그림 옵션
    beside = TRUE,
  )
}

# side형 막대그림인 경우
example2 <- function() {
  # Cars93 데이터셋의 'Type'과 'AirBags' 열을 사용하여 크로스 탭(cross tabulation)을 생성합니다.
  # 'xtabs' 함수는 수식을 기반으로 테이블을 생성합니다. 여기서는 Type과 AirBags 간의 조합에 따라 데이터를 집계합니다.
  (tab <- with(Cars93, xtabs(~Type + AirBags)))

  # 막대 그래프를 그립니다.
  barplot(
    tab,  # 'tab'은 Type과 AirBags에 따른 집계된 데이터를 포함하고 있습니다.
    col = rainbow(6),  # 막대의 색상을 무지개 색상 6개로 설정합니다.

    # 범례를 추가합니다. 'legend'는 각 막대 그룹이 나타내는 자동차 유형의 이름을 지정합니다.
    legend = c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'),

    xlab = 'AirBags',  # x축 레이블을 'AirBags'로 설정합니다.
    ylab = 'Number of Cars',  # y축 레이블을 'Number of Cars'로 설정합니다.

    beside = TRUE,  # 'beside = TRUE'는 각 자동차 유형별로 에어백의 유무에 따른 막대들을 옆으로 나란히 배치합니다.
  )
}

# stacked 막대그림
example3 <- function() {
  # Cars93 데이터셋의 'Type'과 'AirBags' 열을 사용하여 크로스 탭(cross tabulation)을 생성합니다.
  # 'xtabs' 함수는 수식을 기반으로 테이블을 생성합니다. 여기서는 Type과 AirBags 간의 조합에 따라 데이터를 집계합니다.
  (tab <- with(Cars93, xtabs(~Type + AirBags)))

  # 'barplot' 함수를 사용하여 막대 그래프를 그립니다.
  barplot(
    tab,  # 'tab'은 각 자동차 유형과 에어백 유무에 따라 집계된 데이터를 포함하고 있습니다.
    col = rainbow(6),  # 막대의 색상을 무지개 색상 6개로 설정합니다.

    # 범례를 추가합니다. 'legend'는 각 색상이 나타내는 자동차 유형의 이름을 지정합니다.
    legend = c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'),

    xlim = c(0, ncol(tab) + 2),  # x축의 범위를 설정합니다. 그래프에 여유 공간을 추가하기 위해 범위를 조정합니다.
    xlab = 'AirBags',  # x축 레이블을 'AirBags'로 설정합니다.
    ylab = 'Number of Cars',  # y축 레이블을 'Number of Cars'로 설정합니다.

    # 'args.legend'는 범례의 위치를 지정합니다.
    # 여기서는 x 위치를 그래프의 마지막 막대 오른쪽에, y 위치를 그래프의 최대 높이에 맞춥니다.
    args.legend = list(x = ncol(tab) + 2, y = max(colSums(tab))),

    bty = 'n'  # 그래프 주변의 박스 타입을 없애는 설정입니다.
  )
}

# 파이차트 1
example4 <- function() {
  # 'tab' 변수에 Cars93 데이터셋의 'Type' 열을 기반으로 한 빈도수 테이블을 생성합니다.
  # 'with' 함수는 Cars93 데이터셋 내에서 특정 작업을 수행하기 위해 사용됩니다.
  tab <- with(Cars93, table(Type))

  # 파이 차트를 그립니다.
  pie(
    tab,  # 파이 차트에 사용될 데이터. 여기서는 자동차 유형별 빈도수입니다.
    col = topo.colors(6)  # 파이 차트의 각 조각에 색을 지정합니다. 'topo.colors' 함수는 지형을 표현하는 데 적합한 색상을 제공합니다.
  )
}

# 파이차트 2
example5 <- function() {
  # 'tab' 변수에 Cars93 데이터셋의 'Type' 열을 기반으로 한 빈도수 테이블을 생성합니다.
  # 'with' 함수는 Cars93 데이터셋 내에서 특정 작업을 수행하기 위해 사용됩니다.
  tab <- with(Cars93, table(Type))
  # 'names' 함수를 사용하여 'tab'의 각 요소(자동차 유형)에 더 읽기 쉬운 이름을 지정합니다.
  # 이 이름들은 파이 차트에서 각 조각의 레이블로 사용됩니다.
  names(tab) <- c('경형', '대형', '중형', '소형', '스포티', '벤')

  # 파이 차트를 그립니다.
  pie(
    tab,  # 파이 차트에 사용될 데이터. 여기서는 자동차 유형별 빈도수입니다.
    col = topo.colors(6)  # 파이 차트의 각 조각에 색을 지정합니다. 'topo.colors' 함수는 지형을 표현하는 데 적합한 색상을 제공합니다.
  )
}

#####
# 단변수 연속형 자료
#####

# 히스토그램
example6 <- function() {
  # 'with' 함수를 사용하여 'Cars93' 데이터 프레임 내에서 특정 코드를 실행합니다.
  # 이는 데이터 프레임에 있는 변수들을 직접 호출하여 사용할 수 있게 해줍니다.
  with(
    Cars93,  # 'Cars93' 데이터 프레임을 사용합니다.

    # 'hist' 함수로 'MPG.highway' 데이터의 히스토그램을 그립니다.
    # 히스토그램은 데이터의 분포를 시각적으로 보여줍니다.
    hist(
      MPG.highway,  # 'MPG.highway' 열을 히스토그램의 데이터로 사용합니다.
      xlab = 'MPG in Highway',  # x축 레이블을 'MPG in Highway'로 설정합니다.
      main = 'MPG in Highway'  # 그래프의 주제를 'MPG in Highway'로 설정합니다.
    )
  )
}

# 'vcd' 라이브러리를 불러옵니다.
# 이 라이브러리는 범주형 데이터의 시각화와 관련된 기능을 제공하지만, 여기서는 특별히 사용된 기능은 없습니다.
library(vcd)

# 확률밀도함수 그림(Density Plot) 1
example7 <- function() {
  # 'with' 함수를 사용하여 'Arthritis' 데이터셋 내에서 'Age'의 밀도 추정 그래프를 그립니다.
  with(
    Arthritis,  # 'Arthritis' 데이터셋을 사용합니다.

    # 'plot' 함수를 사용하여 'density' 함수로 계산된 'Age' 데이터의 밀도 추정치를 그래프로 표현합니다.
    plot(density(Age))  # 'density' 함수는 'Age' 데이터의 연속적인 확률 분포 추정을 수행하고, 'plot' 함수로 그 결과를 시각화합니다.
  )
}

# 확률밀도함수 그림 2
example8 <- function() {
  # 고속도로 연비 데이터에 대한 히스토그램과 밀도 추정선을 그립니다.
  with(
    Cars93,
  {
    # 히스토그램을 그립니다. 이 때, probability = TRUE로 설정하여 확률 밀도로 표시합니다.
    hist(MPG.highway, xlab = 'MPG in Highway', main = 'MPG in Highway', probability = TRUE)

    # 밀도 추정 결과를 계산하고 이를 그래프 위에 라인으로 표시합니다.
    # 여기서 선의 색상과 두께를 지정합니다.
    lines(density(MPG.highway), col = 'red', lwd = 2)
  }
  )
}

# Quantile-Quantile (QQ) Plot
example9 <- function() {
  # 'with' 함수를 사용하여 'Cars93' 데이터셋 내에서 코드 블록을 실행합니다.
  # 이를 통해 데이터셋의 'Turn.circle' 열에 직접 접근하여 작업을 수행할 수 있습니다.
  with(
    Cars93,
  {
    # 'qqnorm' 함수를 사용하여 'Turn.circle'의 Q-Q 플롯을 생성합니다.
    # Q-Q 플롯은 표본 데이터의 분위수와 정규 분포의 이론적 분위수를 비교하여 그래프로 나타냅니다.
    # 이 그래프를 통해 데이터가 정규 분포를 따르는지 여부를 시각적으로 판단할 수 있습니다.
    qqnorm(Turn.circle, main = 'Q-Q plot of Turn.circle\n (U-turn space, feet)')

    # 'qqline' 함수를 사용하여 Q-Q 플롯에 참조선을 추가합니다.
    # 이 선은 이론적인 정규 분포를 나타내며, 데이터 포인트들이 이 선에 가깝게 위치할수록
    # 데이터가 정규 분포에 잘 부합함을 의미합니다.
    qqline(Turn.circle, col = 'orange', lwd = 2)
  }
  )
}
