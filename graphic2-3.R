library(ggplot2)
# install.packages("maps")
library(maps)

# csv 파일에서 한글을 못 읽는 오류 시, 적절하게 아래의 옵션을 섞는다.
# fileEncoding = "CP949" | "EUC-KR"
# fileEncoding = "UTF-8"

# 단계구분도(Choropleth Map) - 1
example1 <- function() { }

# 단계구분도(Choropleth Map) - 2
example2 <- function() { }

# 단계구분도(Choropleth Map) - 3
example3 <- function() {
  # 서울시의 '영유아보육시설' 데이터와 서울시 지도 데이터를 불러옵니다.
  young_d <- read.csv('./data/data_seoul_child.csv', header = TRUE, fileEncoding = "CP949")
  map_seoul <- read.csv('./data/mapv2_final_seoul.csv', header = TRUE)

  # '시군구명' 별로 그룹화된 목록을 생성합니다.
  pro.list <- names(table(map_seoul$시군구명))

  # 각 '시군구명'에 대한 평균 위도와 경도를 계산합니다.
  xx <- vector(); yy <- vector()
  for (jj in 1:length(pro.list)) {
    xx[jj] <- mean(subset(map_seoul, 시군구명 == pro.list[jj])$long)
    yy[jj] <- mean(subset(map_seoul, 시군구명 == pro.list[jj])$lat)
  }

  # 계산된 평균 좌표와 '시군구명'을 결합합니다.
  tab.x.y <- cbind(pro.list, xx, yy)

  # 'young_d' 데이터셋을 정렬합니다.
  data5 <- young_d[sort.int(young_d[, 1], index.return = TRUE)$ix,]

  # ggplot을 사용하여 지도에 '영유아보육시설' 데이터를 시각화합니다.
  ggplot(young_d, aes(map_id = region, fill = 영유아보육시설)) +
    geom_map(map = map_seoul, alpha = 0.3, colour = 'white', size = 0.1) +  # 지도를 그리고 각 구역을 구분
    theme(legend.position = c(0.1, 0.8)) +  # 범례의 위치를 조정
    scale_fill_gradientn(colours = c('yellow', 'red')) +  # 색상 그라데이션 설정
    expand_limits(x = map_seoul$long, y = map_seoul$lat) +  # x, y 축 한계 설정
    coord_fixed() +  # 고정된 종횡비 유지
    labs(x = "", y = "", title = "영유아보육시설") +  # 레이블과 타이틀 설정
    theme(plot.title = element_text(hjust = 0.5)) +  # 제목의 위치 조정
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +  # y축 눈금과 텍스트 제거
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +  # x축 눈금과 텍스트 제거
    geom_text(x = xx, y = yy + 400, label = data5$영유아보육시설, size = 3, col = 4) +  # 위도 위치에 따라 텍스트 추가
    geom_text(x = xx, y = yy - 600, label = pro.list, size = 3, col = 1)  # 위도 위치 조정하여 텍스트 추가
}

# install.packages("igraph")
library(igraph)

# 네트워크 그림 - Undirected vs Directed
example4 <- function() {
  # R의 그래픽 디바이스를 두 개의 화면으로 분할합니다. 이 구성은 1행 2열로 설정됩니다.
  split.screen(figs = c(1, 2))

  # 첫 번째 화면을 활성화합니다.
  screen(1)
  # 비방향성 그래프를 생성하고, 플롯합니다. 노드는 3개, 간선은 1-2, 2-3, 3-1입니다.
  g1 <- graph(edges = c(1, 2, 2, 3, 3, 1), n = 3, directed = FALSE)
  plot(g1)

  # 두 번째 화면을 활성화합니다.
  screen(2)
  # 방향성 그래프를 생성하고, 플롯합니다. 동일한 노드와 간선 구조를 사용하지만 간선에 방향이 있습니다.
  g1 <- graph(edges = c(1, 2, 2, 3, 3, 1), n = 3, directed = TRUE)
  plot(g1)
}

# 네트워크 그림 - Isolated points
example5 <- function() {
  # 화면을 1행 2열로 분할합니다.
  split.screen(figs = c(1, 2))

  # 첫 번째 스크린을 활성화합니다.
  screen(1)
  # 비방향성 그래프를 생성하고 플롯합니다. 노드는 3개, 간선은 1-2, 2-3, 3-1, 1-3입니다.
  g1 <- graph(edges = c(1, 2, 2, 3, 3, 1, 1, 3), n = 3)
  plot(g1, edge.arrow.size = 0.5)  # 간선의 화살표 크기를 0.5로 설정

  # 두 번째 스크린을 활성화합니다.
  screen(2)
  # 비방향성 그래프를 생성하고 플롯합니다. 노드는 7개, 간선은 1-2, 2-3, 3-1입니다.
  g2 <- graph(edges = c(1, 2, 2, 3, 3, 1), n = 7)
  plot(g2, edge.arrow.size = 0.5)  # 간선의 화살표 크기를 0.5로 설정
}

# 네트워크 그림 - Vertex 이름
example6 <- function() {
  # 문자열 벡터를 사용하여 그래프 객체 'g3'를 생성합니다. 이 그래프는 지정된 도시 간의 연결을 나타냅니다.
  # 'Seoul'은 'Pusan'과, 'Pusan'은 'Gwangju'와, 'Gwangju'는 'Seoul'과 연결됩니다.
  g3 <- graph(c("Seoul", "Pusan", "Pusan", "Gwangju", "Gwangju", "Seoul"))

  # 생성된 그래프 'g3'를 시각화합니다.
  plot(g3)
}

g4 <- graph(c("Seoul", "Pusan", "Pusan", "Gwangju", "Gwangju", "Seoul", "Seoul",
              "Daegu", "Seoul", "Deajeon"), isolates = c("Sejong", "Ulsan"))

# 네트워크 그림 - Edge network
example7 <- function() {
  # 그래프 'g4'를 사용자 정의 스타일로 플로팅합니다. 다양한 시각적 속성을 조정하여 그래프의 모양을 개선합니다.
  plot(
    g4,
    edge.arrow.size = 1.5,         # 간선의 화살표 크기를 1.5로 설정
    vertex.color = "gold",         # 정점의 색상을 'gold'로 설정
    vertex.size = 15,              # 정점의 크기를 15로 설정
    vertex.frame.color = "gray",   # 정점의 테두리 색상을 'gray'로 설정
    vertex.label.color = "black",  # 정점 레이블의 색상을 'black'으로 설정
    vertex.label.cex = 1.2,        # 정점 레이블의 글자 크기를 1.2배로 조정
    vertex.label.dist = 2,         # 정점 레이블과 정점 사이의 거리를 2로 설정
    edge.curved = 0.2              # 간선의 곡률을 0.2로 설정
  )
}

# 네트워크 그림 - Edge & Vertex
example8 <- function() {
  # 그래프 'g4'의 모든 간선을 반환합니다.
  E(g4)

  # 그래프 'g4'의 모든 정점의 이름을 반환합니다.
  V(g4)$name

  # 그래프 'g4'의 각 정점에 'Type' 속성을 할당합니다.
  V(g4)$Type <- c("Special", "Metropolitan", "Metropolitan", "Metropolitan", "Metropolitan", "Multi-functional Administrative", "Metropolitan")

  # 그래프 'g4'의 각 정점에 'Pop' (인구) 속성을 할당합니다.
  V(g4)$Pop <- c(9.7, 3.4, 5.5, 2.4, 1.5, 4.27, 2.1)

  # 그래프 'g4'의 각 간선에 'traffic_volume' 속성을 할당합니다.
  E(g4)$traffic_volume <- c(2.8, 4.5, 8.7, 5.64, 4.9)

  # 그래프 'g4'의 각 간선에 교통 수단 유형을 나타내는 'traffic' 속성을 할당합니다.
  E(g4)$traffic <- c('train', 'plane', 'train', 'highway', 'highway')

  # 그래프 'g4'의 각 간선에 용도를 나타내는 'ttype' 속성을 할당합니다.
  E(g4)$ttype <- c('business', 'business', 'business', 'travel', 'travel')

  # 그래프 'g4'를 사용자 정의 스타일로 플로팅합니다.
  plot(
    g4,
    edge.arrow.size = 1.5,         # 간선의 화살표 크기를 1.5로 설정
    vertex.label.color = "black",  # 정점 레이블의 색상을 'black'으로 설정
    vertex.label.dist = 2,         # 정점 레이블과 정점 사이의 거리를 2로 설정
    vertex.color = c("pink", rep("skyblue", 4), "peru", "skyblue"), # 정점의 색상 설정
    edge.curved = 0.2              # 간선의 곡률을 0.2로 설정
  )
}

# 네트워크 그림 - Edge curve
example9 <- function() {
  color <- c("pink", rep("skyblue", 4), "peru", "skyblue")
  curved <- c(0.1, 0.9, 0.3, 0.4, 0.1)
  # 사전에 정의된 색상과 곡률 설정을 사용하여 그래프 'g4'를 플로팅합니다.
  # 색상과 곡률 값은 각각 정점과 간선에 적용됩니다.
  plot(
    g4,
    edge.arrow.size = 1.5,               # 간선의 화살표 크기를 1.5로 설정
    vertex.label.color = "black",        # 정점 레이블의 색상을 'black'으로 설정
    vertex.label.dist = 2,               # 정점 레이블과 정점 사이의 거리를 2로 설정
    vertex.color = color,                # 정점의 색상을 'color' 벡터에 따라 설정
    edge.curved = curved                 # 간선의 곡률을 'curved' 벡터에 따라 설정
  )
}

# 네트워크 그림 - Vertex size
example10 <- function() {
  color <- c("pink", rep("skyblue", 4), "peru", "skyblue")
  # 그래프 'g4'를 사용자 정의 스타일로 플로팅합니다. 이때 정점과 간선에 다양한 시각적 속성을 적용합니다.
  plot(
    g4,
    edge.arrow.size = 1.5,                # 간선의 화살표 크기를 1.5로 설정
    vertex.size = 30,                     # 정점의 크기를 30으로 설정
    vertex.frame.color = "gray",          # 정점의 테두리 색상을 'gray'로 설정
    vertex.label.color = "black",         # 정점 레이블의 색상을 'black'으로 설정
    vertex.label.cex = 1.2,               # 정점 레이블의 글자 크기를 1.2배로 조정
    vertex.label.dist = 3.5,              # 정점 레이블과 정점 사이의 거리를 3.5로 설정
    edge.curved = 0.2,                    # 간선의 곡률을 0.2로 설정
    vertex.color = color                  # 정점의 색상을 사전 정의된 'color' 벡터에 따라 설정
  )
}

te <- c('train', 'plane', 'train', 'highway', 'highway')
line.col <- ifelse(te == 'train', 1, ifelse(te == 'plane', 2, 3))
colrs <- c("black", "maroon", "blue")

# 네트워크 그림 - Line color
example11 <- function() {
  # 그래프 'g4'를 사용자 정의 스타일로 플로팅합니다. 이때 정점과 간선에 다양한 시각적 속성을 적용합니다.
  plot(
    g4,
    edge.color = colrs[line.col],        # 간선의 색상을 'colrs' 배열에서 'line.col' 인덱스에 따라 설정
    # 아래 코드 애러 나서 주석 처리함
    # vertex.size = V(g4)$Pop * 4,         # 정점의 크기를 각 정점의 'Pop' 속성 값에 4를 곱한 값으로 설정
    vertex.frame.color = "gray",         # 정점의 테두리 색상을 'gray'로 설정
    vertex.label.color = "black",        # 정점 레이블의 색상을 'black'으로 설정
    vertex.label.cex = 1.2,              # 정점 레이블의 글자 크기를 1.2배로 조정
    vertex.label.dist = 3.5,             # 정점 레이블과 정점 사이의 거리를 3.5로 설정
    edge.curved = 0.2,                   # 간선의 곡률을 0.2로 설정
    vertex.color = c("pink", rep("skyblue", 4), "peru", "skyblue")  # 정점의 색상을 사전 정의된 색상 배열에 따라 설정
  )
}

# 네트워크 그림 - Legend 추가
example12 <- function() {
  line.curve <- c(0.1, 0.9, 0.3, 0.4, 0.1)
  mycolrs <- c("gold", rep("tomato", 4), "lightpink", "tomato")
  # 그래프 'g4'를 사용자 정의 스타일로 플로팅합니다. 이때 정점과 간선에 다양한 시각적 속성을 적용합니다.
  plot(
    g4,
    edge.color = colrs[line.col],        # 간선의 색상을 'colrs' 배열에서 'line.col' 인덱스에 따라 설정
    # 아래 코드 애러 나서 주석 처리함
    # vertex.size = V(g4)$Pop * 6,         # 정점의 크기를 각 정점의 'Pop' 속성 값에 6을 곱한 값으로 설정
    vertex.frame.color = "gray",         # 정점의 테두리 색상을 'gray'로 설정
    vertex.label.color = "black",        # 정점 레이블의 색상을 'black'으로 설정
    vertex.label.cex = 1.2,              # 정점 레이블의 글자 크기를 1.2배로 조정
    edge.curved = line.curve,            # 간선의 곡률을 'line.curve' 벡터에 따라 설정
    vertex.color = mycolrs               # 정점의 색상을 'mycolrs' 벡터에 따라 설정
  )

  # 첫 번째 범례를 추가합니다. 범례는 도시 유형에 대한 설명을 포함합니다.
  legend(
    'topleft',                           # 범례의 위치를 그래프의 상단 왼쪽에 배치
    c("Special", "Metropolitan", "Metropolitan Autonomous"),  # 범례에 표시할 텍스트
    pch = 21,                            # 범례에 사용될 포인트 형태
    pt.bg = c("gold", "tomato", "lightpink"),  # 포인트의 배경색
    pt.cex = 2,                          # 포인트 크기 조정
    bty = "n",                           # 범례 상자 없음
    ncol = 1                             # 범례 열 수
  )

  # 두 번째 범례를 추가합니다. 범례는 교통 수단 유형에 대한 설명을 포함합니다.
  legend(
    x = -1.0, y = -1.2,                 # 범례의 위치를 지정 (x, y 좌표)
    c('train', 'plane', 'highway'),     # 범례에 표시할 텍스트
    lty = 1,                            # 범례에 사용될 선의 유형
    lwd = 2,                            # 선의 두께
    col = colrs,                        # 선의 색상
    bty = "n",                          # 범례 상자 없음
    ncol = 3                            # 범례 열 수
  )
}

example12()
