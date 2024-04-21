#### `union`

합집합

```r
x <- c(1,2,5)
y <- c(2,3,4,5)
union(x, y) # [1] 1 2 3 4 5
```

#### `intersect`

교집합

```r
x <- c(1,2,5)
y <- c(2,3,4,5)
intersect(x, y) # [1] 2 5
```

#### `setdiff`

차집합

```r
x <- c(1,2,5)
y <- c(2,3,4,5)
setdiff(x, y) # [1] 1
```

#### `setequal`

동일 여부 판단

```r
x <- c(1,2,5)
y <- c(2,3,4,5)
setequal(x, y) # [1] FALSE
```

#### `A %in% B`

원소 A 가 집합 B 에 속한다

```r
x <- 2
y <- c(2,3,4,5)
z <- c(1,2,3)
x %in% y # [1] TRUE
y %in% z # [1] TRUE TRUE FALSE FALSE
```

#### `choose(n, k)`

n 개의 원소로 이루어진 집합에서 추출 가능한 k 개의 원소로 이루어진 부분집합의 수를 계산

```r
choose(5, 2) # 10
```

## -

dbinom (Density of Binomial):
이항 분포의 확률 질량 함수(probability mass function, PMF)를 계산합니다. 특정한 성공 횟수에 대한 확률을 계산할 때 사용합니다. 예를 들어, n번의 시도 중에 x번 성공할 확률을 구할 때 dbinom(x, size = n, prob = p) 함수를 사용합니다.

pbinom (Probability of Binomial):
이항 분포의 누적 분포 함수(cumulative distribution function, CDF)를 계산합니다. 즉, 특정 값 이하의 확률을 구할 때 사용합니다. 예를 들어, n번의 시도 중에 x번 이하로 성공할 확률을 계산할 때 pbinom(x, size = n, prob = p) 함수를 사용합니다.

qbinom (Quantile of Binomial):
이항 분포의 분위수 함수입니다. 주어진 확률에 해당하는 이항 분포의 값(성공 횟수)을 구할 때 사용합니다. 즉, 누적 확률이 주어졌을 때, 이에 해당하는 이항 분포의 값을 찾습니다. 예를 들어, p 확률로 n번 시도 중 특정 누적 확률에 해당하는 성공 횟수를 찾을 때 qbinom(p, size = n, prob = q)를 사용합니다.

rbinom (Random Generation for Binomial):
이항 분포를 따르는 난수를 생성합니다. 시뮬레이션 또는 부트스트랩과 같은 목적으로 사용할 때 유용합니다. 예를 들어, n번의 시도에서 각 시도의 성공 확률이 p일 때의 성공 횟수를 나타내는 난수를 생성하려면 rbinom(n, size, prob = p) 함수를 사용합니다.

# Base

- pi: 파이 값

- sin(x): sin 함수
- cos(x): cosine 함수
- tan(x): tangent 함수
- asin(x): arc sin 함수
- acos(x): arc cosine 함수
- atan(x): arc tangent 함수

- log(x): 자연 로그 함수
- log10(x): 상용 로그 함수
- exp(x): 지수 로그 함수
- sqrt(x): 루트 함수

- min(x | x1, x2, ...): 벡터에서 최소값
- max(x): 벡터에서 최대값
- range(x): 벡터의 범위

- pmin(x1, x2): 두 벡터를 비교해 각 위치에서 작은 원소 추출
- pmax(x1, x2): 두 벡터를 비교해 각 위치에서 큰 원소 추출

- mean(x): 평균
- var(x): 분산
- sd(x): 표준 편차
- median(x): 중앙값
- quantile(x, p): x 벡터에서 (100*p)%에 해당하는 값
- cor(x, y): 상관 계수

# 조건문

내가 모르던 것만 기록함

## ifelse(조건문, 참, 거짓)

참일때 반환될 값, 거짓일 때 반환될 값

```r
numbers <- c(-1, 2, -3, 4, -5)
result <- ifelse(numbers < 0, 0, numbers)
print(result)
```

## switch(표현식, 케이스1=함수1, 케이스2=함수2, 케이스3=함수3)

표현식과 동일한 값에 해당하는 케이스n을 실행한다.

```r
value <- 3
result <- switch(value,
                 "1" = "First case",
                 "2" = "Second case",
                 "3" = "Third case",
                 "Default case")
print(result)
```

## for(i in a:b)

a ~ b 까지 i에 대입하면서 반복 실행

## while(조건문)

조건문이 False 가 될 때 까지 계속 반복

## repeat{ code }

블럭 내부에 `break`를 만나기 전까지 무한히 실행

```r
count <- 0

repeat {
    count <- count + 1
    print(count)

    if (count >= 5) {
        break
    }
}
```

## break / next

break: 반복문 중지
next: 자바스크립트의 continue 와 같음




















