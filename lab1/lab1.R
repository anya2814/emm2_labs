## Лабораторная работа №1: AR(p) ----

## 1 ----
arf = function(n,teta) {
  x = array(dim=n)
  ksi = rnorm(n-1, 0, 1)
  x[1] = 0
  for(i in 2:n)
    x[i] = teta*x[i-1] + ksi[i-1]
  return(x)
}
n = 100

##a)
teta1 = 0.62
ar1 = arf(n, teta1)
plot(ar1, type = "l")

##b)
teta2 = 1
ar2 = arf(n, teta2)
plot(ar2, type = "l")

##c)
teta3 = 1.1
ar3 = arf(n, teta3)
plot(ar3, type = "l")

## 2 ----
MNK_func = function(ar, n) {
  sum1 = 0
  sum2 = 0
  for (i in 2:n) {
    sum1 = sum1 + ar[i-1]*ar[i]
    sum2 = sum2 + ar[i-1]*ar[i-1]
  }
  return (sum1/sum2)
} 

MNKteta = MNK_func(ar1, n)

## 3 ----
MP_func = function(teta) {
  sum = 0
  for (i in 2:n)
    sum = sum + (ar1[i]-teta*ar1[i-1])^2
  return(sum)
}

interval = c(-2,2)
MPteta <- optimize(f = MP_func, lower = min(interval), upper = max(interval),
                   maximum = FALSE)$minimum

MNKteta
MPteta
## Вывод: МНК и МП оценки параметра тета совпадают

## 4 ----
n1 = 1000
ar4 = arf(n1, teta1)

tetaArr = array(dim = n1-9)
for (i in 10:n1)
  tetaArr[i-9] = MNK_func(ar4, i)

plot(tetaArr, type = "l")
tetaArr[991]
## Вывод: для k=10 оценка параметра тета отличается от заданного параметра 0.62 примерно на 0.15, 
## но с увеличением числа наблюдений оценка сходится к значению примерно 0.63,
## то есть отличается на 0.01.

## 5 ----
arf2 = function(n, teta1, teta2) {
  x = array(dim=n)
  ksi = rnorm(n-2, 0, 1)
  x[1] = 0
  x[2] = 1.6
  for(i in 3:n)
    x[i] = teta1*x[i-1] + teta2*x[i-2] + ksi[i-2]
  return(x)
}

ar5 = arf2(100, 0.6, 0.15)
plot(ar5, type = "l")

library("stats")
arima(ar5, order=c(2,0,0), include.mean=FALSE)
