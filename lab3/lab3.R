## Лабораторная работа №3 ----

library("stats")
library("tseries")

## 1 ----
## процесс AR(2)ARCH(3)
ar_archf23 = function(n,x0,teta,a,ret) {
  x = array(dim = n)
  ksi = rnorm(n,0,1)
  d2 = array(dim = n)
  
  d2[1] = a[1] + a[2]*x0[3]^2 + a[3]*x0[2]^2 + a[4]*x0[1]^2
  x[1] = teta[1]*x0[3] + teta[2]*x0[2] + sqrt(d2[1])*ksi[1]
  
  d2[2] = a[1] + a[2]*x[1]^2 + a[3]*x0[3]^2 + a[4]*x0[2]^2
  x[2] = teta[1]*x[1] + teta[2]*x0[3] + sqrt(d2[2])*ksi[2]
  
  d2[3] = a[1] + a[2]*x[2]^2 + a[3]*x[1]^2 + a[4]*x0[3]^2
  x[3] = teta[1]*x[2] + teta[2]*x[1] + sqrt(d2[3])*ksi[3]
  
  for (i in 4:n) {
    d2[i] = a[1] + a[2]*x[i-1]^2 + a[3]*x[i-2]^2 + a[4]*x[i-3]^2
    x[i] = teta[1]*x[i-1] + teta[2]*x[i-2] + sqrt(d2[i])*ksi[i]
  }
  if (ret == 1) return(x)
  else return(sqrt(d2))
}

n = 2100
teta = c(-0.3, 0.4)
a = c(1, 0.2, 0.1, 0.2)
x0 = c(1,0.3,0.17)

X1 = ar_archf23(n,x0,teta,a,1)
plot(X1, type='l')

## 2 Разделить выборку на обучающую и тестовую в отношении 20:1 ----
n1 = 2000; n2 = 100

X1learn = X1[1:n1]
X1test = X1[(n1+1):(n1+n2)]

## 3 Получить оценки параметров teta и a ----
arima = arima(X1learn, order=c(2,0,0), include.mean=FALSE)
teta_learn = c(arima$coef[1], arima$coef[2])
rm(arima)

h = array(dim=n1)
h[1] = X1[1] - teta_learn[1]*x0[3] - teta_learn[2]*x0[2]
h[2] = X1[2] - teta_learn[1]*X1[1] - teta_learn[2]*x0[3] 
for (i in 3:n1)
  h[i] = X1[i] - teta_learn[1]*X1[i-1] - teta_learn[2]*X1[i-2]

garch = garch(h,order=c(3,0),start=a)
a_learn = c(garch$coef[1],garch$coef[2],garch$coef[3],garch$coef[4])
rm(garch)

## 4 Построить последовательность прогнозов на один шаг ----
## на тестовой выборке

prediction = function(n, x, teta, a) {
  x_pr = array(dim=n)
  d2 = array(dim=n)
  high = array(dim=n)
  low = array(dim=n)
  
  x_pr[1] = x[1]; x_pr[2] = x[2]
  x_pr[3] = teta[1]*x[2] + teta[2]*x[1]
  
  d2[1] = a[1]
  d2[2] = a[1] + a[2]*x[1]^2
  d2[3] = a[1] + a[2]*x[2]^2 + a[3]*x[1]^2
  
  for(i in 4:n) {
    x_pr[i] = teta[1]*x[i-1] + teta[2]*x[i-2]
    d2[i] = a[1] + a[2]*x[i-1]^2 + a[3]*x[i-2]^2 + a[4]*x[i-3]^2
  }
  
  for(i in 1:n) {
    high[i] = x_pr[i] + sqrt(d2[i])
    low[i] = x_pr[i] - sqrt(d2[i])
  }
  
  plot(x, type='l', col='cyan', lwd=2)
  lines(x_pr, type='p',lwd=2)
  lines(high, type='l', col='red', lty=4)
  lines(low, type='l', col='red', lty=4)
}

prediction(n2,X1test,teta_learn,a_learn)

## 5,6 Скачать данные с сайта finam.ru и импортировать их ----
options(max.print=10000)
data = read.table("C:/Users/ivank/Documents/GitHub/emm2_labs/lab3/data.txt",sep=';',header=TRUE)
X2 = data$X.OPEN.

## 7 Построить график динамики актива ----
plot(X2, type='l')

## 8 Привести данные к стационарному виду ----
N = nrow(data)
z = array(dim=N); z[1] = 0
for(i in 2:N)
  z[i] = (X2[i]-X2[i-1])/X2[i-1]

plot(z, type='l')

## 9 Повторить шаги 2-4 для последовательности {z[n]} ----
# шаг 2
N1 = round(N/21*20); N2 = N - N1

zlearn = z[1:N1]
ztest = z[(N1+1):(N1+N2)]

# шаг 3
arima = arima(zlearn, order=c(2,0,0), include.mean=FALSE)
teta_zlearn = c(arima$coef[1], arima$coef[2])
rm(arima)

h = array(dim=N1)
h[1] = z[1]
h[2] = z[2] - teta_zlearn[1]*z[1]
for (i in 3:N1)
  h[i] = z[i] - teta_zlearn[1]*z[i-1] - teta_zlearn[2]*z[i-2]

garch = garch(h,order=c(3,0),start=c(0.05,0.2,0.5,0.2))
a_zlearn = c(garch$coef[1],garch$coef[2],garch$coef[3],garch$coef[4])
rm(garch)

# шаг 4
prediction(N2,ztest,teta_zlearn,a_zlearn)
