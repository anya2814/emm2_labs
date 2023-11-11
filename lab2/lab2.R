## Лабораторная работа №2 ----

## 1 ----
## процесс GARCH(1,0)
garchf10 = function(n,h0,a0,a1,ret) {
  h = array(dim = n)
  ksi = rnorm(n,0,1)
  d2 = array(dim = n)
  
  d2[1] = a0 + a1*h0^2
  h[1] = sqrt(d2[1])*ksi[1]
  
  for (i in 2:n) {
    d2[i] = a0 + a1*h[i-1]^2
    h[i] = sqrt(d2[i])*ksi[i]
  }
  if (ret == 1) return(h)
  else return(sqrt(d2))
}

h0 = 1
n = 1000
a0 = 1
a1 = 0.4

h1 = garchf10(n,h0,a0,a1,1)
d1 = garchf10(n,h0,a0,a1,0)

## график h
plot(h1, type = 'l')
## график волатильности
plot(d1, type = 'l')

## 2 ----
## МНК-оценка a0
MNKfa0 = function(n,h,a1) {
  sum = 0
  for (i in 2:n)
    sum = sum + (h[i]^2 - a1*h[i-1]^2)
  return(sum/n)
}

MNKa0 = MNKfa0(n,h1,a1)
MNKa0

## МНК-оценка a1
MNKfa1 = function(n,h,a0) {
  sum1 = 0
  sum2 = 0
  for (i in 2:n) {
    sum1 = sum1 + h[i-1]^2*(h[i]^2-a0)
    sum2 = sum2 + h[i-1]^4
  }
  return (sum1/sum2)
}

MNKa1 = MNKfa1(n,h1,a0)

## 3 ----
library("tseries")
garch(h1,order=c(1,0),start=c(a0,a1))

## 4 ----
## процесс GARCH(3,0)
garchf30 = function(n,h0,a,ret) {
  h = array(dim = n)
  ksi = rnorm(n,0,1)
  d2 = array(dim = n)
  
  d2[1] = a[1] + a[2]*h0[3]^2 + a[3]*h0[2]^2 + a[4]*h0[1]^2
  h[1] = sqrt(d2[1])*ksi[1]
  
  d2[2] = a[1] + a[2]*h[1]^2 + a[3]*h0[3]^2 + a[4]*h0[2]^2
  h[2] = sqrt(d2[2])*ksi[2]
  
  d2[3] = a[1] + a[2]*h[2]^2 + a[3]*h[1]^2 + a[4]*h0[3]^2
  h[3] = sqrt(d2[3])*ksi[3]
  
  for (i in 4:n) {
    d2[i] = a[1] + a[2]*h[i-1]^2 + a[3]*h[i-2]^2 + a[4]*h[i-3]^2
    h[i] = sqrt(d2[i])*ksi[i]
  }
  if (ret == 1) return(h)
  else return(sqrt(d2))
}

n = 1100
a = c(1,0.6,0.2,0.1)
h0 = c(1.3,0.7,0.8)

h2 = garchf30(n,h0,a,1)

n1 = 1000; n2 = 100
hlearn = array(dim = n1)
for(i in 1:n1)
  hlearn[i] = h2[i]

garch(hlearn,order=c(3,0),start=a)$coef
a_pr = garch(hlearn,order=c(3,0),start=a)$coef

pr = function(n1,n2,h0,h,a) {
  h_pr = array(dim=n1+n2)
  
  for(i in 1:n1)
    h_pr[i] = h[i]^2
  
  h_pr[n1+1] = a[1] + a[2]*h_pr[n1] + a[3]*h[n1-1]^2 + a[4]*h[n1-2]^2
  h_pr[n1+2] = a[1] + a[2]*h_pr[n1+1] + a[3]*h[n1]^2 + a[4]*h[n1-1]^2
  h_pr[n1+3] = a[1] + a[2]*h_pr[n1+2] + a[3]*h_pr[n1+1] + a[4]*h[n1]^2
  
  for(i in (n1+4):(n1+n2))
    h_pr[i] = a[1] + a[2]*h_pr[i-1] + a[3]*h_pr[i-2] + a[4]*h_pr[i-3]

  h_pr = sqrt(h_pr)
  for(i in 1:n1)
    h_pr[i] = h[i]
  return(h_pr)
}

h2_pr = pr(n1,n2,h0,hlearn,a_pr)

## график h
plot(h2, type='l',col='green')
## график обучающей и тестовой выборки 
lines(h2_pr, type='l', col='blue')

## 5 ----
## процесс GARCH(1,1)
garchf11 = function(n,h0,d0,a0,a1,b1,ret) {
  h = array(dim = n)
  ksi = rnorm(n,0,1)
  d2 = array(dim = n)
  
  d2[1] = a0 + a1*h0^2 + b1*d0^2
  h[1] = sqrt(d2[1])*ksi[1]
  
  for (i in 2:n) {
    d2[i] = a0 + a1*h[i-1]^2 + b1*d2[i-1]
    h[i] = sqrt(d2[i])*ksi[i]
  }
  if (ret == 1) return(h)
  else return(sqrt(d2))
}

n = 1000
h0 = 1; d0 = 1.1
a0 = 1; a1 = 0.4; b1 = 0.2

h3 = garchf11(n,h0,d0,a0,a1,b1,1)
garch(h3,order=c(1,1),start=c(a0,a1,b1))
