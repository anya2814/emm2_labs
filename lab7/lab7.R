## Лабораторная работа №7

## 1. Реализовать 1000 случайных величин 𝑁𝑡 при фиксированном значении 𝑡 = 50, 𝜆 = 2. ----

t = 50; lambda = 2

n = 1000
Nt = array(dim=n)

i = 1
repeat {
  count = 0
  tau = c()
  repeat {
    tau = c(tau,rexp(1,lambda))
    if (sum(tau)<=t) count = count + 1
    else break
  }
  Nt[i] = count
  if(i==n) break
  i = i + 1
}
Nt

## 2. Построить гистограмму реализованной последовательности и наложить ----
## на гистограмму график (13).

f_pois <- function(x,lambda,t) {
  res = (lambda*t)^x/factorial(x)*exp(-lambda*t)
  return(res)
}

hist(Nt,freq=FALSE,ylim=c(0,0.045))
curve(f_pois(x,lambda,t),xlim=c(min(Nt),max(Nt)),add=TRUE)

## 3. Реализовать процесс (12) до фиксированного момента времени 𝑡𝑚𝑎𝑥 и ----
## построить его график.

u_f <- function(c,lambda,m,U0,tmax) {
  U = U0
  T = 0
  u_prev = U0
  sum_tau = 0
  repeat {
    tau = rexp(1,lambda)
    sum_tau = sum_tau + tau
    if(sum_tau>tmax) break
    T = c(T,sum_tau)
    X = rexp(1,1/m)
    u_prev = u_prev + c*tau - X
    U = c(U,u_prev)
  }
  return(cbind.data.frame(U,T))
}

## (a) 𝑈0 = 50, условие (15) выполнено:
U0 = 50
c = 3; lambda = 0.2; m = 8
ro = c/(lambda*m) - 1
ro
tmax = 100

UT = u_f(c,lambda,m,U0,tmax)
U = UT$U; U
T = UT$T; T

plot(T,U,type='l')

# (b) 𝑈0 = 50, условие (15) не выполнено:
c = 4; lambda = 0.25; m = 20
ro = c/(lambda*m) - 1
ro

UT = u_f(c,lambda,m,U0,tmax)
U = UT$U; U
T = UT$T; T

plot(T,U,type='l')

## 4. Рассчитать выборочную вероятность разорения фирмы. ----

N = 1000
tmax = 1000; c = 1; lambda = 0.3
m = 3; U0 = 100

count = 0
for(i in 1:N) {
  U = u_f(c,lambda,m,U0,tmax)$U
  if(min(U)<0) count = count + 1
}

psi_est = 1/N*count
psi_est
