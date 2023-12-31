## Лабораторная работа №6

# 1. Замоделировать процесс (10) для 𝑆0 = 100, 𝑎 = 0.5, 𝜎 = 0.8,
# √ ∆ = 0.01,𝑘 = 0,1,...,10^3.

n = 1000
d = 0.0001
i = 0:n
kd = d*i
a = 0.5; sig = 0.8

S = array(dim=n+1)
S[1] = 100
B = array(dim=n+1)
B[1] = 0

for(i in 2:(n+1)) {
  B[i] = B[i-1] + rnorm(1,0,0.01) 
  S[i] = S[1]*exp((a-sig^2/2)*(i)*d+sig*B[i])
}
plot(kd,S,type='l')

# 2. Рассчитать оценки параметров броуновского движения (𝑎,𝜎^2)
# по наблюдениям процесса {𝑆𝑘∆}𝑘≥1.

X = array(dim=n)

for(i in 1:n)
  X[i] = log(S[i+1]) - log(S[i])

m_est = mean(X)
sigB2_est = var(X)*(n-1)/n

sig2_est = sigB2_est/d
a_est = m_est/d + sig2_est/2

sig_est = sqrt(sig2_est)

# оценки параметров броуновского движения (a, 𝜎^2): 
a_est; sig2_est
