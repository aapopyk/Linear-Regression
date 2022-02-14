


setwd("/Users/aaronpopyk")
data = read.csv("fredgraph.csv", header=T)

cpi = data$CPILFESL_PC1
mb = data$BOGMBASE

n = length(cpi)
CPI = c()
for(i in 1:n-1) {
  CPI[i] = cpi[i+1] - cpi[i]
}

m = length(mb)
MB = c()
for(i in 1:m-1) {
  MB[i] = mb[i + 1] - mb[i]
}


# Maximize Pearson's r 

t = (25:440)

r = cor(MB[t], CPI[t])

for (i in 1:24){
  r[i] = cor(MB[t-i], CPI[t])
}

plot(r, xlab="Monthly Observation (i)", ylab="Pearson's Coefficient (r)", main="Correlation Plot of (M0 - i) and CPI")
max(r)
which.max(r)


# 1985-2021

plot(mb, cpi, xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Jan.1985-Sept.2021, before adj.)")

LM1 = lm(CPI~MB)
summary(LM1)

plot(MB, resid(LM1), xlab="Adj.Monetary Base (Jan.1985-Sept.2021)")
plot(resid(LM1), xlab="Monthly Observation (Jan.1985-Sept.2021)")
qqnorm(resid(LM1))


# 1985-2008

t2 = (1:285)
plot(mb[t2],cpi[t2], xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Jan.1985-Aug.2008, before adj.)")

MB2 = MB[1:272]
CPI2 = CPI[14:285]

plot(MB2,CPI2, xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Jan.1985-Aug.2008, after adj.)")

LM2 = lm(CPI2~MB2)
summary(LM2)

plot(MB2, CPI2, xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Jan.1985-Aug.2008, after adj.)")
abline(LM2)

plot(MB2, resid(LM2), xlab="Monetary Base ($)", main="Residual Plot (Jan.1985-Aug.2008, after adj.)")
abline(0,0)

plot(resid(LM2), xlab="Monthly Observation (Time)", main="Residual Index Plot (Jan.1985-Aug.2008)")
abline(0,0)

qqnorm(resid(LM2), main="Normal Q-Q Plot (Jan.1985-Aug.2008)")


# 2008-2021

t3 = (286:441)
plot(mb[t3],cpi[t3], xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Sept.2008-Sept.2021, before adj.)")

MB3 = MB[285:427]
CPI3 = CPI[298:440]

plot(MB3,CPI3, xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Sept.2008-Sept.2021, after adj.)")

LM3 = lm(CPI3~MB3)
summary(LM3)

plot(MB3, CPI3, xlab="Monetary Base ($)", ylab="Consumer Price Index (%)", main="M0 & CPI (Sept.2008-Sept.2021, after adj.)")
abline(LM3)

plot(MB3, resid(LM3), xlab="Monetary Base ($)", main="Residual Plot (Sept.2008-Sept.2021, after adj.)")
abline(0,0)

plot(resid(LM3), xlab="Monthly Observation (Time)", main="Residual Index Plot (Sept.2008-Sept.2021)")
abline(0,0)

qqnorm(resid(LM3), main="Normal Q-Q Plot (Sept.2008-Sept.2021)")






