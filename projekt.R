library(moments)
library(fitdistrplus)
library(ggplot2)
library(actuar)
library(ggExtra)
library(MVN)
library(mnormt)
library(nortest)

mil <- read.csv("C:/Users/kacpe/Downloads/mil_d.csv")
mbk <- read.csv("C:/Users/kacpe/Downloads/mbk_d.csv")
mil_zamk <- mil$Zamkniecie
mbk_zamk <- mbk$Zamkniecie

# ROZDZIAŁ 1 

# Bank Millenium

#1

mil_data <- mil$Data
mil_data<- as.Date(mil_data)
date_values_mil <- data.frame(
  date = mil_data,
  value = mil_zamk
)

hist(mil_zamk, prob=T, main="Histogram kursów zamknięcia Bank Millennium SA", col="lightblue", border="darkblue",xlab="Kurs Zamknięcia",ylab="Gęstość", col.main="darkblue")
lines(density(mil_zamk, adjust=2), lty="dotted")

ggplot(date_values_mil, aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%Y-%m-%d") +
  labs(title = "Wykres kursów zamknięcia Bank Millennium SA",
       x = "Data",
       y = "Kurs zamknięcia")


#2

mil_kurt <- kurtosis(mil_zamk)
mil_sd <- sd(mil_zamk)
mil_skew <- skewness(mil_zamk)
mil_mean <- mean(mil_zamk)

median(mil_zamk)

mil_mean
mil_kurt
mil_sd
mil_skew

#3
fit_norm_mil <- fitdist(mil_zamk,"norm")
fit_lnorm_mil <- fitdist(mil_zamk, "lnorm")
fit_gamma_mil <- fitdist(mil_zamk, "gamma")

fit_norm_mil
fit_lnorm_mil
fit_gamma_mil

#4
plot.legend <- c("mil_normal", "mil_lognormal", "mil_gamma")
denscomp(list(fit_norm_mil,fit_lnorm_mil,fit_gamma_mil),legendtext = plot.legend)
cdfcomp(list(fit_norm_mil,fit_lnorm_mil,fit_gamma_mil),legendtext = plot.legend) #dystrybuanta empiryczna i teoretyczna
qqcomp(list(fit_norm_mil,fit_lnorm_mil,fit_gamma_mil), legendtext = plot.legend)
gofstat(list(fit_norm_mil,fit_lnorm_mil,fit_gamma_mil),fitnames= c('mil_normal','mil_lognormal','mil_gamma'))


#5
N <- 10000
n_mil <- length(mil_zamk); n_mil
alpha <- 0.05


Dn <- c()
for (i in 1:N) { 
  Yn <- rlnorm(n_mil, fit_lnorm_mil$estimate[[1]], fit_lnorm_mil$estimate[[2]])
  Dn[i] <-  ks.test(Yn, plnorm, fit_lnorm_mil$estimate[[1]], fit_lnorm_mil$estimate[[2]], exact=TRUE)$statistic
}

dn_mil <- ks.test(mil_zamk, plnorm, fit_lnorm_mil$estimate[[1]], fit_lnorm_mil$estimate[[2]], exact=TRUE)$statistic
dn_mil
hist(Dn, prob=T)
points(dn_mil,0,pch=19,col=2)

p_value_mil <- length(Dn[Dn>dn_mil])/N; p_value_mil
p_value_mil <= alpha


# mBank

#1
mbk_data <- mbk$Data
mbk_data<- as.Date(mbk_data)
date_values_mbk <- data.frame(
  date = mil_data,
  value = mil_zamk
)

hist(mbk_zamk, prob=T, main="Histogram kursów zamknięcia mBank SA", col="lightblue", border="darkblue", xlab = "Kurs zamknięcia", ylab = "Gęstość", col.main="darkblue")
lines(density(mbk_zamk, adjust=2), lty="dotted")

ggplot(date_values_mbk, aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%Y-%m-%d") +
  labs(title = "Wykres kursów zamknięcia mBank SA",
       x = "Data",
       y = "Kurs zamknięcia")


#2
mbk_kurt <- kurtosis(mbk_zamk)
mbk_sd <- sd(mbk_zamk)
mbk_skew <- skewness(mbk_zamk)
mbk_mean <- mean(mbk_zamk)

median(mbk_zamk)

mbk_kurt
mbk_sd
mbk_skew
mbk_mean

#3
fit_norm_mbk <- fitdist(mbk_zamk,"norm")
fit_lnorm_mbk <- fitdist(mbk_zamk, "lnorm")
fit_gamma_mbk <- fitdist(mbk_zamk, "gamma")

fit_norm_mbk
fit_lnorm_mbk
fit_gamma_mbk

#4
plot.legend <- c("mbk_normal", "mbk_lognormal", "mbk_gamma")
denscomp(list(fit_norm_mbk, fit_lnorm_mbk, fit_gamma_mbk), legendtext = plot.legend)
qqcomp(list(fit_norm_mbk, fit_lnorm_mbk, fit_gamma_mbk), legendtext = plot.legend)
cdfcomp(list(fit_norm_mbk, fit_lnorm_mbk, fit_gamma_mbk), legendtext = plot.legend)
gofstat(list(fit_norm_mbk, fit_lnorm_mbk, fit_gamma_mbk), fitnames = c("mbk_norm", "mbk_lognormal", "mbk_gamma"))

#5

N <- 10000
n_mbk <- length(mbk_zamk); n_mbk

D <- c()

for (i in 1:N) { 
  Y <- rgamma(n_mbk, fit_gamma_mbk$estimate[[1]], fit_gamma_mbk$estimate[[2]]) 
  D[i] <- ks.test(Y, pgamma, fit_gamma_mbk$estimate[[1]], fit_gamma_mbk$estimate[[2]], exact=TRUE)$statistic
}


dn_mbk <- ks.test(mbk_zamk, pgamma, fit_gamma_mbk$estimate[[1]], fit_gamma_mbk$estimate[[2]], exact=TRUE)$statistic
dn_mbk

hist(D, prob=T)
points(dn_mbk,0,pch=19,col=2)

p_value_mbk <- length(D[D>dn_mbk])/N; p_value_mbk

alpha <- 0.05
p_value_mbk <= alpha


#ROZDZIAŁ 2

#A.1 

log_mil = log(mil_zamk)
log_mbk = log(mbk_zamk)

diff_mil = diff(log_mil)
diff_mbk = diff(log_mbk)

fit_norm_lmil <- fitdist(diff_mil,"norm")
fit_norm_lmbk <- fitdist(diff_mbk,"norm")


fit_norm_lmil
fit_norm_lmbk


denscomp(list(fit_norm_lmil),legendtext =c('lmil_norm'))
cdfcomp(list(fit_norm_lmil),legendtext =c('lmil_norm'))
qqcomp(list(fit_norm_lmil), legendtext =c('lmil_norm'))


denscomp(list(fit_norm_lmbk),legendtext =c('lmbk_norm'))
cdfcomp(list(fit_norm_lmbk),legendtext =c('lmbk_norm'))
qqcomp(list(fit_norm_lmbk), legendtext =c('lmbk_norm'))


N <- 1000
n <- length(diff_mbk); n
alpha <- 0.05


Dn_mil <- c()
Dn_mbk <- c()
for (i in 1:N) { 
  
  Yn_mil <- rnorm(n,fit_norm_lmil$estimate[[1]],fit_norm_lmil$estimate[[2]])
  
  Dn_mil[i] <-  ks.test(Yn_mil,pnorm, fit_norm_lmil$estimate[[1]],fit_norm_lmil$estimate[[2]],exact=TRUE)$statistic
  
  Yn_mbk <- rnorm(n,fit_norm_lmbk$estimate[[1]],fit_norm_lmbk$estimate[[2]])
  
  Dn_mbk[i] <-  ks.test(Yn_mbk,pnorm, fit_norm_lmbk$estimate[[1]],fit_norm_lmbk$estimate[[2]],exact=TRUE)$statistic
  
}

dn_n_mil <-  ks.test(diff_mil,pnorm,fit_norm_lmil$estimate[[1]],fit_norm_lmil$estimate[[2]],exact=TRUE)$statistic
dn_n_mil
dn_n_mbk <-  ks.test(diff_mbk,pnorm,fit_norm_lmbk$estimate[[1]],fit_norm_lmbk$estimate[[2]],exact=TRUE)$statistic
dn_n_mbk

hist(Dn_mil,prob=T)
points(dn_n_mil,0,pch=19,col=2)

p_value_n_mil <- length(Dn_mil[Dn_mil>dn_n_mil])/N; p_value_n_mil
p_value_n_mil <= alpha

hist(Dn_mbk,prob=T)
points(dn_n_mbk,0,pch=19,col=2)

p_value_n_mbk <- length(Dn_mbk[Dn_mbk>dn_n_mbk])/N; p_value_n_mbk
p_value_n_mbk <= alpha


#B.1
df <- data.frame(mil=diff_mil,mbk=diff_mbk)
p <-  ggplot(df, aes(x=mil, y=mbk)) + geom_point()
ggMarginal(p, type="histogram")

ggMarginal(p, type="density")
?ggMarginal

#B.2
mu <- colMeans(df); mu

Sigma <- cov(df)

n <- dim(df)[1]; n
Sigma_ob <- (n-1)*cov(df)/n

Sigma 
Sigma_ob

P <- cor(df)  #macierz korelacji
P

set.seed(100)
Z <- rmnorm(1000,mu,Sigma)

Z <- as.data.frame(Z) 
colnames(Z) <- c('x','y')

próba <-  ggplot(Z, aes(x=x, y=y))+
  geom_point(size=1)+
  xlim(-0.2,0.2) +
  ylim(-0.3,0.3)
próba

rozkład_dane <- ggplot(df, aes(x=mil, y=mbk))+
  geom_point()+
  xlim(-0.2,0.2) +
  ylim(-0.3,0.3)
rozkład_dane

#B.3


s1 <- sd(diff_mil)
s2 <- sd(diff_mbk)
x     <- seq(-3*s1, 3*s1, 0.008) 
y     <- seq(-3*s2, 3*s2, 0.008)

par(mfrow=c(3,1))
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")

curve(dnorm(x,mu[1],s1),xlim=c(-4*s1,4*s1))
grid()

curve(dnorm(x,mu[2],s2),xlim=c(-4*s2,4*s2))
grid()

#C1
n <- nrow(df); n

set.seed(100)
#Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma)
Z <- rmnorm(n,mu,Sigma)
#wykresy rozrzutu
par(mfrow=c(1,2))
plot(df, xlim=c(-0.15,0.15),ylim=c(-0.15,0.15))
plot(Z,xlim=c(-0.15,0.15),ylim=c(-0.15,0.15))

#C2
par(mfrow=c(1,1))
#kwadraty odległości Mahalobisa
dM <- mahalanobis(df,mu,Sigma)
hist(dM, main = "Histogram kwadratów odległości Mahalanobisa", prob=T)

n <- dim(df)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

#testuje hipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2)
#ks.test(dM,'pchisq',2,exact=TRUE)
ks.test(dM,'pchisq',2)

#p-value < 5%, zatem odrzucam hipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2),
#co skutkuje tez odrzuceniem hipotezy o normalnosci rozkladu log-zwrotow
 
# ROZDZIAŁ 3

df <- data.frame(millennium=diff_mil, mbank=diff_mbk)

model.lm <- lm(diff_mil~diff_mbk,data=df)
model.lm

sum <- summary(model.lm)
sum

sigma(model.lm)

coef <- model.lm$coefficients 
coef
beta0 <- coef[[1]]
beta1 <- coef[[2]]
beta0; beta1

ggplot(data = df, aes(x = mbank, y = millennium)) +
  geom_point(color = "blue", size = 1.5) +
  geom_abline(intercept = beta0, slope = beta1, color = "red", linewidth = 1) +
  ggtitle("Regresja liniowa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("diff_mbk") +
  ylab("diff_mil")


# reszty

reszty <- model.lm$residuals

hist(reszty)

qqnorm(reszty)
qqline(reszty,col=2)

m <- mean(reszty)
s <- sd(reszty)
m;s
ks.test(reszty, 'pnorm', m, s)
ad.test(reszty)
shapiro.test(reszty)


#p-value < 5% dla AD i SW, są bardziej czułe niż KS więc odrzucam hipotezę
# o normalności rozkładu reszt

#RSE
RSE <- sqrt(sum(reszty^2)/(length(diff_mbk)-2))
RSE

#Ponowna regresja i predykcja, przy b0=0


df <- data.frame(millennium=diff_mil, mbank=diff_mbk)

model.lm.no.intercept <- lm(diff_mil~diff_mbk-1,data=df)
model.lm.no.intercept

sum2 <- summary(model.lm.no.intercept)
sum2 

# wielkości log-zwrotów spółki Bank Millennium, gdy log-zwroty spółki mBank będą 
# na poziomie średniej z posiadanej próby

m <- mean(diff_mbk)
m

beta0 <- coef(model.lm)[1]; beta0
beta1 <- coef(model.lm)[2]; beta1

pred1 <- beta0 + beta1 * m
pred1

beta1_model2 <- model.lm2$coefficients
beta1_model2



pred2 <- beta1_model2*m
pred2

nowa.model <- data.frame(mBank=m)

nowa.model;
model.lm;

predict(model.lm2, nowa.model, interval="confidence")




