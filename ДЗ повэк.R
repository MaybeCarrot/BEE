data <- data_ode_onecon
library("maxLik") 
library(moments)
library(BSDA)
library(stargazer)

#2)
#trust_game
data_sea_trust <- data[data$sea_dummy == 1, "trust"]
data_sea_twp <- data[data$sea_dummy == 1, "tw_p"]
data_dummy_trust <- data[data$sea_dummy != 1, "trust"]
data_dummy_twp <- data[data$sea_dummy != 1, "tw_p"]
t_trust <- t.test(data_sea_trust, data_dummy_trust, na.rm = TRUE)#t.test - для срдених, никакой иной функции не несет
t_twp <- t.test(data_sea_twp, data_dummy_twp, na.rm = TRUE)
w_trust <- wilcox.test(data_sea_trust$trust, data_dummy_trust$trust, na.rm = TRUE)
w_twp <- wilcox.test(data_sea_twp$tw_p, data_dummy_twp$tw_p, na.rm = TRUE)

plot(density(data_sea_trust$trust, na.rm = TRUE), col = "blue", lwd = 3,xlim = c(0, 5), xlab = "Brasilian Reais", main = "Trust")
points(density(data_dummy_trust$trust, na.rm = TRUE), col = "green", lwd = 3,xlim = c(0, 5))

plot(density(data_sea_twp$tw_p, na.rm = TRUE), col = "blue", lwd = 3,xlim = c(0, 100), xlab = "Percents", main = "Tw_p")
points(density(data_dummy_twp$tw_p, na.rm = TRUE), col = "green", lwd = 3,xlim = c(0, 100))

#Ultimatum game 
data_sea_offer <- data[data$sea_dummy == 1, "offer"]
data_sea_norm <- data[data$sea_dummy == 1, "norm"]
data_dummy_offer <- data[data$sea_dummy != 1, "offer"]
data_dummy_norm <- data[data$sea_dummy != 1, "norm"]
t_offer <- t.test(data_sea_offer, data_dummy_offer, na.rm = TRUE)
t_norm <- t.test(data_sea_norm, data_dummy_norm, na.rm = TRUE)
w_offer <- wilcox.test(data_sea_offer$offer, data_dummy_offer$offer, na.rm = TRUE)
w_norm <- wilcox.test(data_sea_norm$norm, data_dummy_norm$norm, na.rm = TRUE)

plot(density(data_sea_offer$offer, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Rate", main = "Offer")
points(density(data_dummy_offer$offer, na.rm = TRUE), col = "green", lwd = 3)

plot(density(data_sea_norm$norm, na.rm = TRUE), col = "blue", lwd = 3,xlab = "Rate", main = "Norm")
points(density(data_dummy_norm$norm, na.rm = TRUE), col = "green", lwd = 3)

#Donation experiment
data_sea_self <- data[data$sea_dummy == 1, "self"]
data_sea_donate <- data[data$sea_dummy == 1, "donate"]
data_dummy_self <- data[data$sea_dummy != 1, "self"]
data_dummy_donate <- data[data$sea_dummy != 1, "donate"]
t_self <- t.test(data_sea_self, data_dummy_self, na.rm = TRUE)
t_donate <- t.test(data_sea_donate, data_dummy_donate, na.rm = TRUE)
w_self <- wilcox.test(data_sea_self$self, data_dummy_self$self, na.rm = TRUE)
w_donate <- wilcox.test(data_sea_donate$donate, data_dummy_donate$donate, na.rm = TRUE)

plot(density(data_sea_self$self, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Rate", main = "Self")
points(density(data_dummy_self$self, na.rm = TRUE), col = "green", lwd = 3)

plot(density(data_sea_donate$donate, na.rm = TRUE), col = "blue", lwd = 3,xlab = "Rate", main = "Donate")
points(density(data_dummy_donate$donate, na.rm = TRUE), col = "green", lwd = 3)

#Public goods experiment
data_sea_coop <- data[data$sea_dummy == 1, "coop"]
data_dummy_coop <- data[data$sea_dummy != 1, "coop"]
t_coop <- t.test(data_sea_coop, data_dummy_coop, na.rm = TRUE)
w_coop <- wilcox.test(data_sea_coop$coop, data_dummy_coop$coop, na.rm = TRUE)

plot(density(data_sea_coop$coop, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Rate", main = "Coop")
points(density(data_dummy_coop$coop, na.rm = TRUE), col = "green", lwd = 3)

#Coordination game
data_sea_stag1 <- data[data$sea_dummy == 1, "stag1"]
data_dummy_stag1 <- data[data$sea_dummy != 1, "stag1"]
t_stag1 <- t.test(data_sea_stag1, data_dummy_stag1, na.rm = TRUE)
w_stag1 <- wilcox.test(data_sea_stag1$stag1, data_dummy_stag1$stag1, na.rm = TRUE)

plot(density(data_sea_stag1$stag1, na.rm = TRUE), col = "blue", lwd = 3, ylim = c(0, 2), xlab = "Stag", main = "Stag1")
points(density(data_dummy_stag1$stag1, na.rm = TRUE), col = "green", lwd = 3)

#Risk-aversion game
data_sea_risk <- data[data$sea_dummy == 1, "risk"]
data_dummy_risk <- data[data$sea_dummy != 1, "risk"]
t_risk <- t.test(data_sea_risk, data_dummy_risk, na.rm = TRUE)
w_risk <- wilcox.test(data_sea_risk$risk, data_dummy_risk$risk, na.rm = TRUE)

plot(density(data_sea_risk$risk, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Risk rate", main = "Risk")
points(density(data_dummy_risk$risk, na.rm = TRUE), col = "green", lwd = 3)

#Cooperation rate
data_sea_coopnorm <- data[data$sea_dummy == 1, "coopnorm"]
data_dummy_coopnorm <- data[data$sea_dummy != 1, "coopnorm"]
t_coopnorm <- t.test(data_sea_coopnorm, data_dummy_coopnorm, na.rm = TRUE)
w_coopnorm <- wilcox.test(data_sea_coopnorm$coopnorm, data_dummy_coopnorm$coopnorm, na.rm = TRUE)

plot(density(data_sea_coopnorm$coopnorm, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Cooperation rate", main = "Coopnorm")
points(density(data_dummy_coopnorm$coopnorm, na.rm = TRUE), col = "green", lwd = 3)

#3)
#left
datal <- left
l_sea_coopnorm <- datal[datal$sea_dummy == 1, "coopnorm"]
l_dummy_coopnorm <- datal[datal$sea_dummy != 1, "coopnorm"]
t_seal <- t.test(data_sea_coopnorm, l_sea_coopnorm, na.rm = TRUE)
w_seal <- wilcox.test(data_sea_coopnorm$coopnorm, l_sea_coopnorm$coopnorm, na.rm = TRUE)

t_dummyl <- t.test(data_dummy_coopnorm, l_dummy_coopnorm, na.rm = TRUE)
w_dummyl <- wilcox.test(data_dummy_coopnorm$coopnorm, l_dummy_coopnorm$coopnorm, na.rm = TRUE)

#women
dataw <- women
w_sea_coopnorm <- dataw[dataw$sea_dummy == 1, "coopnorm"]
w_dummy_coopnorm <- dataw[dataw$sea_dummy != 1, "coopnorm"]
t_seaw <- t.test(data_sea_coopnorm, w_sea_coopnorm, na.rm = TRUE)
w_seaw <- wilcox.test(data_sea_coopnorm$coopnorm, w_sea_coopnorm$coopnorm, na.rm= TRUE)

t_dummyw <- t.test(data_dummy_coopnorm, w_dummy_coopnorm, na.rm = TRUE)
w_dummyw <- wilcox.test(data_dummy_coopnorm$coopnorm, w_dummy_coopnorm$coopnorm, na.rm = TRUE)
