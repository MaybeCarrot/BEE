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
t_trust <- t.test(data_sea_trust, data_dummy_trust, na.rm = TRUE)
t_twp <- t.test(data_sea_twp, data_dummy_twp, na.rm = TRUE)
z_trust <- z.test(data_sea_trust, data_dummy_trust, sigma.x = sd(data_sea_trust$trust, na.rm = TRUE), sigma.y = sd(data_dummy_trust$trust, na.rm = TRUE))
z_twp <- z.test(data_sea_twp, data_dummy_twp, sigma.x = sd(data_sea_twp$tw_p, na.rm = TRUE), sigma.y = sd(data_dummy_twp$tw_p, na.rm = TRUE))
w_trust <- wilcox.test(data_sea_trust$trust, data_dummy_trust$trust, na.rm = TRUE)
w_twp <- wilcox.test(data_sea_twp$tw_p, data_dummy_twp$tw_p, na.rm = TRUE)

plot(density(data_sea_trust$trust, na.rm = TRUE), col = "blue", lwd = 3,xlim = c(0, 5), xlab = "Brasilian Reais", main = "Trust")
points(density(data_dummy_trust$trust, na.rm = TRUE), col = "green", lwd = 3,xlim = c(0, 5))

plot(density(data_sea_twp$tw_p, na.rm = TRUE), col = "blue", lwd = 3,xlim = c(0, 100), xlab = "Brasilian Reais", main = "Tw_p")
points(density(data_dummy_twp$tw_p, na.rm = TRUE), col = "green", lwd = 3,xlim = c(0, 100))

#Ultimatum game
data_sea_offer <- data[data$sea_dummy == 1, "offer"]
data_sea_norm <- data[data$sea_dummy == 1, "norm"]
data_dummy_offer <- data[data$sea_dummy != 1, "offer"]
data_dummy_norm <- data[data$sea_dummy != 1, "norm"]
t_offer <- t.test(data_sea_offer, data_dummy_offer, na.rm = TRUE)
t_norm <- t.test(data_sea_norm, data_dummy_norm, na.rm = TRUE)
z_offer <- z.test(data_sea_offer, data_dummy_offer, sigma.x = sd(data_sea_offer$offer, na.rm = TRUE), sigma.y = sd(data_dummy_offer$offer, na.rm = TRUE))
z_norm <- z.test(data_sea_norm, data_dummy_norm, sigma.x = sd(data_sea_norm$norm, na.rm = TRUE), sigma.y = sd(data_dummy_norm$norm, na.rm = TRUE))
w_offer <- wilcox.test(data_sea_offer$offer, data_dummy_offer$offer, na.rm = TRUE)
w_norm <- wilcox.test(data_sea_norm$norm, data_dummy_norm$norm, na.rm = TRUE)

plot(density(data_sea_offer$offer, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Brasilian Reais", main = "Offer")
points(density(data_dummy_offer$offer, na.rm = TRUE), col = "green", lwd = 3)

plot(density(data_sea_norm$norm, na.rm = TRUE), col = "blue", lwd = 3,xlab = "Brasilian Reais", main = "Norm")
points(density(data_dummy_norm$norm, na.rm = TRUE), col = "green", lwd = 3)

#Donation experiment
data_sea_self <- data[data$sea_dummy == 1, "self"]
data_sea_donate <- data[data$sea_dummy == 1, "donate"]
data_dummy_self <- data[data$sea_dummy != 1, "self"]
data_dummy_donate <- data[data$sea_dummy != 1, "donate"]
t_self <- t.test(data_sea_self, data_dummy_self, na.rm = TRUE)
t_donate <- t.test(data_sea_donate, data_dummy_donate, na.rm = TRUE)
z_self <- z.test(data_sea_self, data_dummy_self, sigma.x = sd(data_sea_self$self, na.rm = TRUE), sigma.y = sd(data_dummy_self$self, na.rm = TRUE))
z_donate <- z.test(data_sea_donate, data_dummy_donate, sigma.x = sd(data_sea_donate$donate, na.rm = TRUE), sigma.y = sd(data_dummy_donate$donate, na.rm = TRUE))
w_self <- wilcox.test(data_sea_self$self, data_dummy_self$self, na.rm = TRUE)
w_donate <- wilcox.test(data_sea_donate$donate, data_dummy_donate$donate, na.rm = TRUE)

plot(density(data_sea_self$self, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Brasilian Reais", main = "Self")
points(density(data_dummy_self$self, na.rm = TRUE), col = "green", lwd = 3)

plot(density(data_sea_donate$donate, na.rm = TRUE), col = "blue", lwd = 3,xlab = "Brasilian Reais", main = "Donate")
points(density(data_dummy_donate$donate, na.rm = TRUE), col = "green", lwd = 3)

#Public goods experiment
data_sea_coop <- data[data$sea_dummy == 1, "coop"]
data_dummy_coop <- data[data$sea_dummy != 1, "coop"]
t_coop <- t.test(data_sea_coop, data_dummy_coop, na.rm = TRUE)
z_coop <- z.test(data_sea_coop, data_dummy_coop, sigma.x = sd(data_sea_coop$coop, na.rm = TRUE), sigma.y = sd(data_dummy_coop$coop, na.rm = TRUE))
w_coop <- wilcox.test(data_sea_coop$coop, data_dummy_coop$coop, na.rm = TRUE)

plot(density(data_sea_coop$coop, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Brasilian Reais", main = "Coop")
points(density(data_dummy_coop$coop, na.rm = TRUE), col = "green", lwd = 3)

#Coordination game
data_sea_stag1 <- data[data$sea_dummy == 1, "stag1"]
data_dummy_stag1 <- data[data$sea_dummy != 1, "stag1"]
t_stag1 <- t.test(data_sea_stag1, data_dummy_stag1, na.rm = TRUE)
z_stag1 <- z.test(data_sea_stag1, data_dummy_stag1, sigma.x = sd(data_sea_stag1$stag1, na.rm = TRUE), sigma.y = sd(data_dummy_stag1$stag1, na.rm = TRUE))
w_stag1 <- wilcox.test(data_sea_stag1$stag1, data_dummy_stag1$stag1, na.rm = TRUE)

plot(density(data_sea_stag1$stag1, na.rm = TRUE), col = "blue", lwd = 3, ylim = c(0, 2), xlab = "Brasilian Reais", main = "Stag1")
points(density(data_dummy_stag1$stag1, na.rm = TRUE), col = "green", lwd = 3)

#Risk-aversion game
data_sea_risk <- data[data$sea_dummy == 1, "risk"]
data_dummy_risk <- data[data$sea_dummy != 1, "risk"]
t_risk <- t.test(data_sea_risk, data_dummy_risk, na.rm = TRUE)
z_risk <- z.test(data_sea_risk, data_dummy_risk, sigma.x = sd(data_sea_risk$risk, na.rm = TRUE), sigma.y = sd(data_dummy_risk$risk, na.rm = TRUE))
w_risk <- wilcox.test(data_sea_risk$risk, data_dummy_risk$risk, na.rm = TRUE)

plot(density(data_sea_risk$risk, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Brasilian Reais", main = "Risk")
points(density(data_dummy_risk$risk, na.rm = TRUE), col = "green", lwd = 3)

#Cooperation rate
data_sea_coopnorm <- data[data$sea_dummy == 1, "coopnorm"]
data_dummy_coopnorm <- data[data$sea_dummy != 1, "coopnorm"]
t_coopnorm <- t.test(data_sea_coopnorm, data_dummy_coopnorm, na.rm = TRUE)
z_coopnorm <- z.test(data_sea_coopnorm, data_dummy_coopnorm, sigma.x = sd(data_sea_coopnorm$coopnorm, na.rm = TRUE), sigma.y = sd(data_dummy_coopnorm$coopnorm, na.rm = TRUE))
w_coopnorm <- wilcox.test(data_sea_coopnorm$coopnorm, data_dummy_coopnorm$coopnorm, na.rm = TRUE)

plot(density(data_sea_coopnorm$coopnorm, na.rm = TRUE), col = "blue", lwd = 3, xlab = "Brasilian Reais", main = "Coopnorm")
points(density(data_dummy_coopnorm$coopnorm, na.rm = TRUE), col = "green", lwd = 3)

#Histograms
mean_sea_trust <- mean(data_sea_trust$trust, na.rm = TRUE) *20
mean_sea_twp <- mean(data_sea_twp$tw_p, na.rm = TRUE)
mean_dummy_trust <- mean(data_dummy_trust$trust, na.rm = TRUE) *20
mean_dummy_twp <- mean(data_dummy_twp$tw_p, na.rm = TRUE)
mean_seatrust <- as.numeric(c(mean_sea_trust, mean_sea_twp))
hist(as.numeric(c(mean_sea_trust, mean_sea_twp)),  freq = 5, breaks=5,xlim = c(0, 100), ylim = c(0, 1.2), col = "blue")
hist(as.numeric(c(mean_dummy_trust, mean_dummy_twp)),  freq = 5, breaks=5,ylim = c(0, 1.2), col = "green", add = TRUE)


#3)


