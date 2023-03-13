data <- read.csv("Extraktionen_gesamt_V2_DL.csv", sep=";")
# unique vs razor
MBR <- (data$Razor.MBR.2.peptides/data$Unique.MBR.2.peptides)*100
noMBR <- (data$Razor.noMBR.2.peptides/data$Unique.no.MBR.2.peptides)*100

boxplot(MBR, noMBR, main = "Abweichung Razor vs Unique [%]", names = c("MBR", "noMBR"))
summary(MBR)
summary(noMBR)
t.test(MBR,noMBR,paired=TRUE)
t.test(data$Razor.noMBR.2.peptides,data$Unique.no.MBR.2.peptides,paired = TRUE)
t.test(data$Razor.MBR.2.peptides,data$Unique.MBR.2.peptides,paired=TRUE)

mean_MBR <- (data$Razor.MBR.2.peptides+data$Unique.MBR.2.peptide)/2
mean_noMBR <- (data$Razor.noMBR.2.peptides+data$Unique.no.MBR.2.peptides)/2


###############no MBR
# https://www.r-bloggers.com/2018/09/fitting-exponential-decays-in-r-the-easy-way/
dat <- as.data.frame(cbind(noMBR,mean_noMBR))
colnames(dat) <- c("y","x")
dat <- dat[-c(161:162),]


fit1 <- nls(y ~ SSasymp(x, yf, y0, log_alpha), data = dat)
fit1
qplot(x, y, data = augment(fit1)) + geom_line(aes(y = .fitted))

summary(fit1)
###############MBR
# https://www.r-bloggers.com/2018/09/fitting-exponential-decays-in-r-the-easy-way/
dat <- as.data.frame(cbind(MBR,mean_MBR))
colnames(dat) <- c("y","x")
dat <- dat[-c(161:162),]


fit <- nls(y ~ SSasymp(x, yf, y0, log_alpha), data = dat)
fit
qplot(x, y, data = augment(fit)) + geom_line(aes(y = .fitted))


summary(fit)

anova(fit1,fit)
