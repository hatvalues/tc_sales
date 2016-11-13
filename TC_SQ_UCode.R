## ---- Initial_Load ----
# original link
# link <- "https://raw.githubusercontent.com/julianhatwell/TC_SalesQuotas/master/TC_SalesQuota.csv"
# local file
link <- "C:\\Dev\\Study\\R\\TC_SalesQuotas\\TC_SalesQuota.csv"
#link <- "D:\\github\\TC_SalesQuotas\\TC_SalesQuota.csv"
raw.data <- read_csv(link)
n <- nrow(raw.data)
attach(raw.data)
raw.data$Group <- as.factor(Group)
raw.data$KSales <- Sales/1000
raw.data$KQuota <- Quota/1000
raw.data$OnTarget <- (Sales-Quota)/Quota*100
raw.data$MetTarget <- raw.data$OnTarget >= 0
attach(raw.data)
# means
# pop
mn_Quo_P <- mean(KQuota)
mn_Sal_P <- mean(KSales)
mn_Tar_P <- mean(OnTarget)
md_Quo_P <- median(KQuota)
mn_Quo_Met_P <- tapply(KQuota, MetTarget, mean)
# Grp A
mn_Quo_A <- mean(KQuota[Group == "A"])
mn_Sal_A <- mean(KSales[Group == "A"])
mn_Tar_A <- mean(OnTarget[Group == "A"])
md_Quo_A <- median(KQuota[Group == "A"])
mn_Quo_Met_A <- tapply(KQuota[Group == "A"], MetTarget[Group == "A"], mean)
# Grp B
mn_Quo_B <- mean(KQuota[Group == "B"])
mn_Sal_B <- mean(KSales[Group == "B"])
mn_Tar_B <- mean(OnTarget[Group == "B"])
md_Quo_B <- median(KQuota[Group == "B"])
mn_Quo_Met_B <- tapply(KQuota[Group == "B"], MetTarget[Group == "B"], mean)

mns <- round(matrix(c(mn_Quo_Met_A, mn_Quo_Met_B),2,2),3)
# St Devs
pop.sd <- function(x) {
  sqrt(var(x) * (length(x) -1) / length(x))
}
# Pop
sd_Quo_P <- pop.sd(KQuota)
sd_Sal_P <- pop.sd(KSales)
sd_Tar_P <- pop.sd(OnTarget)
# Grp A
sd_Quo_A <- pop.sd(KQuota[Group == "A"])
sd_Sal_A <- pop.sd(KSales[Group == "A"])
sd_Tar_A <- pop.sd(OnTarget[Group == "A"])
# Grp B
sd_Quo_B <- pop.sd(KQuota[Group == "B"])
sd_Sal_B <- pop.sd(KSales[Group == "B"])
sd_Tar_B <- pop.sd(OnTarget[Group == "B"])

# centering each group in its attainment mean
raw.data$Sales_C <- KSales - ifelse(Group == "A", mn_Sal_A, mn_Sal_B)
raw.data$Quota_C <- KQuota - ifelse(Group == "A", mn_Quo_A, mn_Quo_B)
raw.data$OnTarget_C <- OnTarget - ifelse(Group == "A", mn_Tar_A, mn_Tar_B)
# population centres
raw.data$Sales_CP <- KSales - mn_Sal_P
raw.data$Quota_CP <- KQuota - mn_Quo_P
raw.data$OnTarget_CP <- OnTarget - mn_Tar_P

raw.data$Quota_Size <- rep(NA, n)
raw.data$Quota_Size[Group == "A"] <- ifelse(KQuota[Group == "A"] > median(md_Quo_A), "High", "Low")
raw.data$Quota_Size[Group == "B"] <- ifelse(KQuota[Group == "B"] > median(md_Quo_B), "High", "Low")
attach(raw.data)

## ---- moments ----
shapiro.test(OnTarget[Group == "A"])
cat("Skewness Group A")
skewness(Quota_CP[Group == "A"])
cat("Kurtosis Group A")
kurtosis(Quota_CP[Group == "A"])
shapiro.test(OnTarget[Group == "B"])
cat("Skewness Group B")
skewness(Quota_CP[Group == "B"])
cat("Kurtosis Group B")
kurtosis(Quota_CP[Group == "B"])

## ---- odds_ratio ----
QuotaTarget <- with(raw.data, table(Quota_Size, MetTarget, Group))
QT.OR <- loddsratio(QuotaTarget, log = FALSE)
coef(QT.OR)

## ---- basic_lm ----
lm1 <- lm(Sales_CP ~ Group * Quota_CP -1)
raw.data$predict.lm1 <- lm1$fitted.values
attach(raw.data)
misclass <- raw.data[Group == "B" &
                       Quota == tapply(Quota, Group, max)[2]
                     ,c("KQuota", "OnTarget")]
influential <- raw.data[as.numeric(
  names(
    head(
      sort(cooks.distance(lm1)[Group == "B"]
           , decreasing = TRUE), 2))), ]
s.lm1 <- summary(lm1)
cat("OLS Linear Model")
round(coef(s.lm1), 4)
cat("R-Squared", s.lm1$r.squared, "\n"
    , "Model explains" , round(s.lm1$r.squared, 2) * 100
    , "per cent of the variability")

## ---- Bayes_lm ----
lm1.mcmc <- MCMCregress(Sales_CP ~ Group * Quota_CP -1
                        , b0 = s.lm1$coefficients[,1]
                        , B0=1/s.lm1$coefficients[,2]
                        , marginal.likelihood = "Chib95")
s.lm1.mcmc <- summary(lm1.mcmc)
cat("Bayes LM")
params <- dimnames(lm1.mcmc)[[2]]
params <- params[params != "sigma2"]
t(sapply(params, function(x) {
  quantile(lm1.mcmc[, x], c(0.1, 0.9))
}))

## ---- varianceTrend ----
loosd <- function(x) {# leave one out SD
  stu.mat <- matrix(x, nrow = length(x), ncol = length(x))
  stu.mat[row(stu.mat) == col(stu.mat)] <- NA
  stu.mat <- apply(stu.mat, 2, sd, na.rm = TRUE)
  #exp(sqrt((stu.mat-sd(x))^2)) * sd(x) - sd(x)
  (stu.mat-sd(x))^2 * 100
}
raw.data$sd_inflation <- unlist(tapply(OnTarget, Group, loosd))
attach(raw.data)

## ---- Statistics ----
head(dplyr::select(raw.data, Sales, Quota, Attainment, OnTarget, MetTarget, Group))
summary(dplyr::select(raw.data, Sales, Quota, Attainment, OnTarget, MetTarget, Group))
# means
kable(data.frame(
  Population_mean_KQuota = mn_Quo_P
  , Population_mean_KSales = mn_Sal_P
  , Population_mean_OnTarget = mn_Tar_P
))
kable(data.frame(
  GroupA_mean_KQuota = mn_Quo_A
  , GroupA_mean_KSales = mn_Sal_A
  , GroupA_mean_OnTarget = mn_Tar_A
))
kable(data.frame(
  GroupB_mean_KQuota = mn_Quo_B
  , GroupB_mean_KSales = mn_Sal_B
  , GroupB_mean_OnTarget = mn_Tar_B
))
kable(data.frame(
  Population_StDev_KQuota = sd_Quo_P
  , Population_StDev_KSales = sd_Sal_P
  , Population_StDev_OnTarget = sd_Tar_P
))
kable(data.frame(
  GroupA_StDev_KQuota = sd_Quo_A
  , GroupA_StDev_KSales = sd_Sal_A
  , GroupA_StDev_OnTarget = sd_Tar_A
))
kable(data.frame(
  GroupB_StDev_KQuota = sd_Quo_B
  , GroupB_StDev_KSales = sd_Sal_B
  , GroupB_StDev_OnTarget = sd_Tar_B
))

## ---- furtherLM ----
lm2 <- lm(Sales_CP ~ Group * Quota_CP + sd_inflation -1)
lm3 <- lm(Sales_CP ~ Group * Quota_CP + Group * sd_inflation -1)
anova(lm1, lm2, lm3, test = "Chisq")
LRstats(lm1, lm2, lm3)

## ---- investigationOfGLM ----
raw.data$CRate <- (Sales/(Quota * 20))
glm1 <- glm(CRate ~ Group, family = binomial, data = raw.data, weights = Quota)
glm2 <- glm(CRate ~ Group + Quota, data = raw.data, family = binomial)