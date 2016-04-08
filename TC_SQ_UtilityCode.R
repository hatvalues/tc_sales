## ---- Initial_Load ----
# original link
# link <- "https://raw.githubusercontent.com/julianhatwell/TC_SalesQuotas/master/TC_SalesQuota.csv"
# local file
#link <- "C:\\Dev\\Study\\R\\TC_SalesQuotas\\TC_SalesQuota.csv"
link <- "D:\\github\\TC_SalesQuotas\\TC_SalesQuota.csv"
raw.data <- read_csv(link)
n = nrow(raw.data)
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
# Pop
sd_Quo_P <- sd(KQuota)
sd_Sal_P <- sd(KSales)
sd_Tar_P <- sd(OnTarget)
# Grp A
sd_Quo_A <- sd(KQuota[Group == "A"])
sd_Sal_A <- sd(KSales[Group == "A"])
sd_Tar_A <- sd(OnTarget[Group == "A"])
# Grp B
sd_Quo_B <- sd(KQuota[Group == "B"])
sd_Sal_B <- sd(KSales[Group == "B"])
sd_Tar_B <- sd(OnTarget[Group == "B"])

# centering each group in its attainment mean
raw.data$Sales_C <- KSales - ifelse(Group == "A", mn_Sal_A, mn_Sal_B)
raw.data$Quota_C <- KQuota - ifelse(Group == "A", mn_Quo_A, mn_Quo_B)
raw.data$OnTarget_C <- OnTarget - ifelse(Group == "A", mn_Tar_A, mn_Tar_B)

raw.data$Quota_Size <- rep(NA, n)
raw.data$Quota_Size[Group == "A"] <- ifelse(KQuota[Group == "A"] > median(md_Quo_A), "High", "Low")
raw.data$Quota_Size[Group == "B"] <- ifelse(KQuota[Group == "B"] > median(md_Quo_B), "High", "Low")
attach(raw.data)

## ---- odds_ratio ----
QuotaTarget <- with(raw.data, table(Quota_Size, MetTarget, Group))
QT.OR <- loddsratio(QuotaTarget, log = FALSE)
coef(QT.OR)

## ---- group_B_cluster_params ----
lowOutlierBoundary <- -25
clusterOneBoundary <- -1
clusterTwoBoundary <- 16.5
highOutlierBoundary <- 35
mvarBoundary <- 7.38

cluster1 <- Group == "B" & 
  OnTarget < clusterOneBoundary & 
  OnTarget > lowOutlierBoundary
cluster2 <- Group == "B" & 
  OnTarget < clusterTwoBoundary & 
  OnTarget > clusterOneBoundary
cluster3 <- Group == "B" & 
  OnTarget < highOutlierBoundary & 
  OnTarget > clusterTwoBoundary

cluster1A <- Group == "A" & 
  OnTarget < clusterOneBoundary & 
  OnTarget > lowOutlierBoundary &
  KQuota <= max(KQuota[Group == "B"])
cluster2A <- Group == "A" & 
  OnTarget < clusterTwoBoundary & 
  OnTarget > clusterOneBoundary &
  KQuota <= max(KQuota[Group == "B"])
cluster3A <- Group == "A" & 
  OnTarget < highOutlierBoundary & 
  OnTarget > clusterTwoBoundary &
  KQuota <= max(KQuota[Group == "B"])

raw.data$clusterGroup <- ifelse(Group == "A", "GroupA", ifelse(cluster1, "cluster1", ifelse(cluster2, "cluster2", ifelse(cluster3, "cluster3", "outlier"))))
raw.data$clusterGroupA <- ifelse(Group == "B", "GroupB", ifelse(cluster1A, "cluster1A", ifelse(cluster2A, "cluster2A", ifelse(cluster3A, "cluster3A", "outlier"))))
attach(raw.data)

## ---- basic_lm ----
lm1 <- lm(Sales_C ~ Group * Quota_C -1)
raw.data$predict.lm1 <- lm1$fitted.values
attach(raw.data)
round(coef(lm1),4)

## ---- Statistics ----
head(select(raw.data, Sales, Quota, Attainment, OnTarget, MetTarget, Group))
summary(select(raw.data, Sales, Quota, Attainment, OnTarget, MetTarget, Group))
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

## ---- Cluster_Ids ----
(clusterMembers <- list(
  "cluster1" = (1:422)[cluster1]
  , "cluster2" = (1:422)[cluster2]
  , "cluster3" = (1:422)[cluster3]
  , "cluster3A" = (1:422)[cluster3A]))
