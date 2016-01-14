## ---- Initial_Load ----
# original link
link <- "https://raw.githubusercontent.com/julianhatwell/TC_SalesQuotas/master/TC_SalesQuota.csv"
# local file
#link <- "C:\\Dev\\Study\\R\\Tanc_investigation\\TC_SalesQuota.csv"
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
# Grp A
mn_Quo_A <- mean(KQuota[Group == "A"])
mn_Sal_A <- mean(KSales[Group == "A"])
mn_Tar_A <- mean(OnTarget[Group == "A"])
# Grp B
mn_Quo_B <- mean(KQuota[Group == "B"])
mn_Sal_B <- mean(KSales[Group == "B"])
mn_Tar_B <- mean(OnTarget[Group == "B"])
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
attach(raw.data)

## ---- group_B_cluster_params ----
lowOutlierBoundary <- -30
clusterOneBoundary <- -1
clusterTwoBoundary <- 15.5
highOutlierBoundary <- 40
cluster1 <- Group == "B" & 
  OnTarget < clusterOneBoundary & 
  OnTarget > lowOutlierBoundary
cluster2 <- Group == "B" & 
  OnTarget < clusterTwoBoundary & 
  OnTarget > clusterOneBoundary
cluster3 <- Group == "B" & 
  OnTarget < highOutlierBoundary & 
  OnTarget > clusterTwoBoundary

## ---- group_B_cluster_analysis ----
basicClustAnal <- data.frame(
    means = 
    c(mean(OnTarget[cluster1])
     , mean(OnTarget[cluster2])
     , mean(OnTarget[cluster3]))
   , members =
    c(sum(cluster1)
    , sum(cluster2)
    , sum(cluster3))
   , row.names = 
    c("Cluster.1"
    , "Cluster.2"
    , "Cluster.3"))
kable(basicClustAnal)

## ---- basic_lm ----
lm1 <- lm(OnTarget ~ Group * Quota_C -1)
raw.data$predict.lm1 <- lm1$fitted.values
attach(raw.data)
coef(lm1)
(list(root.mean.squared.error = sqrt(mean((OnTarget - lm1$fitted.values)^2))))

## ---- missing_var_from_variance ----
set.seed(103)
k <- 20
wndws <- cbind(lwr = 1:(n-k+1), idx = floor(k/2):(n-floor(k/2)), upr = k:n)

rsd <- as.numeric(rep(NA, n))
for (i in 1:dim(wndws)[1]) {
  rsd[wndws[i,"idx"]] <-  sd(raw.data$OnTarget[order(raw.data$Quota, decreasing = FALSE)][wndws[i,"lwr"]:wndws[i,"upr"]])
}
# bootstrap missing rolling window ends
missing <- matrix(nrow = 2, ncol = 3)
missing[1,] <- c(1
                 , wndws[1,"idx"]-1
                 , length(1:(wndws[1,"idx"]-1))
)
missing[2,] <- c(wndws[nrow(wndws)-floor(k/2),"idx"]+floor(k/2)+1
                 , n
                 , length((wndws[nrow(wndws)-floor(k/2),"idx"]+floor(k/2)+1):n)
)
# lower end
rsd[missing[1,1]:missing[1,2]] <- sample(rsd[(missing[1,2]+1):(missing[1,2]+1+k)], missing[1,3], replace = TRUE)
# upper end
rsd[missing[2,1]:missing[2,2]] <- sample(rsd[(missing[2,1]-1):(missing[2,1]-1-k)], missing[2,3], replace = TRUE)

rsd <- cbind(rsd, id = order(Quota))
raw.data$rollingSD <- rsd[order(rsd[,"id"]),"rsd"]
attach(raw.data)

## ---- mvar_lm ----
lm.mvar <- lm(OnTarget~Group * Quota_C + rollingSD -1)
raw.data$predict.lm.mvar <- lm.mvar$fitted.values
attach(raw.data)
coef(lm.mvar)
(list(root.mean.squared.error = sqrt(mean((OnTarget - lm1$fitted.values)^2))))

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
(clusterMembers <- list(
  "cluster1" = (1:422)[cluster1]
  , "cluster2" = (1:422)[cluster2]
  , "cluster3" = (1:422)[cluster3]))
