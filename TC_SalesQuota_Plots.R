# ---- boxplot_MetTarget ----
bwplot(KQuota ~ factor(MetTarget) | Group
       , scales = list(relation = "free", format = list(digits = 10))
       , par.settings = MyLatticeTheme
       , strip = MyLatticeStrip
       , xlab = "Met target"
       , ylab = "Quota (1000's units)"
       , main = "Group performance to targets"
       , panel = function(x, y, ...) {
          panel.bwplot(x, y, ...)
          panel.average(x, y, lwd = 2, lty = 1, col = myPalDark[1], ...)
       }
)

# ---- densityplot_MetTarget ---
densityplot(~OnTarget, groups = Group
      #, col = myPalDark[c(1,5)]
      , bw = 3
      , kernel = "gaussian"
      , scales = list(tck = c(1, 0))
      , par.settings = MyLatticeTheme
      , strip = MyLatticeStrip
      , xlab = "To target %"
      , main = "Group performance to targets"
      , panel = panel.superpose
      , panel.groups = function(x, group.number, ...) {
        panel.densityplot(x, ...)
        panel.abline(v = c(clusterOneBoundary, clusterTwoBoundary)
                     , col = myPalContrasts[3])
      }
      , auto.key = list(text = levels(Group), col = myPalDark[c(1,5)]
                        , columns = 2, space = "top"
                        , lines = FALSE)

      )

## ---- ggplot_Sales_depends_Quota ----
# smooth line shows the strong trend between quota and sales
gSQ <- ggplot(data = raw.data, aes(x = KQuota, y = KSales, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  scale_colour_manual(values = myPalContrasts) +
  geom_abline(intercept=0, slope=1, linetype = "dotted", colour = myPalDark[2]) +
  labs(list(title = "Sales by Quota by Group \nwith lm fit and identity line"
                   , x = "Quota (1000's)", y = "Sales (1000's)")) +
  theme_bw() + myGgTheme
gSQ

## ---- expected_nonlinear_increase ----
set.seed(1004)
x <- KQuota #seq(5, 60, length.out = 422)
y <- (x/(1+x) + rnorm(length(x), sd = 0.0025)) * 40
xyplot(y~x, alpha = 0.6
  , scales = list(tck = c(1, 0))
  , par.settings = MyLatticeTheme
  , strip = MyLatticeStrip
  , main = list("A more intuitive relationship of Sales to Quotas\n(Simulation)")
  , xlab = "Quota (1000's)"
  , ylab = "Sales (1000's)"
  , panel = function(x, y, ...) {
      panel.xyplot(x, y, col = myPal[1], ...)
      panel.loess(x, y, col = myPal[5], lwd = 4
                  , span = 0.25, degree = 2, ...)
  }
  )

## ---- mediated_binomial_sales ----
set.seed(1004)
x <- KQuota
y <- (rbinom(422, round(Quota,0), p = 0.05) * 20 + 
  rnorm(422, sd = 2500))/1000
xyplot(y~x, alpha = 0.6
       , scales = list(tck = c(1, 0))
       , par.settings = MyLatticeTheme
       , strip = MyLatticeStrip
       , main = list("Sales modelled as a mediated binomial\n(Simulation)")
       , xlab = "Quota (1000's)"
       , ylab = "Sales (1000's)"
       , panel = function(x, y, ...) {
         panel.xyplot(x, y, col = myPal[1], ...)
         panel.loess(x, y, col = myPal[5], lwd = 4
                     , span = 0.25, degree = 2, ...)
       }
)


## ---- ggplot_pred_lm1 ----
gLM1 <- ggplot(data = raw.data
               , aes(x = predict.lm1, y = (Sales_C-predict.lm1))) +
  geom_point(alpha = I(0.75), colour = myPal[1]) +
  geom_smooth(span = 0.75, degree = 1, size = 1.25
              , se = FALSE, colour = myPal[5]) +
  labs(list(title = "Fitted vs Residuals"
            , x = "Fitted Values", y = "Residuals")) +
  theme_bw() + myGgTheme
gLM1

## ---- ggplot_OnTarget_depends_Quota ----
gAQ <- ggplot(data = raw.data, aes(x = KQuota, y = OnTarget, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  geom_abline(intercept=35, slope=-(2/5), linetype = "dashed", colour = myPal[3]) +
  geom_abline(intercept=-32, slope=1/3, linetype = "dashed", colour = myPal[3]) +
  geom_point(aes(x= 10, y = 25), pch = 1, size = 30, colour = myPal[3]) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Attainment by Quota by Group with lm fit"
                   , x = "Quota (1000's)", y = "On Target %")) +
  theme_bw() + myGgTheme
gAQ

## ---- ggplot_missing_var ----
gMVar <- ggplot(data = raw.data, aes(x = KQuota, y = rollingSD, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(span = 0.75, degree = 1, size = 1.25, se = FALSE) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Mystery Variable derived from Heteroscedastic Trend"
                   , x = "Quota (1000's)", y = "Missing variable - decreasing variability")) +
  theme_bw() + myGgThemeSilentY
gMVar

gMVar <- ggplot(data = raw.data, aes(x = KQuota, y = oOrollingSD, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(span = 0.75, degree = 1, size = 1.25, se = FALSE) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = expression(paste("The new variable from ", frac(100,Mystery),"
        \"Certainty\" increases with Quota"))
            , x = "Quota (1000's)", y = "Certainty")) +
  theme_bw() + myGgThemeSilentY
gMVar

## ---- ggplot_pred_lm.mvar ----
gLMVar <- ggplot(data = raw.data
              , aes(x = predict.lm.mvar, y = (Sales_C-predict.lm.mvar))) +
  geom_point(alpha = I(0.75), colour = myPal[1]) +
  geom_smooth(span = 0.75, degree = 1, size = 1.25
              , se = FALSE, colour = myPal[5]) +
  labs(list(title = "Fitted vs Residuals, model including derived Certainty variable"
            , x = "Fitted Values", y = "Residuals")) +
  theme_bw() + myGgTheme
gLMVar

## ---- ggplot_mystery_var ----
gFMvar <- ggplot(data = subset(raw.data, clusterGroup != "outlier")
                , aes(x = oOrollingSD, y = OnTarget
                      , size = Quota, colour = Group)) +
  geom_point(alpha = I(0.75)) +
  geom_smooth(span = 0.75, degree = 1, size = 1.25, se = FALSE) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Effect of the isolated Certainty variable"
            , x = "Certainty", y = "On Target %")) +
  theme_bw() + myGgThemeSilentX
gFMvar

## ---- ggplot_mystery_varB ----
gFMvarB <- ggplot(data = subset(raw.data, Group != "A" & clusterGroup != "outlier")
                , aes(x = oOrollingSD, y = OnTarget
                      , size = Quota)) +
  xlim(4.2, 11.8) +
  geom_point(alpha = I(0.5), colour = myPalContrasts[2]) +
  geom_smooth(span = 0.75, degree = 1, size = 1
              , se = FALSE, colour = myPalContrasts[2]) +
  geom_vline(x = 7.8, linetype = "dashed", colour = myPal[3]) +
  labs(list(title = "Effect of the isolated Certainty - focus on Group B"
            , x = "Certainty", y = "On Target %")) +
  theme_bw() + myGgThemeSilentX
gFMvarB

## ---- ggplot_cluster_reprise ----
gD <- ggplot(data = subset(raw.data, Group != "A" & clusterGroup != "outlier")
             , aes(x = OnTarget, fill = clusterGroup)) +
  stat_bin(binwidth = (range(OnTarget)[2]-range(OnTarget)[1])/22
           , aes(yend = ..density..), geom = "density"
           , position = "dodge", alpha = 0.5) +
  scale_fill_manual(values = myPalContrasts[c(7,3,4)]) +
  coord_flip() +
  labs(list(title = "Density of sales agents levels of performance"
            , x = "On Target %", y = "Density (area of polygon proportional to group size)")) +
  theme_bw() + myGgTheme
gD

## ---- ggplot_cluster_analysis ----
gclus <- ggplot(data = subset(raw.data, Group != "A" & clusterGroup != "outlier")
                , aes(x = oOrollingSD, y = OnTarget
                      , colour = clusterGroup
                      , size = Quota)) +
  xlim(4.2, 11.8) +
  geom_point(alpha = I(0.75)) +
  geom_smooth(span = 0.8, degree = 1, size = 1, se = FALSE) +
  geom_vline(x = 7.8, linetype = "dashed", colour = myPal[3]) +
  scale_colour_manual(values = myPalContrasts[c(7,3,4)]) +
  labs(list(title = "Effect of the isolated Certainty - focus on Group B clusters"
            , x = "Certainty", y = "On Target %")) +
  theme_bw() + myGgThemeSilentX
gclus

## ---- bwplot_cluster_analysis ----
QuotaRanges <- matrix(NA, nrow = 3, ncol = 2)
QuotaRanges[1,] <- range(KQuota[clusterGroup == "cluster1"])/range(KQuota)
QuotaRanges[2,] <- range(KQuota[clusterGroup == "cluster2"])/range(KQuota)
QuotaRanges[3,] <- range(KQuota[clusterGroup == "cluster3"])/range(KQuota)
QuotaRanges[,1] <- QuotaRanges[,1]-1

bwplot(mvar_f~OnTarget | clusterGroup, data = raw.data %>%
         filter(Group != "A" & clusterGroup != "outlier")
      , box.ratio = 2
      , par.settings = MyLatticeTheme
      , strip = function(which.given, which.panel, factor.levels, ...) {
        panel.rect(0, 0, 1, 1
                   , col = myPalContrasts[c(7,3,4)][which.panel]
                   , border = myPal[2])
        panel.rect(QuotaRanges[which.panel, 1]+0.05, 0.05, QuotaRanges[which.panel, 2], 0.95
                   , col = myPal.range(100)[3]
                   , border = "transparent")
        panel.text(x = QuotaRanges[which.panel, 1]/2 + QuotaRanges[which.panel, 2]/2, y = 0.5
                   , col = myPalDark[2]
                   , lab = paste("Quota range", factor.levels[which.panel[which.given]]))
        panel.text(x = 1, y = 0.5, pos = 2
                   , col = myPalDark[2]
                   , lab = factor.levels[which.panel[which.given]])
      }
      , layout = c(1,3)
      , scales = list(relation = "free")
      , xlab = "To target %"
      , ylab = "Certainty"
      , main = "Effect of Certainty on performance to target - Group B clusters"
      , panel = function(x, y, ...) {
        panel.bwplot(x, y, ...)
        panel.average(x, y, lwd = 2, lty = 1, col = myPalDark[1], ...)
      })

## ---- bwplot_compare_analysis ----
QuotaBands <- equal.count(KQuota, number = 8)
bwplot(mvar_f~OnTarget | QuotaBands, data = raw.data %>%
         filter(Group == "A")
       , index.cond = list(c(3,2,1))
       , box.ratio = 2
       , par.settings = MyLatticeTheme
       , strip = MyLatticeStrip
       , layout = c(1,3)
       , scales = list(relation = "free")
       , xlab = "To target %"
       , ylab = "Certainty"
       , main = "Effect of Certainty on performance to target - Group A"
       , panel = function(x, y, ...) {
         panel.bwplot(x, y, ...)
         panel.average(x, y, lwd = 2, lty = 1, col = myPalDark[1], ...)
       })