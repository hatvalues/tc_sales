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

## ---- expected_loglinear_increase_in_sales ----
set.seed(1004)
x <- seq(5, 60, 0.25)
y <- 9*log(x) + rnorm(length(x))
xyplot(y~x, alpha = 0.6
  , scales = list(tck = c(1, 0))
  , par.settings = MyLatticeTheme
  , strip = MyLatticeStrip
  , main = list("A more intuitive relationship of Sales to Quotas\n(Simulation)")
  , xlab = "Quota (1000's)"
  , ylab = "Sales (1000's)"
  , panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.loess(x, y, col = myPal[1], lwd = 4
                  , span = 0.25, degree = 2, ...)
  }
  )

## ---- ggplot_OnTarget_depends_Quota ----
gAQ <- ggplot(data = raw.data, aes(x = KQuota, y = OnTarget, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Attainment by Quota by Group with lm fit"
                   , x = "Quota (1000's)", y = "On Target %")) +
  theme_bw() + myGgTheme
gAQ

## ---- ggplot_pred_lm1 ----
gLM1 <- ggplot(data = raw.data, aes(x = predict.lm1, y = OnTarget, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Fitted Values vs Actual"
            , x = "Fitted Values", y = "On Target %")) +
  theme_bw() + myGgTheme
gLM1

## ---- ggplot_missing_var ----
gMVar <- ggplot(data = raw.data, aes(x = KQuota, y = rollingSD, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(span = 0.75, degree = 1, size = 1.25, se = FALSE) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Heteroskedastic Trend by Quota by Group"
                   , x = "Quota (1000's)", y = "Missing variable - falling variance")) +
  theme_bw() + myGgTheme
gMVar

## ---- ggplot_pred_lm.mvar ----
gLMVar <- ggplot(data = raw.data, aes(x = predict.lm.mvar, y = OnTarget, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Fitted Values vs Actual (model includes derived variable)"
            , x = "Fitted Values", y = "On Target %")) +
  theme_bw() + myGgTheme
gLMVar

## ---- ggplot_cluster_analysis ----
gclus <- ggplot(data = raw.data, aes(x = rollingSD, y = OnTarget)) +
  geom_point(data = raw.data[(1:422)[cluster1],], aes(size = Quota), colour = myPalContrasts[2]) +
  geom_point(data = raw.data[(1:422)[cluster2],], aes(size = Quota), colour = myPalContrasts[6]) +
  geom_point(data = raw.data[(1:422)[cluster3],], aes(size = Quota), colour = myPalContrasts[9]) +
  #geom_hline(yintercept = c(clusterOneBoundary, clusterTwoBoundary)) +
  labs(list(title = "Analysis of Group B clusters"
            , x = "Mystery Variable", y = "On Target %")) +
  theme_bw() + myGgTheme
gclus