# ---- boxplot_MetTarget ----
bwplot(KQuota ~ factor(MetTarget) | Group
       , scales = list(relation = "free"
                       , format = list(digits = 10))
       , par.settings = MyLatticeTheme
       , strip = MyLatticeStrip
       , xlab = "Met target"
       , ylab = "Quota (1000's units)"
       , main = "Group performance to targets"
       , panel = function(x, y, ...) {
         panel.bwplot(x, y, ...)
         panel.average(x, y, lwd = 2, lty = 1, col = myPalDark[1], ...)
         panel.text(1:2,mns[,which.packet()] - 1.9, mns[,which.packet()]
                    , col = myPalDark[1], cex = 1.1, fontface = "bold", ...)
       }
)

# ---- densityplot_MetTarget ---
densityplot(~OnTarget, groups = Group
            , bw = 3
            , kernel = "gaussian"
            , scales = list(tck = c(1, 0))
            , par.settings = MyLatticeTheme
            , strip = MyLatticeStrip
            , xlab = "To target %"
            , main = "Group performance to targets"
            , auto.key = list(text = levels(Group), col = myPalDark[c(1,5)]
                              , columns = 2, space = "top"
                              , lines = FALSE)
)

## ---- moments_plots ----
qqmath(~OnTarget | Group
            , scales = list(tck = c(1, 0))
            , panel = function(x, ...) {
                c <- MyLatticeTheme$superpose.symbol$col[panel.number()]
                panel.qqmathline(x, ...)
                panel.qqmath(x, col = c, ...)
            }
            , par.settings = MyLatticeTheme
            , strip = MyLatticeStrip
            #, xlab = "To target %"
            , main = "QQ normal plot of performance to targets"
)
qq(Group~OnTarget
       , scales = list(tck = c(1, 0))
       , par.settings = MyLatticeTheme
       , strip = MyLatticeStrip
       #, xlab = "To target %"
       , main = "QQ Group A v Group B plot performance to targets"
       , auto.key = list(text = levels(Group), col = myPalDark[c(1,5)]
                         , columns = 2, space = "top"
                         , lines = FALSE)
)


# ---- oddsratioplot ----
plot(QT.OR, confidence = TRUE
     , conf_level = 0.9
     , col = myPalFourFold[3]
     , gp_bars = gpar(fill = myPalFourFold[4], alpha = 0.5)
     , gp_main = gpar(fontsize = 14)
     , main = "Odds Ratio plot for Quota Size / Met Target by Group"
     , ylab ="Odds Ratio"
     , ylim = c(0, 1))

# ---- fourfold_MetTarget ----
fourfold(QuotaTarget, conf_level = 0.9
         , color = myPalFourFold)

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

## ---- ggplot_OnTarget_depends_Quota ----
gAQ <- ggplot(data = raw.data, aes(x = KQuota, y = OnTarget, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  geom_abline(intercept=35, slope=-(2/5), linetype = "dashed", colour = myPal[3]) +
  geom_abline(intercept=-32, slope=1/3, linetype = "dashed", colour = myPal[3]) +
  geom_point(aes(x= 10, y = 25), pch = 1, size = 30, colour = myPal[3]) +
  geom_point(data = influential, aes(x = KQuota, y = OnTarget)
             , pch = 19, size = 3, colour = myPalContrasts[2]) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Attainment by Quota by Group with lm fit"
            , x = "Quota (1000's)", y = "On Target %")) +
  theme_bw() + myGgTheme
gAQ

## ---- ggplot_OnTarget_depends_Quota_B ----
raw.data.B <- raw.data[Group == "B", ]
raw.data.B <- raw.data.B[raw.data.B$KQuota != max(raw.data.B$KQuota),]
raw.data.B$Group <- factor(raw.data.B$Group, levels = c("A", "B"))
gAQB <- ggplot(data = raw.data.B, aes(x = KQuota, y = OnTarget, colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  geom_abline(intercept=35, slope=-(2/5), linetype = "dashed", colour = myPal[3]) +
  geom_abline(intercept=-32, slope=1/3, linetype = "dashed", colour = myPal[3]) +
  geom_point(aes(x= 10, y = 25), pch = 1, size = 30, colour = myPal[3]) +
  geom_point(data = misclass, aes(x = KQuota, y = OnTarget)
             , pch = 19, size = 3, colour = myPalContrasts[2]) +
  scale_colour_manual(values = myPalContrasts, drop = FALSE) +
  labs(list(title = "Attainment by Quota by Group with lm fit"
            , x = "Quota (1000's)", y = "On Target %")) +
  theme_bw() + myGgTheme
gAQB

## ---- ggplot_OnTarget_Loess ----
gMVar <- ggplot(data = raw.data, aes(x = KQuota, y = OnTarget
                                     , colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(method = "loess", se = TRUE
              , method.args = list(span = 0.75
                                   , degree = 1)
              , size = 1.25, alpha = 0.2) +
  geom_point(aes(x= 10, y = 25), pch = 1, size = 30, colour = myPal[3]) +
  geom_abline(intercept=35, slope=-(2/5), linetype = "dashed", colour = myPal[3]) +
  geom_abline(intercept=-32, slope=1/3, linetype = "dashed", colour = myPal[3]) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Attainment by Quota by Group with loess fit"
            , x = "Quota (1000's)", y = "On Target %")) +
  theme_bw() + myGgThemeSilentY
gMVar

## ---- ggplot_varInfluence_Loess ----
gMVar <- ggplot(data = raw.data, aes(x = KQuota, y = OnTarget
                                     , colour = Group)) +
  geom_point(alpha = I(0.5)) +
  geom_smooth(aes(y = sd_inflation), method = "loess"
              , se = FALSE
              , method.args = list(span = 0.75
                                   , degree = 1)
              , size = 1.25, alpha = 0.2) +
  scale_colour_manual(values = myPalContrasts) +
  labs(list(title = "Same as above with loess on contribution to st.dev"
            , x = "Quota (1000's)", y = "On Target %")) +
  theme_bw() + myGgThemeSilentY
gMVar

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