The following figure shows a histogram of the Attainment in each group. There is a noticeable skew to the Group B distribution.

```{r Histogram_Attainment}
# These groups have the same average attainment 
# but do they have the same distributions?
histogram(~OnTarget | Group, layout = c(1, 2), breaks = 20
          , main = "Histogram of group A and B attainment for comparison")
```

The following figure shows a Quantile-Quantile plot, reflecting the above mentioned skew.

```{r QQ_Group_Attainment}
# quantiles plot against T disp df = sample size -1.
qq(Group~OnTarget
   , type=c("p", "g")
   , main = "Q-Q Attainment Grp A vs B"
   , aspect = "xy"
)
```

Group B appear to be over represented in the lower attainment group. There must be correspondingly a small set of high performers who pull up the group average to resemble Group A's mean.