---
title: "Intensifiers: Replication Experiment"
author: "Erin Bennett"
output: 
  html_document:
      toc: false
---

```{r global_options, include=FALSE}
rm(list=ls())
knitr::opts_chunk$set(echo=F, warning=F, cache=T, message=F, sanitiz =T, fig.width = 5, fig.height = 3)
```

```{r load_settings}
source("~/Settings/startup.R")
```

<!-- ## Experiment

Rating experiment.

## Planned analysis

### 1. Reformat data

We first excluded any participants who are not native speakers of English or who stated that they did not think they followed instructions.

People represent prices logarithmically, so we log-transform all of the prices that participants entered.

We don't care about modeling the prices of the individual objects. We think that this factor might interact with participants (different participants have different beliefs about price distributions for different objects), but we do not have enough data to fit an accurate mixed model with interacting random effects. So we z-score within each participant's ratings for each object (Thanks, Reviewer, for the suggestion!).
-->

```{r}
## load intensifier data
unigrams = read.csv("new-intensifiers-with-freq.csv")
total_ngrams = 1024908267229
freq = unigrams$frequency
syll = unigrams$syllables
names(freq) = names(syll) = unigrams$intensifier

## load experiment data
raw.df = read.csv("../../../data/study1b_data.csv")
df = raw.df %>%
  filter(language %in% c(
    "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
    "ENGLISH", "ENG", "eng") &
      assess != "No") %>%
  select(workerid, price, intensifier, object) %>%
  mutate(logprice = as.numeric(log(price))) %>%
  group_by(object, workerid) %>%
  mutate(logprice = as.numeric(log(price)),
         logprice.centered = logprice - mean(logprice),
         z = as.numeric(scale(log(price))),
         z = ifelse(is.nan(z), 0, z),
         logprice.scaled = z) %>%
  ungroup %>%
  mutate(freq = freq[as.character(intensifier)],
         syll = syll[as.character(intensifier)],
         syll.centered = syll - mean(syll),
         surprisal = -log(freq/total_ngrams),
         surprisal.centered = surprisal - mean(surprisal),
         syll.scaled = scale(syll),
         surprisal.scaled = scale(surprisal),
         word.cost = 0.18124*syll.centered + 0.10704*surprisal.centered)
```

<!-- Here's the original data. The slopes and intercepts vary by both participants and objects. In particular, watches seem to have larger slopes than laptops. -->

```{r}
# df %>% ggplot(., aes(x=word.cost,
#                      y=logprice,
#                      group=paste(workerid, object))) +
#   geom_line(stat="smooth",method = "lm", alpha = 0.1) +
#   facet_wrap(~object) +
#   geom_point(alpha=0.1)
# ggsave("raw-data.png", width=6, height=3)
```

<!-- And here's what the data look like when we z-score within each participant's responses for each object. Now each participant has an intercept of zero (kind of obviously...), but the slopes vary across participants. -->

```{r}
# df %>% ggplot(., aes(x=word.cost,
#                      y=logprice.scaled,
#                      group=paste(workerid, object))) +
#   geom_line(stat="smooth",method = "lm", alpha = 0.1) +
#   facet_wrap(~object) +
#   geom_point(alpha=0.1)
# ggsave("scaled-data.png", width=6, height=3)
```

<!-- ### 2. Model

Given that preprocessing involves normalizing responses from each participant for each item, we would use the following model, which includes a random slope for participant but no random intercept. We include random intercepts for intensifier (the particular words). We center the continuous predictors by subtracting their means. -->

Model:

```{r}
## colinearity
r = with(df, cor(surprisal.centered, syll.centered))
surp_by_syll = lm(surprisal.centered ~ syll.centered, df)
syll_by_surp = lm(syll.centered ~ surprisal.centered, df)
df = df %>% mutate(
  surprisal_resid = resid(surp_by_syll),
  syll_resid = resid(syll_by_surp))
```

```{r, echo=T}
## unable to evaluate scaled gradientModel...
m_colinear = lmer(logprice.scaled ~ 1 + syll.centered + surprisal.centered +
    (0 + syll.centered + surprisal.centered | workerid), df)
```

```{r}
m_resid_surp = lmer(logprice.scaled ~ 1 + syll.centered + surprisal_resid +
    (0 + syll.centered + surprisal_resid | workerid), df)
m_resid_syll = lmer(logprice.scaled ~ 1 + syll_resid + surprisal.centered +
    (0 + syll_resid + surprisal.centered | workerid), df)

## unable to evaluate scaled gradientModel...
m_only_syll = lmer(logprice.scaled ~ 1 + syll.centered +
    (0 + syll.centered + surprisal.centered | workerid), df)

## unable to evaluate scaled gradientModel...
m_only_surp = lmer(logprice.scaled ~ 1 + surprisal.centered +
    (0 + syll.centered + surprisal.centered | workerid), df)

m_fixed_only = lm(logprice.scaled ~ 1 + syll.centered + surprisal.centered, df)





m_colinear_with_randomI = lmer(logprice.scaled ~ 1 + syll.centered + surprisal.centered +
    (0 + syll.centered + surprisal.centered + intensifier | workerid), df)
m_resid_surp_with_randomI = lmer(logprice.scaled ~ 1 + syll.centered + surprisal_resid +
    (0 + syll.centered + surprisal_resid + intensifier | workerid), df)
m_resid_syll_with_randomI = lmer(logprice.scaled ~ 1 + syll_resid + surprisal.centered +
    (0 + syll_resid + surprisal.centered  + intensifier | workerid), df)

## unable to evaluate scaled gradientModel...
m_only_syll_with_randomI = lmer(logprice.scaled ~ 1 + syll.centered +
    (0 + syll.centered + surprisal.centered + intensifier | workerid), df)

m_only_surp_with_randomI = lmer(logprice.scaled ~ 1 + surprisal.centered +
    (0 + syll.centered + surprisal.centered + intensifier | workerid), df)
```

```{r}
report = function(m, regressor) {
  coefs = summary(m)$coefficients[regressor,]
  b = coefs[["Estimate"]]
  df = coefs[["df"]]
  t = coefs[["t value"]]
  p = coefs[["Pr(>|t|)"]]
  return(paste(
    "$b=",
    round(b, 3),
    ", t(",
    # "df=",
    round(df, 1),
    ")=",
    round(t, 3),
    ", ",
    ifelse(0==round(p, 4), "p<0.0005", paste("p=", round(p, 4), sep="")),
    # ifelse(p<0.001, "p<0.001" "p<0.05", paste("p=", round(p, 3), sep="")),
    "$",
    sep=""))
}
```

Colinear model:

* surprisal: `r report(m_colinear, "surprisal.centered")`
* syllables: `r report(m_colinear, "syll.centered")`

Surprisal and length in syllables are correlated predictors ($r=`r r`$). We focus on surprisal as our primary effect, but we are also interested in the independent contribution of length in syllables.

Model with syllables residualized (by surprisal):

* surprisal: `r report(m_resid_syll, "surprisal.centered")`
* syllables: `r report(m_resid_syll, "syll_resid")`

Model with surprisal residualized (by syllables):

* surprisal: `r report(m_resid_surp, "surprisal_resid")`
* syllables: `r report(m_resid_surp, "syll.centered")`

To address the independent effects of these variables, we ran model comparisons using log likelihood, leaving out each of the fixed effects but maintaining the full random effects structure. We also ran two additional mixed effects reggressions: one in which the suprisal is first residualized against syllables (using ordinary linear regression), and one where syllables is residualized against surprisal.

```{r}
require(lmtest)
m_simple = lmer(logprice.scaled ~ 1 +
    (0 | workerid) +
    (1 | intensifier), df)
m_syll = lmer(logprice.scaled ~ 1 + syll.centered +
    (0 + syll.centered | workerid) +
    (1 | intensifier), df)
m_surp = lmer(logprice.scaled ~ 1 + surprisal.centered +
    (0 + surprisal.centered | workerid) +
    (1 | intensifier), df)
lr_full = anova (m_colinear, m_simple)
lr_diff_due_to_syll = anova(m_colinear, m_only_surp)
lr_diff_due_to_surp = anova(m_colinear, m_only_syll)
```

```{r}
sink("study1b_models.txt")
summary(m_colinear)
cat("\n\n\n====================\n\n\n")
summary(m_resid_surp)
cat("\n\n\n====================\n\n\n")
summary(m_resid_syll)
cat("\n\n\n====================\n\n\n")
summary(m_only_syll)
cat("\n\n\n====================\n\n\n")
summary(m_only_surp)
cat("\n\n\n====================\n\n\n")
summary(m_fixed_only)
cat("\n\n\n====================\n\n\n")
summary(lr_full)
cat("\n\n\n====================\n\n\n")
summary(lr_diff_due_to_syll)
cat("\n\n\n====================\n\n\n")
summary(lr_diff_due_to_surp)

cat("\n\n\n\n=========m_colinear_with_randomI===========\n\n")
summary(m_colinear_with_randomI)
cat("\n\n\n\n=========m_resid_surp_with_randomI===========\n\n")
summary(m_resid_surp_with_randomI)
cat("\n\n\n\n=========m_resid_syll_with_randomI===========\n\n")
summary(m_resid_syll_with_randomI)
cat("\n\n\n\n=========m_only_syll_with_randomI===========\n\n")
summary(m_only_syll_with_randomI)
cat("\n\n\n\n=========m_only_surp_with_randomI===========\n\n")
summary(m_only_surp_with_randomI)
cat("\n\n\n\n=========m_fixed_only_with_randomI===========\n\n")
sink()
```

```{r}
report = function(df, chisq, p) {
  return(paste(
    "$\\chi^2(",
    # "df=",
    df,
    ")=",
    round(chisq, 3),
    ", ",
    ifelse(p<0.05, "p<0.05", paste("p=", round(p, 3), sep="")),
    "$",
    sep=""))
}
```


full likeihood ratio test: `r report(lr_full$Df[[2]], lr_full$Chisq[[2]], lr_full[["Pr(>Chisq)"]][[2]])`

likeihood ratio test for surprisal effect beyond syllables effect: `r report(lr_diff_due_to_surp$Df[[2]], lr_diff_due_to_surp$Chisq[[2]], lr_diff_due_to_surp[["Pr(>Chisq)"]][[2]])`

likeihood ratio test for syllables effect beyond surprisal effect: `r report(lr_diff_due_to_syll$Df[[2]], lr_diff_due_to_syll$Chisq[[2]], lr_diff_due_to_syll[["Pr(>Chisq)"]][[2]])`

<!-- We did not replicate our original model. -->

```{r}
# m.old = lmer(logprice ~ surprisal + syll +
#                (1 + surprisal + syll | workerid) +
#                (1 + surprisal + syll | object), data=df)
# summary(m.old)
```

```{r}
# get_coef = function(s) {
#   sub_data = df %>% filter(syll==s)
#   coefs = (sub_data %>%
#             lmer(logprice ~ 1 + surprisal.centered +
#                    (0 + surprisal.centered | workerid) +
#                    (1 | intensifier), .) %>%
#             summary())$coefficients
#   return(c(
#     syllables = s,
#     coef = coefs["surprisal.centered", "Estimate"],
#     se = coefs["surprisal.centered", "Std. Error"],
#     mean.surprisal = mean(sub_data$surprisal),
#     se.surprisal = sd(sub_data$surprisal)/sqrt(length(sub_data$surprisal)),
#     mean.logprice = mean(sub_data$logprice),
#     se.logprice = sd(sub_data$logprice)/sqrt(length(sub_data$logprice))
#   ))
# }
# syllables_coefs_data = as.data.frame(t(sapply(1:4, get_coef)))
# syllables_coefs_data %>%
#   ggplot(., aes(x=syllables, y=coef)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=coef-2*se, ymax=coef+2*se), width=0) +
#   ylab("surprisal coefficient")
# ggsave("surprisal-coefficient-by-syll.png", width=4, height=3)
# syllables_coefs_data %>%
#   ggplot(., aes(x=syllables, y=mean.logprice)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=mean.logprice-2*se.logprice,
#                     ymax=mean.logprice+2*se.logprice), width=0) +
#   ylab("mean log(price estimate)")
# ggsave("response-by-syll.png", width=4, height=3)
# syllables_coefs_data %>%
#   ggplot(., aes(x=syllables, y=mean.surprisal)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=mean.surprisal-(2*se.surprisal),
#                     ymax=mean.surprisal+(2*se.surprisal)), width=0) +
#   ylab("mean surprisal")
# ggsave("surprisal-by-syll.png", width=4, height=3)
# df %>% group_by(syll) %>%
#   summarise(n = length(unique(intensifier)))
```

Results:

```{r, width=10}
library(ggrepel)
df %>% group_by(intensifier, object) %>%
  summarise(surprisal.centered = surprisal.centered[[1]],
            syll = syll[[1]],
            logprice.scaled = mean(logprice.scaled)) %>%
  summarise(surprisal.centered = surprisal.centered[1],
            syllables = syll[1],
            low = ci.low(logprice.scaled),
            high = ci.high(logprice.scaled),
            logprice.scaled = mean(logprice.scaled)) %>%
  ggplot(., aes(x=surprisal.centered,
                y=logprice.scaled,
                colour=syllables)) +
  geom_smooth(method="lm", colour="gray", alpha=0.1) +
  geom_point() +
  geom_errorbar(aes(ymin=low, ymax=high)) +
  # geom_text_repel(aes(label=intensifier)) +
  # facet_wrap(~object, scale="free") +
  ylab("normalized log price") +
  scale_colour_gradient(low="gray", high="black") +
  xlab("centered surprisal") +
  ggtitle("Study 1b") +
  theme_bw()
# ggsave("../edited_draft/images/plot_study1b.pdf", width=8, height=2.5)
ggsave("../edited_draft/images/plot_study1b.pdf", width=5, height=3)
```
```{r}
df1b = df %>% group_by(intensifier) %>%
  summarise(logprice=mean(logprice))
# df1b$intensifier[order(df1b$logprice)] %>%
df1b %>% write.csv("intensifiers_mean_logprice_study1b.csv",
                   row.names=F)
```


```{r}
# intensifiers = df %>% group_by(intensifier) %>%
#   summarise(freq=freq[[1]],
#             syll=syll[[1]])
# intensifiers = intensifiers[order(intensifiers$freq),]
# intensifiers %>%
#   write.csv("intensifiers_for_table.csv", row.names=F)
```


```{r}
intensities = df %>% group_by(intensifier) %>%
  summarise(
    low = ci.low(logprice.scaled),
    high = ci.high(logprice.scaled),
    mean_logprice = mean(logprice.scaled)) %>%
  mutate(
    low = (low - mean(mean_logprice))/sd(mean_logprice),
    high = (high - mean(mean_logprice))/sd(mean_logprice),
    intensity = (mean_logprice - mean(mean_logprice))/sd(mean_logprice)) %>%
  select(intensifier, intensity, low, high)
intensities = intensities[order(intensities$intensity),]
write.csv(intensities, "intensities_study1b.csv", row.names=F)
```
