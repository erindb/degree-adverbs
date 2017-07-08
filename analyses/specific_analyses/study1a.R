---
title: "Intensifiers: Study 1a"
author: "Erin Bennett"
output: 
  html_document:
      toc: false
---

```{r global_options, include=FALSE}
rm(list=ls())
knitr::opts_chunk$set(echo=F, warning=F, cache=F, message=F, sanitiz =T, fig.width = 5, fig.height = 3)
```

```{r load_settings}
source("~/Settings/startup.R")
```

```{r}
## load intensifier data
unigrams = read.csv("web_1grams.csv")
total_ngrams = 1024908267229
freq = unigrams$frequency
syll = unigrams$syllables
names(freq) = names(syll) = unigrams$ngram

## load experiment data
raw.df = read.csv("../../../data/study1a_data.csv")
df = raw.df %>%
  filter(language %in% c(
    "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
    "ENGLISH", "ENG", "eng") &
      asses != "No") %>%
  mutate(price = response,
         intensifier = as.character(adverb)) %>%
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

```{r}
## colinearity
r = with(df, cor(surprisal.centered, syll.centered))
surp_by_syll = lm(surprisal.centered ~ syll.centered, df)
syll_by_surp = lm(syll.centered ~ surprisal.centered, df)
df = df %>% mutate(
  surprisal_resid = resid(surp_by_syll),
  syll_resid = resid(syll_by_surp))
```

Model:

```{r, echo=T}
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
# cor(df$logprice.scaled, fitted(m_colinear))^2
# cor(df$logprice.scaled, fitted(m_fixed_only))^2
# logLik(m_colinear)
# summary(m_colinear)
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
# anova(m_colinear)
lr_diff_due_to_syll = anova(m_colinear, m_only_surp)
lr_diff_due_to_surp = anova(m_colinear, m_only_syll)
```
```{r}
sink("study1a_models.txt")
cat("\n\n\n\n=========m_colinear===========\n\n")
summary(m_colinear)
cat("\n\n\n\n=========m_resid_surp===========\n\n")
summary(m_resid_surp)
cat("\n\n\n\n=========m_resid_syll===========\n\n")
summary(m_resid_syll)
cat("\n\n\n\n=========m_only_syll===========\n\n")
summary(m_only_syll)
cat("\n\n\n\n=========m_only_surp===========\n\n")
summary(m_only_surp)
cat("\n\n\n\n=========m_fixed_only===========\n\n")
summary(m_fixed_only)
cat("\n\n\n\n=========lr_diff_due_to_syll===========\n\n")
summary(lr_diff_due_to_syll)
cat("\n\n\n\n=========lr_diff_due_to_surp============\n\n")
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
report_chisq = function(df, chisq, p) {
  return(paste(
    "$\\chi^2(",
    # "df=",
    df,
    ")=",
    round(chisq, 3),
    ", ",
    ifelse(0==round(p, 4), "p<0.0005", paste("p=", round(p, 4), sep="")),
    "$",
    sep=""))
}
```

likeihood ratio test for surprisal effect beyond syllables effect: `r report_chisq(lr_diff_due_to_surp$Df[[2]], lr_diff_due_to_surp$Chisq[[2]], lr_diff_due_to_surp[["Pr(>Chisq)"]][[2]])`

likeihood ratio test for syllables effect beyond surprisal effect: `r report_chisq(lr_diff_due_to_syll$Df[[2]], lr_diff_due_to_syll$Chisq[[2]], lr_diff_due_to_syll[["Pr(>Chisq)"]][[2]])`

Check assumptions.

Normality:

```{r}
ggplot(NULL, aes(x=resid(m_colinear))) +
  geom_density()
```

Residuals as a function of predictors:

```{r}
ggplot(NULL, aes(x=df$surprisal.centered, y=resid(m_colinear))) +
  geom_point() +
  geom_smooth(method="loess")
ggplot(NULL, aes(x=df$syll.centered, y=resid(m_colinear))) +
  geom_point()
```

Check linearity:

```{r}
ggplot(NULL, aes(x=predict(m_colinear), y=df$logprice.scaled)) +
         geom_point() + geom_smooth(method="loess") +
         geom_smooth(method="lm")
ggplot(df, aes(x=1.33*syll.centered + 1.06*surprisal.centered, y=logprice.scaled)) +
         geom_point() + geom_smooth(method="loess") +
         geom_smooth(method="lm")
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
  ggtitle("Study 1a") +
  theme_bw()
# ggsave("../edited_draft/images/plot_study1b.pdf", width=8, height=2.5)
ggsave("../edited_draft/images/plot_study1a.pdf", width=5, height=3)
```


```{r}
df %>% group_by(intensifier) %>%
  summarise(freq=freq[[1]],
            syll=syll[[1]]) %>%
  write.csv("intensifiers_for_table.csv", row.names=F)
```

```{r}
intensities = df %>% group_by(intensifier) %>%
  summarise(
    low = ci.low(logprice.scaled),
    high = ci.high(logprice.scaled),
    mean_logprice = mean(logprice.scaled)) %>%
  mutate(
    low = low/sd(mean_logprice) - mean(mean_logprice),
    high = high/sd(mean_logprice) - mean(mean_logprice),
    intensity = mean_logprice/sd(mean_logprice) - mean(mean_logprice)) %>%
  select(intensifier, intensity, low, high)
intensities = intensities[order(intensities$intensity),]
write.csv(intensities, "intensities_study1a.csv", row.names=F)
```
