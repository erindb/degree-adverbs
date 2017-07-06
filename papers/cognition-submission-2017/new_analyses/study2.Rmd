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

# install.packages("boot")
# install.packages("ordinal", dependencies=T)
# install.packages("languageR", dependencies=T)
# install.packages("mlogit", dependencies=T)

# load dependencies
library(boot)
library(ordinal)
# library(plyr)
library(languageR)
library(mlogit)
```

```{r}
## load data

myCenter <- function(x) {
  if (is.numeric(x)) { return(x - mean(x)) }
  if (is.factor(x)) {
    x <- as.numeric(x)
    return(x - mean(x))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m <- matrix(nrow=nrow(x), ncol=ncol(x))
    colnames(m) <- paste("c", colnames(x), sep="")
    for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) {
        y <- as.numeric(x[,i])
        m[,i] <- y - mean(y, na.rm=T)
      }
      if (is.numeric(x[,i])) {
        m[,i] <- x[,i] - mean(x[,i], na.rm=T)
      }
    }
    return(as.data.frame(m))
  }
}

# read in data
d = read.csv("../../../data/study2_data.csv")
total_ngrams = 1024908267229
ngrams = read.csv("web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))

df = d

list_a = c(
  "surpassingly",
  "astoundingly",
  "fantastically",
  "strikingly",
  "excessively",
  "markedly",
  "remarkably",
  "utterly",
  "truly",
  "particularly"
)
list_b = c(
  "colossally",
  "phenomenally",
  "mightily",
  "acutely",
  "extraordinarily",
  "amazingly",
  "terribly",
  "notably",
  "significantly",
  "quite"
)
list_c = c(
  "terrifically",
  "uncommonly",
  "supremely",
  "awfully",
  "exceedingly",
  "radically",
  "exceptionally",
  "incredibly",
  "totally",
  "especially"
)
list_d = c(
  "frightfully",
  "outrageously",
  "insanely",
  "decidedly",
  "intensely",
  "unusually",
  "desperately",
  "seriously",
  "extremely",
  "very"
)

total_workers = length(unique(df$workerid))
uncooperative = length(unique(subset(df, asses=="No")$workerid))
df = subset(df, asses == "Yes" | is.na(asses))
good_workers = length(unique(df$workerid))
print(total_workers)
print(uncooperative)
print(good_workers)
df = df[,c("workerid", "adverb", "ranking")]
df$adverb = as.character(df$adverb)
df$adjective = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][2])})
df$adverb = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][1])})
df$surprisal = sapply(df$adverb, function(adv) {return(ngrams$surprisal[adv == as.character(ngrams$ngram)][1])})
df$syllables = sapply(df$adverb, function(adv) {return(ngrams$syllables[adv == as.character(ngrams$ngram)][1])})
df$height_in_list = 9 - df$ranking
df$adverb_list = rep("A", nrow(df))
df$adverb_list[df$adverb %in% list_b] = "B"
df$adverb_list[df$adverb %in% list_c] = "C"
df$adverb_list[df$adverb %in% list_d] = "D"

## add predictor for surprisal that is the residual of surprisal after being predicted by syllables
m.resid = lm(surprisal~syllables,data=df)
df$resid_surprisal = resid(m.resid)

aggdf = ddply(df, .(adverb, adjective), function(subd) {
  resampled = boot(subd, function(orig, indices) {
    return( mean(orig[indices,]$height_in_list) )
  }, 100)$t
  newd = data.frame(
    adverb = subd$adverb[[1]],
    adjective = subd$adjective[[1]],
    surprisal = subd$surprisal[[1]],
    resid_surprisal = subd$resid_surprisal[[1]],
    syllables = subd$syllables[[1]],
    adverb_list = subd$adverb_list[[1]],
    height_in_list = mean(subd$height_in_list),
    height_in_list_high = quantile(resampled, 0.025),
    height_in_list_low = quantile(resampled, 0.975)
  )
  return(newd)
})
p = aggdf %>%
  mutate(syllables = as.numeric(syllables)) %>%
  ggplot(data=., aes(x=surprisal, y=height_in_list, colour=syllables)) +
  geom_smooth(method="lm", lwd=0, colour="gray", alpha=1/10) +
  geom_point(
    # size=1
  ) +
  geom_errorbar(aes(ymin=height_in_list_low, ymax=height_in_list_high, x=surprisal), width=0) +
  theme_bw() +
  facet_grid(. ~ adjective, scale="free") +
  xlab("surprisal") +
  ylab("height in list") +
  scale_colour_continuous(name="syllables") +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  ggtitle("Study 2") +
  #   scale_colour_brewer(type="div", palette=7) +
  #   scale_fill_brewer(type="div", palette=7) +
  scale_colour_gradient(low="gray", high="black") +
  theme(panel.grid=element_blank())
print(p)
ggsave("../edited_draft/images/plot_study2.pdf", width=10, height=3)

response_order = ddply(aggdf, .(adverb), summarise, height_in_list = mean(height_in_list))
aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$height_in_list)])

centered = cbind(df, myCenter(df[,c("surprisal","syllables","resid_surprisal")]))

sink(file="model_study2.txt")
df$ch = df$ranking + 1
df$chid = paste(df$adverb_list, df$workerid)
G <- mlogit.data(df, choice = "ch", shape = "long", chid.var="chid",
                 alt.var="adverb", ranked = TRUE)
model = mlogit(ch ~ surprisal + syllables | 0, G)
print(summary(model))
print(summary(mlogit(ch ~ surprisal + syllables + surprisal:adjective + syllables:adjective | 0, G)))
sink(NULL)

```
```{r}
df2 = df %>% group_by(adverb) %>%
  summarise(ranking=mean(ranking))
# df1b$intensifier[order(df1b$logprice)] %>%
df2 %>% write.csv("intensifiers_mean_logprice_study2.csv",
                   row.names=F)
```

```{r}
intensities = df %>% 
  rename(intensifier = adverb) %>%
  group_by(intensifier) %>%
  summarise(
    low = ci.low(height_in_list),
    high = ci.high(height_in_list),
    mean_height_in_list = mean(height_in_list)) %>%
  mutate(
    low = (low - 
      mean(mean_height_in_list))/sd(mean_height_in_list),
    high = (high - 
      mean(mean_height_in_list))/sd(mean_height_in_list),
    intensity = (mean_height_in_list -
      mean(mean_height_in_list))/sd(mean_height_in_list)) %>%
  select(intensifier, intensity, low, high)
intensities = intensities[order(intensities$intensity),]
write.csv(intensities, "intensities_study2.csv", row.names=F)
```