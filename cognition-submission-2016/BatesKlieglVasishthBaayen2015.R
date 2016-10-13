# working through examples in their paper

# devtools::install_github("dmbates/RePsychLing")
library(RePsychLing)
library(multcomp)
library(lme4)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lmerTest)

## load intensifier data
unigrams = read.csv("web_1grams.csv")
total_ngrams = 1024908267229
freq = unigrams$frequency
syll = unigrams$syllables
names(freq) = names(syll) = unigrams$ngram

## load experiment data
df = read.csv("Experiment1.csv") %>%
  filter(language %in% c("english", "English", "ENGLISH") & asses == "Yes") %>%
  select(workerid, adverb, object, response) %>%
  mutate(workerid = factor(workerid)) %>%
  group_by(workerid, object) %>%
  mutate(z = as.numeric(scale(log(response))),
         z = ifelse(is.nan(z), 0, z)) %>%
  ungroup %>%
  mutate(freq = freq[as.character(adverb)],
         syll = syll[as.character(adverb)],
         freq.scaled = scale(freq),
         syll.scaled = scale(syll),
         logfreq = log(freq),
         logfreq.scaled = scale(logfreq),
         cost = -logfreq.scaled + syll.scaled)

adverb.means = df %>% group_by(adverb) %>%
  summarise(mean.z = mean(z))
df = mutate(df, adverb = factor(adverb, levels=adverb.means$adverb[order(adverb.means$mean.z)]))

m = lmer(z ~ 1 + syll.scaled*logfreq.scaled + (1|workerid) + (1|adverb), df)

## a plot of variation across intensifiers
ggplot(df, aes(adverb, z, group=paste(object, workerid))) +
  geom_line(alpha=1/10) +
  facet_wrap(~object) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

## a plot of variation across intensifiers
ggplot(df, aes(cost, z, group=paste(object, workerid))) +
  geom_line(alpha=1/10) +
  facet_wrap(~object) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

# for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x)$thetastar,.975)}

# zscore(log(price)) by "cost" plot
df %>% group_by(adverb, cost) %>%
  summarise(z.mean = mean(z),
            z.low = ci.low(z),
            z.high = ci.high(z)) %>%
  ggplot(., aes(x=cost, y=z.mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=z.low, ymax=z.high)) +
  geom_smooth()

## design is fully crossed
## all participants see all combinations of intensifier and object
df %>% group_by(adverb, object) %>%
  summarise(logfreq = logfreq[1], syll = syll[1], N = length(response)) %>%
  ggplot(., aes(x=logfreq, y=syll)) +
  geom_text(alpha=1/2, aes(label=N)) +
  facet_wrap(~object) +
  ggtitle("design")

model = df %>%
  mutate(surp = -logfreq.scaled, syll=syll.scaled, Z=z,
         I=adverb, O=object, P=workerid) %>%
  select(surp, syll, Z, I, O, P) %>%
  lmer(Z ~ 1 + surp*syll +
         (1 | P) +
         (1 | I), data=.)

coef.data = as.data.frame(confint(glht(model))$confint)
coef.data$Comparison = rownames(coef.data)

ggplot(coef.data, aes(x = Comparison, y = Estimate,
                      ymin = lwr, ymax = upr)) + 
  geom_errorbar(width=0) +
  geom_point() +
  geom_hline(yintercept = 0, colour="gray60")


