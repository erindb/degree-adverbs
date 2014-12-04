library(plyr)
library(lme4)
library(lmerTest)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

intensifiers = read.table("intensifiers.tsv", header=T, sep="\t")
intensifiers = subset(intensifiers, workerid < 40) #later workers are for later experiments
intensifiers$workerid = as.factor(intensifiers$workerid)
intensifiers = intensifiers[,c("price", "intensifier", "item", "workerid")]
intensifiers = ddply(intensifiers, .(workerid), transform, zprice=scale(price))
intensifiers = ddply(intensifiers, .(workerid, item), transform, zzprice=scale(price))
intensifiers = ddply(intensifiers, .(workerid, item), transform, zzlogprice=scale(log(price)))

intensifiers = intensifiers[!is.na(intensifiers$zzprice),]
intensifiers = intensifiers[intensifiers$intensifier != "",] #don't include no intensifier in the analysis, but do use it to zscore.
## ngrams
ngrams = read.table("ngrams.sep", header=T, sep="\t")
frequency = ngrams$frequency
names(frequency) = ngrams$ngram
syllables = ngrams$syllables
names(syllables) = ngrams$ngram
total_1grams = 1024908267229
intensifiers$frequency = sapply(as.character(intensifiers$intensifier), function(adverb) {
  frequency[[adverb]]
})
intensifiers$probability = intensifiers$frequency / total_1grams
intensifiers$surprisal = - log(intensifiers$probability)
intensifiers$syllables = sapply(as.character(intensifiers$intensifier), function(adverb) {
  if (adverb == "") {
    return(0)
  } else {
    return(syllables[[adverb]])
  }
})

##
# independent random effects implies that different people
# have roughly the same idea of how much a laptop as opposed
# to a watch costs (I think)
##

#independent intercept, slope, and random effects (.)
fit = lmer(log(price) ~ 1 + surprisal + (1 | workerid) + (1 | item) + (0 + surprisal | workerid) +
             (0 + surprisal | item), data=intensifiers)
summary(fit)

#independent random effects (.)
fit = lmer(log(price) ~ 1 + surprisal + (1 + surprisal | workerid) + (1 + surprisal | item), data=intensifiers)
summary(fit)

##
# nested random effects say that different people might have totally
# different beliefs about prices of objects (I think)
##

#nested random effects (***)
fit = lmer(log(price) ~ surprisal + (1 + surprisal| workerid/item), data=intensifiers)
fit = lmer(log(price) ~ surprisal + (1 + surprisal| workerid) + (1 + surprisal| workerid:item),
           data=intensifiers)
summary(fit)

fit = lmer(log(price) ~ surprisal + syllables + (1 + surprisal + syllables| workerid/item), data=intensifiers)
summary(fit)

#nested random effects with independent intercept and slope (***)
fit = lmer(log(price) ~ surprisal + (1 | workerid/item) + (0 + surprisal| workerid/item), data=intensifiers)
summary(fit)

## zz anova (***)
fit = lm(zzprice ~ surprisal * syllables, data=intensifiers)
print(summary(fit))

fit = lm(zzprice ~ frequency * syllables, data=intensifiers)
print(summary(fit))

## zzlog anova (***)
fit = lm(zzlogprice ~ surprisal * syllables, data=intensifiers)
print(summary(fit))

## graphing
zz_intensifiers_summary = bootsSummary(data=intensifiers, measurevar="zzprice",
                                       groupvars=c("item", "intensifier", "syllables", "surprisal"))
p = ggplot(zz_intensifiers_summary, aes(x=surprisal, y=zzprice, colour=syllables)) +
  geom_point() +
  geom_smooth(method="lm", se=T, colour="black") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal)) +
  #scale_colour_grey() +
  scale_colour_gradient(low="black", high="gray") +
  theme_bw(28)
print(p)

zzlog_intensifiers_summary = bootsSummary(data=intensifiers, measurevar="zzlogprice",
                                       groupvars=c("item", "intensifier", "syllables", "surprisal"))
p = ggplot(zzlog_intensifiers_summary, aes(x=surprisal, y=zzlogprice, colour=syllables)) +
  geom_point() +
  geom_smooth(method="lm", se=T, colour="black") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal)) +
  #scale_colour_grey() +
  scale_colour_gradient(low="black", high="gray") +
  theme_bw(28)
print(p)