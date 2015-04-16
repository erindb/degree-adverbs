### load required packages
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(rjson, quietly=T)
source("~/opt/r_helper_scripts/bootsSummary.r")

### load experiment data

d = read.table("degree-adverbs-exp5.tsv", header=T, sep="\t")
d = d[d$asses == "Yes",]
d$intensifier = as.character(d$adverb)
d = d[,c("workerid", "intensifier", "object", "response", "gender")]
d$workerid = as.factor(d$workerid)
d$logprice = log(d$response)

### load web ngrams data
web_1grams = read.table("web_1grams.csv", header=T, sep=",")
syllables = web_1grams$syllables
names(syllables) = web_1grams$ngram
ngrams = read.table("web_1grams.csv", header=T, sep=",")
frequencies = ngrams$frequency
names(frequencies) = as.character(ngrams$ngram)
d$frequency = sapply(d$intensifier, function(adverb) {return(frequencies[adverb])})
d$syllables = sapply(d$intensifier, function(adverb) {return(syllables[adverb])})
d$chars = sapply(d$intensifier, nchar)
total_ngrams = 1024908267229
d$surprisal = log(total_ngrams) - log(d$frequency)
d = d[d$surprisal != 0,]
d$c.surprisal = d$surprisal - mean(d$surprisal)
d$c.syllables = d$syllables - mean(d$syllables)
d$c.chars = d$chars - mean(d$chars)

# white = element_rect(fill="white")

d_summary = bootsSummary(data=d, measurevar="logprice", groupvars=c("intensifier", "object", "surprisal", "syllables"))
d_summary$syllables = as.ordered(d_summary$syllables)
p = ggplot(data=d_summary, aes(x=surprisal, y=logprice, colour=syllables)) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal), width=0.3) +
  theme_bw(10) +
  facet_wrap(~ object, scale="free") +
  xlab("surprisal") +
  ylab("log(price)") +
  ggtitle("Experiment 1: length and surprisal predict degree") +
  scale_colour_brewer(type="div", palette=7) +
  theme(panel.grid=element_blank())
print(p)
ggsave("exp1.pdf", width=8.5, height=3)

library(lmerTest, quietly=T)
full_model = lmer(logprice ~ c.surprisal * c.syllables +
                        (1 + c.surprisal + c.syllables | workerid) +
                        (1 + c.surprisal + c.syllables | object), data=d)
print(summary(full_model))

full_model = lmer(logprice ~ c.surprisal * c.chars +
                        (1 + c.surprisal + c.chars | workerid) +
                        (1 + c.surprisal + c.chars | object), data=d)
print(summary(full_model))