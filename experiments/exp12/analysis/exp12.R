library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

char = as.character
num = function(v) {return(as.numeric(as.character(v)))}
chop = function(s) {
  a = strsplit(s, "")[[1]]
  return(paste(a[2:length(a)], collapse=""))
}
cutquotes = function(s) {
  a = strsplit(s, "")[[1]]
  return(paste(a[2:(length(a)-1)], collapse=""))
}

d = read.table("degree-adverbs-exp12.csv", sep=",", header=T)
d = subset(d, asses != "No")

d = subset(d, select=c("workerid", "ranking", "adverb", "trial_time", "adjective", "length", "nonce_word"))
d$adjective_phrase = char(d$adverb)
d$adverb = sapply(d$adjective_phrase, function(str) {return(strsplit(str, " ")[[1]][[1]])})
d$freq = sapply(d$adverb, function(adverb) {
  return(c(
    "bugornly" = 0,
    "lopusly" = 0,
    "ratumly" = 0,
    "tupabugornly" = 0,
    "fepolopusly" = 0,
    "gaburatumly" = 0,
    "colossally" = 11167,
    "phenomenally" = 120769,
    "extraordinarily" = 900456,
    "amazingly" = 1384225,
    "terribly" = 1906059,
    "mightily" = 252135,
    "significantly" = 19939125,
    "notably" = 3141835,
    "quite" = 55269390
  )[adverb])
})
d$syll = sapply(d$adverb, function(adverb) {
  return(c(
    "bugornly" = 3,
    "lopusly" = 3,
    "ratumly" = 3,
    "tupabugornly" = 5,
    "fepolopusly" = 5,
    "gaburatumly" = 5,
    "colossally" = 4,
    "phenomenally" = 5,
    "extraordinarily" = 6,
    "amazingly" = 4,
    "terribly" = 3,
    "mightily" = 3,
    "significantly" = 5,
    "notably" = 3,
    "quite" = 1
  )[adverb])
})
d$logfreq = log(d$freq)
total_ngrams = 1024908267229
d$surprisal = - log( d$freq / total_ngrams)

d$nonce = d$adverb %in% c("bugornly", "lopusly", "ratumly", "tupabugornly", "fepolopusly", "gaburatumly")
d$height_in_list = 10 - d$ranking

d_existing = subset(d, !nonce)
d_nonce = subset(d, nonce)
d_nonce$root = NA
d_nonce$root[d_nonce$adverb %in% c("tupabugornly", "bugornly")] = "bugorn"
d_nonce$root[d_nonce$adverb %in% c("fepolopusly", "lopusly")] = "lopus"
d_nonce$root[d_nonce$adverb %in% c("gaburatumly", "ratumly")] = "ratum"

d_existing_summary = bootsSummary(data=d_existing, measurevar="height_in_list",
                                  groupvars=c("adverb", "surprisal",
                                              "nonce", "syll", "adjective"))
d_nonce_summary = bootsSummary(data=d_nonce, measurevar="height_in_list",
                               groupvars=c("adverb", "length", "syll",
                                           "root", "adjective"))
d_summary = bootsSummary(data=d, measurevar="height_in_list",
                         groupvars=c("adverb", "nonce", "adjective"))

p = ggplot(data=d_existing_summary, aes(x=surprisal, y=height_in_list, colour=factor(syll))) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal), width=0.3) +
  theme_bw(10) +
  facet_grid(~ adjective) +
  theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  xlab("surprisal") +
  ylab("ranking") +
  ggtitle("Experiment 4: length and surprisal predict degree for different adjectives") +
  scale_colour_manual(values=rev(c("#d7191c", "#FD9934", "#FAFF41", "#6DD5E9", "#2c7bb6")),
                      name="adverb length in syllables")
print(p)
ggsave("exp4_replication.pdf", width=8.5, height=4)

d_existing$c.surprisal = d_existing$surprisal - mean(d_existing$surprisal)
d_existing$c.syll = d_existing$syll - mean(d_existing$syll)

# m_existing = lm(log.response ~ c.syll * c.surprisal, data=d_existing)
# print(summary(m_existing))

library(MASS)
library("AER")
d_existing$fheight_in_list = ordered(d_existing$height_in_list)
m <- polr(fheight_in_list ~ c.surprisal * c.syll, data=d_existing)
coeftest(m)

# p = ggplot(data=d, aes(x=logfreq, y=height_in_list)) +
#   geom_point(alpha=1/10, size=3) +
#   geom_smooth(method="lm")
# print(p)

d_nonce$ordered_height_in_list = ordered(d_nonce$height_in_list)
d_nonce_summary = bootsSummary(data=d_nonce, measurevar="height_in_list",
                               groupvars=c("adverb", "length", "root"))
p = ggplot(data=d_nonce_summary, aes(x=root, y=height_in_list, fill=length)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(x=root, ymin=bootsci_low, ymax=bootsci_high), width=0.1,
                position=position_dodge(width=0.9)) +
  geom_text(aes(x=root, y=0.5, label=paste("N=", N, sep="")),
            position=position_dodge(width=0.9)) +
  ggtitle("Experiment 4: length of novel intensifier predicts degree") +
  ylab("height in ordered list") +
  xlab("nonce word root") +
  theme_bw(15) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("exp4.pdf", width=8.5, height=4)

m <- polr(ordered_height_in_list ~ length, data=d_nonce)
coeftest(m)

d_nonce_summary_length = bootsSummary(data=d_nonce, measurevar="height_in_list", groupvars=c("length"))
p = ggplot(data=d_nonce_summary_length, aes(x=length, y=height_in_list, fill=length)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=length, ymin=bootsci_low, ymax=bootsci_high), width=0.1) +
  geom_text(aes(x=length, y=0.5, label=paste("N=", N, sep="")), size=8) +
  ggtitle("Experiment 4: length of novel intensifier predicts degree") +
  ylab("height in ordered list") +
  xlab("length of nonce word") +
  theme_bw(18) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("exp4.pdf", width=8.5, height=5)

# require(foreign)
# require(ggplot2)
require(MASS)
# require(Hmisc)
# require(reshape2)

m <- polr(ordered_height_in_list ~ numeric_length, data = d, Hess=TRUE)
print(summary(m))

print(t.test(height_in_list~numeric_length, data=d))

require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)

d$root = NA
d$root[d$adverb %in% c("tupabugornly", "bugornly")] = "bugorn"
d$root[d$adverb %in% c("fepolopusly", "lopusly")] = "lopus"
d$root[d$adverb %in% c("gaburatumly", "ratumly")] = "ratum"

m <- glmer(height_in_list ~ length + (1 | root), data = d)
print(summary(m))