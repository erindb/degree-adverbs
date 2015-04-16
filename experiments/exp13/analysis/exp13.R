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

d = read.table("degree-adverbs-exp13.csv", sep=",", header=T)
d = subset(d, asses != "No")
d$nonce_word = d$Answer.nonce_word
d$length = d$Answer.nonce_length
d = subset(d, select=c("workerid", "adverb", "object", "response", "length", "nonce_word"))
d$adverb = as.character(d$adverb)
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
d$log.response = log(d$response)
total_ngrams = 1024908267229
d$surprisal = - log( d$freq / total_ngrams)

d$nonce = d$adverb %in% c("bugornly", "lopusly", "ratumly", "tupabugornly", "fepolopusly", "gaburatumly")

d_existing = subset(d, !nonce)
d_nonce = subset(d, nonce)
d_nonce$root = NA
d_nonce$root[d_nonce$adverb %in% c("tupabugornly", "bugornly")] = "bugorn"
d_nonce$root[d_nonce$adverb %in% c("fepolopusly", "lopusly")] = "lopus"
d_nonce$root[d_nonce$adverb %in% c("gaburatumly", "ratumly")] = "ratum"

d_existing_summary = bootsSummary(data=d_existing, measurevar="log.response",
                                  groupvars=c("adverb", "object", "surprisal",
                                              "nonce", "syll"))
d_nonce_summary = bootsSummary(data=d_nonce, measurevar="log.response",
                               groupvars=c("adverb", "object", "length", "syll",
                                           "root"))
d_summary = bootsSummary(data=d, measurevar="log.response",
                         groupvars=c("adverb", "object", "nonce"))

## real words are within Ss and novel words are between subjects,
## so there are fewer novel words than real words. but they seem to be more spread out...
p = ggplot(data=d, aes(x=adverb, y=log.response, colour=nonce)) +
  geom_point(alpha=1/2) +
  facet_wrap(~ object, scale="free") +
  ggtitle("Experiment 3: all adverbs") +
  ylab("log of estimated price") +
  xlab("adverb") +
  theme_bw(10) +
  theme(panel.grid=element_blank()
        , axis.text.x=element_text(angle=-45, hjust=0)
  ) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("free_response_all_intensifiers.pdf", width=8.5, height=3.5)

## a graph that shows that length and surprisal predict degree
p = ggplot(data=d_existing_summary, aes(x=surprisal, y=log.response,
                                        colour=as.ordered(syll))) +
  geom_point(size=3) +
  geom_errorbar(aes(x=surprisal, ymin=bootsci_low, ymax=bootsci_high)) +
  facet_wrap(~ object, scale="free") +
  ggtitle("Experiment 3: length and surprisal of existing intensifiers predict degree") +
  ylab("log of price estimate") +
  xlab("negative log frequency of adverb") +
  theme_bw(10) +
  theme(panel.grid=element_blank()) +
#   scale_colour_discrete() +
  scale_colour_manual(values=rev(c("#d7191c", "#FD9934", "#FAFF41", "#6DD5E9", "#2c7bb6")),
                      name="adverb length in syllables")
print(p)
ggsave("exp3_replication.pdf", width=8.5, height=3)

## stats that show that length and surprisal predict degree

# library(lme4)
# library(lmerTest)
d_existing$c.surprisal = d_existing$surprisal - mean(d_existing$surprisal)
d_existing$c.syll = d_existing$syll - mean(d_existing$syll)
# m_existing = lm(log.response ~ c.syll * c.surprisal, data=d_existing)
# print(summary(m_existing))

# mixed_m_existing = lmer(log.response ~ c.syll * c.surprisal +
#                     ( 1 + c.surprisal + c.syll | workerid) +
#                     ( 1 + c.surprisal + c.syll | object), data=d_existing)
# print(summary(mixed_m_existing))

d_nonce$adverb = as.factor(d_nonce$adverb)

p = ggplot(data=d_nonce_summary, aes(x=root, y=log.response, fill=length)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(x=root, ymin=bootsci_low, ymax=bootsci_high),
                position=position_dodge(width = 0.90),
                width=0.1) +
  geom_text(aes(x=root, y=0.5, label=paste("N=", N, sep="")),
            position=position_dodge(width = 0.90), size=3) +
  facet_wrap(~ object, scale="free") +
  ggtitle("Experiment 3: length of novel intensifier predicts degree") +
  ylab("log of estimated price") +
  xlab("nonce word") +
  theme_bw(10) +
  theme(panel.grid=element_blank()
        #, axis.text.x=element_text(angle=-45, hjust=0)
        ) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("free_response_nonce_intensifiers.pdf", width=8.5, height=3.5)

d_nonce_summary_length = bootsSummary(data=d_nonce, measurevar="log.response", groupvars=c("length", "object"))
p = ggplot(data=d_nonce_summary_length, aes(x=length, y=log.response, fill=length)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=length, ymin=bootsci_low, ymax=bootsci_high), width=0.1) +
  geom_text(aes(x=length, y=0.5, label=paste("N=", N, sep="")), size=3) +
  facet_wrap(~ object, scale="free") +
  ggtitle("Experiment 3: length of novel intensifier predicts degree") +
  ylab("log of estimated price") +
  xlab("length of nonce word") +
  theme_bw(10) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("free_response_nonce_intensifiers_length.pdf", width=8.5, height=3)

library(lme4)
# library(lmerTest)
d_nonce$numeric_length = 1
d_nonce$numeric_length[d_nonce$length == "\"long\""] = 2
d_nonce$root = NA
d_nonce$root[d_nonce$adverb %in% c("tupabugornly", "bugornly")] = "bugorn"
d_nonce$root[d_nonce$adverb %in% c("fepolopusly", "lopusly")] = "lopus"
d_nonce$root[d_nonce$adverb %in% c("gaburatumly", "ratumly")] = "ratum"
fit = lmer(log.response ~ length + (1 + length | object) + (1 | root), data=d_nonce)
print(summary(fit))

print(t.test(log.response~length, data=d_nonce))