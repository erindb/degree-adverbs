### load required packages
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(rjson, quietly=T)
source("~/opt/r_helper_scripts/bootsSummary.r")

### load web ngrams data
web_1grams = read.table("web_1grams.csv", header=T, sep=",")
syllables = web_1grams$syllables
names(syllables) = web_1grams$ngram
ngrams = read.table("web_1grams.csv", header=T, sep=",")
frequencies = ngrams$frequency
names(frequencies) = as.character(ngrams$ngram)
total_ngrams = 1024908267229

# # Experiment 2
# 
### define different adjective sets
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
### load data
d = read.table("degree-adverbs-exp4.tsv", header=T, sep="\t")
d = subset(d, asses == "Yes")
d = d[,c("workerid", "trial", "adverb", "ranking")]
d$adjective_phrase = d$adverb
d$adverb = sapply(strsplit(as.character(d$adjective_phrase), " "), function(lst) {return(lst[[1]])})
d$adjective = sapply(strsplit(as.character(d$adjective_phrase), " "), function(lst) {return(lst[[2]])})
d$adverb_list = rep("A", nrow(d))
d$adverb_list[d$adverb %in% list_b] = "B"
d$adverb_list[d$adverb %in% list_c] = "C"
d$adverb_list[d$adverb %in% list_d] = "D"
frequencies = ngrams$frequency
names(frequencies) = ngrams$ngram
d$frequency = sapply(d$adverb, function(adverb) {return(frequencies[adverb])})
d$syllables = sapply(d$adverb, function(adverb) {return(syllables[adverb])})
d$surprisal = log(total_ngrams)-log(d$frequency)
d$chars = sapply(d$adverb, nchar)
d = ddply(d, .(workerid, adverb_list), transform, rank_order = rank(frequency))
d$ranking = 1-d$ranking+10
d$c.surprisal = d$surprisal - mean(d$surprisal)
d$c.syllables = d$syllables - mean(d$syllables)
d$c.chars = d$chars - mean(d$chars)

###diverging color scheme color brewer
d_summary = bootsSummary(data=d, measurevar="ranking",
                         groupvars=c("surprisal", "adjective", "syllables", "adverb_list"
                         ))
d_summary$syllables = as.factor(d_summary$syllables)
d_summary = ddply(d_summary, .(adverb_list, adjective), transform, adv_adj_N = sum(N))
d_summary$adjsyll = paste(d_summary$adjective, d_summary$syllables)
p = ggplot(data=d_summary, aes(x=surprisal, y=ranking, colour=syllables)) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal), width=0.3) +
  theme_bw(10) +
  scale_colour_brewer(type="div", palette=7) +
  facet_grid(~ adjective) +
  theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  xlab("surprisal") +
  ylab("ranking") +
  ggtitle("Experiment 2: length and surprisal predict degree for different adjectives")
print(p)
ggsave("exp2.pdf", width=8.5, height=3)

library(MASS)
library("AER")
d$franking = ordered(d$ranking)
m <- polr(franking ~ c.surprisal * c.syllables, data=d)
# m <- polr(franking ~ c.surprisal * c.syllables + c.surprisal:adjective + c.syllables:adjective, data=d)
# m <- polr(franking ~ adverb + adjective:adverb, data=d)
coeftest(m) 
# 
# # ## calculate and store p values
#  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# # ## combined table
# # (ctable <- cbind(ctable, "p value" = p))