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
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal), width=0) +
  theme_bw(14) +
  scale_colour_manual(values=c("#8c510a",
                               "#bf812d",
                               "#dfc27d",
                               "#80cdc1",
                               "#35978f",
                               "#01665e")) +
  facet_grid(~ adjective) +
  theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  xlab("surprisal") +
  ylab("ranking") +
  ggtitle("Experiment 2")
print(p)
ggsave("exp2-plot.pdf", width=10, height=3)

library(lme4)
# source("~/opt/r_helper_scripts/rsquared.glmm.R")
full_model = lmer(ordered(ranking) ~ c.surprisal * c.syllables +
                    (1 + c.surprisal + c.syllables | workerid) +
                    (1 + c.surprisal + c.syllables | adjective), data=d)
# intercept_model = lmer(ordered(ranking) ~ c.surprisal + c.syllables +
#                          (1 + c.surprisal + c.syllables | workerid) +
#                          (1 + c.surprisal + c.syllables | adjective), data=d)
print(rsquared.glmm(list(full_model)))
library(MuMIn)
r.squaredGLMM(full_model)

res = data.frame(
  residual = residuals(full_model),
  predicted = fitted(full_model),
  actual = getME(full_model, name=c("y")),
  workerid = getME(full_model, name=c("flist"))$workerid,
  intensifier = d$adverb
)

res_summary = bootsSummary(data=res, measurevar="residual",
                           groupvars=c("intensifier"))
res_summary$intensifier = factor(as.character(res_summary$intensifier),
                                 levels=res_summary$intensifier[order(res_summary$residual)])
p = ggplot(data=res_summary, aes(x=intensifier, y=residual, fill=intensifier)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=intensifier, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  theme_bw(18) +
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle = 90, vjust = 0.5))
print(p)
ggsave("residuals.pdf", width=8.5, height=11)

p = ggplot(data=res, aes(x=predicted, y=actual, colour=intensifier)) +
  geom_point() +
  theme_bw(18) +
  theme(panel.grid=element_blank())
print(p)

p = ggplot(data=res, aes(x=predicted, y=actual, colour=workerid)) +
  geom_point() +
  theme_bw(18) +
  theme(panel.grid=element_blank())
print(p)

# library(MASS)
# library("AER")
# d$franking = ordered(d$ranking)
# m <- polr(franking ~ c.surprisal * c.syllables, data=d)
# # m <- polr(franking ~ c.surprisal * c.syllables + c.surprisal:adjective + c.syllables:adjective, data=d)
# # m <- polr(franking ~ adverb + adjective:adverb, data=d)
# coeftest(m) 


# 
# # ## calculate and store p values
#  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# # ## combined table
# # (ctable <- cbind(ctable, "p value" = p))