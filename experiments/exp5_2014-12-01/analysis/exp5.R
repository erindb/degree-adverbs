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
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal), width=0) +
  theme_bw(14) +
  facet_wrap(~ object, scale="free") +
  xlab("surprisal") +
  ylab("log(price)") +
  ggtitle("Experiment 1") +
  scale_colour_manual(values=c("#8c510a",
                               "#bf812d",
                               "#dfc27d",
                               "#80cdc1",
                               "#35978f",
                               "#01665e")) +
  theme(panel.grid=element_blank())
print(p)
ggsave("exp1-plot.pdf", width=8.5, height=3)

# library(lme4)
# # library(lmerTest, quietly=T)
# full_model = lmer(logprice ~ c.surprisal * c.syllables +
#                         (1 + c.surprisal + c.syllables | workerid) +
#                         (1 + c.surprisal + c.syllables | object), data=d)
# # intercept_model = lmer(logprice ~ c.surprisal + c.syllables +
# #                         (1 + c.surprisal + c.syllables | workerid) +
# #                         (1 + c.surprisal + c.syllables | object), data=d)
# library(MuMIn)
# r.squaredGLMM(full_model)

# # shuffled = sample(1:length(d$workerid), length(d$workerid))
# # half1 = shuffled[1:length(shuffled/2)]
# # half2 = shuffled[length(shuffled/2) + 1:length(shuffled)]
# # d$half[d$workerid %in% half1] = 1
# # d$half[d$workerid %in% half2] = 2
# # split_half_d = reshape(d, timevar="half", direction="wide", idvar=c("workerid", "object", "intensifier",
# #                                                                     "gender", "frequency", "syllables",
# #                                                                     "chars", "surprisal"))
# # split_half_model = lmer(logprice.1 ~ logprice.2 +
# #                           (1 | workerid) +
# #                           (1 | object), data=split_half_d)
# # print(rsquared.glmm(list(split_half_model)))
# 
# # print(rsquared.glmm(list(full_model)))
# # print(rsquared.glmm(list(intercept_model)))
# 
# ## since conditional is way higher than marginal,
# ## and workerids have a ginormous ceofficient relative to the rest,
# ## we can tell that the random effects explain way more of the
# ## variance than the fixed ones.
# 
# res = data.frame(
#   residual = residuals(full_model),
#   predicted = fitted(full_model),
#   fixed_predicted = getME(full_model, name=c("beta")),
#   actual = getME(full_model, name=c("y")),
#   workerid = getME(full_model, name=c("flist"))$workerid,
#   object = getME(full_model, name=c("flist"))$object,
#   intensifier = d$intensifier
# )
# 
# res_summary = bootsSummary(data=res, measurevar="residual",
#                            groupvars=c("intensifier"))
# res_summary$intensifier = factor(as.character(res_summary$intensifier),
#                                  levels=res_summary$intensifier[order(res_summary$residual)])
# p = ggplot(data=res_summary, aes(x=intensifier, y=residual, fill=intensifier)) +
#   geom_bar(stat="identity") +
#   geom_errorbar(aes(x=intensifier, ymin=bootsci_low, ymax=bootsci_high), width=0) +
#   theme_bw(18) +
#   theme(panel.grid=element_blank(),
#         axis.text.x=element_text(angle = 90, vjust = 0.5))
# print(p)
# ggsave("residuals.pdf", width=8.5, height=11)
# 
# p = ggplot(data=res, aes(x=predicted, y=actual, colour=intensifier)) +
#   geom_point() +
#   facet_grid(. ~ object) +
#   theme_bw(18) +
#   theme(panel.grid=element_blank())
# print(p)
# ggsave("fit_by_intensifier.pdf", width=8.5, height=6)
# 
# p = ggplot(data=res, aes(x=predicted, y=actual, colour=workerid)) +
#   geom_point() +
#   facet_grid(. ~ object) +
#   theme_bw(18) +
#   theme(panel.grid=element_blank())
# print(p)
# ggsave("fit_by_worker.pdf", width=8.5, height=6)
# 
# # print(getME(full_model, name=c("devcomp")))
# # 
# # r2.corr.mer <- function(m) {
# #   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
# #   summary(lmfit)$r.squared
# # }
# # 
# # r2.corr.mer(full_model)
# # 
# # 
# # prediction = ddply(.data=res, .(intensifier), summarise, prediction = mean(predicted))
# # res_summary$intensifier = factor(as.character(res_summary$intensifier),
# #                                  levels=prediction$intensifier[order(prediction$prediction)])
# # p = ggplot(data=res_summary, aes(x=intensifier, y=residual, fill=intensifier)) +
# #   geom_bar(stat="identity") +
# #   geom_errorbar(aes(x=intensifier, ymin=bootsci_low, ymax=bootsci_high), width=0) +
# #   theme_bw(18) +
# #   theme(panel.grid=element_blank(),
# #         axis.text.x=element_text(angle = 90, vjust = 0.5))
# # print(p)
# # ggsave("residuals_by_predicted.pdf", width=8.5, height=11)
# 
# # p = ggplot(data=res, aes(x=predicted, y=residual, colour=workerid)) +
# #   geom_point() +
# #   facet_grid(. ~ object) +
# #   theme_bw(18) +
# #   theme(panel.grid=element_blank())
# # print(p)
# # 
# # p = ggplot(data=res, aes(x=fixed_predicted, y=residual, colour=intensifier)) +
# #   geom_point() +
# #   facet_grid(. ~ object) +
# #   theme_bw(18) +
# #   theme(panel.grid=element_blank())
# # print(p)
# 
# print(summary(full_model))
# 
# ## for characters rather than syllables:
# # full_model = lmer(logprice ~ c.surprisal * c.chars +
# #                         (1 + c.surprisal + c.chars | workerid) +
# #                         (1 + c.surprisal + c.chars | object), data=d)
# # print(summary(full_model))