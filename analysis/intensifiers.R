intensifiers = read.table("intensifiers.tsv", header=T, sep="\t")[,c("price",
                                                                     "intensifier",
                                                                     "item",
                                                                     "workerid")]
#intensifiers = intensifiers[intensifiers$workerid != 39,]
intensifiers$workerid = as.factor(intensifiers$workerid)
orig_nrow = nrow(intensifiers)
deintensifiers = subset(intensifiers, intensifier %in% c("a bit", "a tad", "somewhat",
                                                         "sort of", "kind of", "moderately",
                                                         "slightly"))
simple_adjective = subset(intensifiers, intensifier == "")
intensifiers = subset(intensifiers, intensifier %in% c("exceedingly", "excessively",
                                                         "really", "enormously", "hugely",
                                                         "crazy", "insanely", "wildly",
                                                         "super", "horribly", "extremely",
                                                         "very", "vastly", "quite",
                                                         "terribly", "uncommonly"))
stopifnot(orig_nrow == nrow(deintensifiers) + nrow(intensifiers) + nrow(simple_adjective))
ngrams = read.table("ngrams.sep", header=T, sep="\t")
frequency = ngrams$frequency
names(frequency) = ngrams$ngram
syllables = ngrams$syllables
names(syllables) = ngrams$ngram
intensifiers$frequency = sapply(as.character(intensifiers$intensifier), function(adverb) {
  frequency[[adverb]]
})
deintensifiers$frequency = sapply(as.character(deintensifiers$intensifier), function(adverb) {
  frequency[[adverb]]
})
intensifiers$syllables = sapply(as.character(intensifiers$intensifier), function(adverb) {
  syllables[[adverb]]
})
deintensifiers$syllables = sapply(as.character(deintensifiers$intensifier), function(adverb) {
  syllables[[adverb]]
})
total_1grams = 1024908267229
intensifiers$logprob = log(intensifiers$frequency) - log(total_1grams)
deintensifiers$logprob = log(deintensifiers$frequency) - log(total_1grams)

library(ggplot2)

# plotdata = function(item) {
#   data = intensifiers[intensifiers$item == item,]
#   p = ggplot(data, aes(x=logprob, y=price, colour=factor(intensifier))) +
#     geom_point() +
#     geom_smooth(method="lm", se=T, colour="black") +
#     xlab("log probability of adverb") +
#     ylab("price") +
#     theme_bw()  +
#     theme(legend.position="none") +
#     ggtitle(item)
#   print(p)
# }
# plotdata("coffee maker")
# plotdata("watch")
# plotdata("laptop")

# fitdata = function(item) {
#   data = intensifiers[intensifiers$item == item,]
#   fit = lm(price ~ logprob * syllables, data=data)
#   print(fit)
#   print(anova(fit))
# }
# fitdata("laptop")

# source("~/opt/r_helper_scripts/bootsSummary.r")
# intensifiers_summary = bootsSummary(data=intensifiers, measurevar="price", groupvars=c("item", "intensifier", "syllables", "logprob"))
# library(ggplot2)
# p = ggplot(intensifiers_summary, aes(x=logprob, y=price, colour=intensifier)) +
#   geom_point() +
#   geom_smooth(method="lm", se=T, colour="black") +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=logprob)) +
#   facet_wrap(~ item)
# print(p)

fit = lm(price ~ logprob * syllables * item * workerid, data=intensifiers)
print(fit)
print(anova(fit))

library(lme4)
lmer()

# lmer(thatrt~cond_prob + (cond_prob | subject) + (1 | item), data=the_that, REML=F)

# fitdata = function(item) {
#   data = intensifiers[intensifiers$item == item,]
#   fit = lm(price ~ logprob * syllables, data=data)
#   print(fit)
#   print(anova(fit))
# }
# fitdata("laptop")