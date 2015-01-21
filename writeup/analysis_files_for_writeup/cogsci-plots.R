### load required packages
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(rjson, quietly=T)
source("~/opt/r_helper_scripts/bootsSummary.r")

### load experiment data

d = read.table("degree-adverbs-exp5.tsv", header=T, sep="\t")
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
total_ngrams = 1024908267229
d$surprisal = log(total_ngrams) - log(d$frequency)
d = d[d$surprisal != 0,]
d$c.surprisal = d$surprisal - mean(d$surprisal)
d$c.syllables = d$syllables - mean(d$syllables)

white = element_rect(fill="white")

d_summary = bootsSummary(data=d, measurevar="logprice", groupvars=c("intensifier", "object", "frequency", "syllables"))
d_summary$syllables = as.ordered(d_summary$syllables)
p = ggplot(data=d_summary, aes(x=-log(frequency), y=logprice, colour=syllables)) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=4) +
  theme_bw(22) +
  facet_wrap(~ object, scale="free") +
  scale_colour_grey() +
  theme(panel.grid=element_blank()) +
  xlab("inverse log(frequency)") +
  ylab("log(price)") +
  ggtitle("Experiment 1") +
  #ggtitle("how much does a ___ expensive object cost?") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=-log(frequency), width=0), lwd=1.5)
print(p)
ggsave("images/exp1-plot.png", width=16, height=6)

# library(lmerTest, quietly=T)
# d$c.surprisal = d$surprisal - mean(d$surprisal)
# d$c.syllables = d$syllables - mean(d$syllables)
# full_model = lmer(logprice ~ c.surprisal * c.syllables +
#                         (1 + c.surprisal + c.syllables | workerid) +
#                         (1 + c.surprisal + c.syllables | object), data=d)
# print(summary(full_model))

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
d = d[,c("workerid", "trial", "adverb", "ranking", "asses")]
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
#d$surprisal = -log(d$frequency)
d = ddply(d, .(workerid, adverb_list), transform, rank_order = rank(frequency))
d$ranking = 1-d$ranking+10
d$c.surprisal = d$surprisal - mean(d$surprisal)
d$c.syllables = d$syllables - mean(d$syllables)
# d_ranks = ddply(d, .(adverb_list, workerid), transform, surprisal_rank = rank(surprisal))
# d_ranks = ddply(d_ranks, .(adverb_list, workerid), transform, syllables_rank = rank(syllables))
# d_ranks$intensifier = d_ranks$adverb
# d_ranks$list = d_ranks$adverb_list
# d_ranks = d_ranks[,c("workerid", "intensifier", "ranking", "adjective", "list", "syllables", "syllables_rank", "surprisal", "surprisal_rank")]
# d = ddply(d, .(adverb_list), transform, surp.range = max(surprisal) - min(surprisal))
# d$surprisal_scaled = (d$surprisal / d$surp.range)
# d = ddply(d, .(adverb_list), transform, syll.range = max(syllables) - min(syllables))
# d$syllables_scaled = (d$syllables / d$syll.range)

d_summary = bootsSummary(data=d, measurevar="ranking",
                         groupvars=c("surprisal", "adjective", "syllables", "adverb_list"))
d_summary$syllables = as.factor(d_summary$syllables)
d_summary = ddply(d_summary, .(adverb_list, adjective), transform, adv_adj_N = sum(N))
d_summary$adjsyll = paste(d_summary$adjective, d_summary$syllables)
p = ggplot(data=d_summary, aes(x=surprisal, y=ranking, colour=syllables)) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal, width=0), lwd=1.5) +
  theme_bw(22) +
  scale_colour_grey() +
  facet_grid(adverb_list ~ adjective) +
  geom_text(aes(label=adv_adj_N), x=10, y=1) +
  geom_text(label="N=", x=9, y=1) +
  theme(panel.grid=element_blank()) +
  xlab("surprisal") +
  ylab("ranking") +
  ggtitle("Experiment 2")
print(p)
ggsave("images/exp2-plot.png", width=16, height=10)

# d_summary = bootsSummary(data=d, measurevar="ranking",
#                          groupvars=c("c.surprisal", "adjective", "c.syllables", "adverb_list"))
# d_summary$c.syllables = as.factor(d_summary$c.syllables)
# p = ggplot(data=d_summary, aes(x=c.surprisal, y=ranking, colour=c.syllables)) +
#   geom_point(size=4) +
#   geom_smooth(method="lm", se=F) +
#   theme_bw(22) +
#   facet_grid(adverb_list ~ adjective) +
#   theme(panel.grid=element_blank()) +
#   xlab("surprisal") +
#   ylab("ranking")
# print(p)

# 
# d_summary = bootsSummary(data=d, measurevar="height_in_list", groupvars=c("surprisal", "adjective", "adverb", "adverb_list", "syllables"))
# d_summary$syllables = as.factor(d_summary$syllables)
# p = ggplot(data=d_summary, aes(x=surprisal, y=height_in_list, colour=syllables)) +
#   geom_smooth(method="lm", colour="grey", alpha=1/10) +
#   geom_point(size=3) +
#   theme_bw(18) +
#   scale_colour_grey() +
#   facet_wrap(~ adjective) +
#   theme(panel.grid=element_blank()) +
#   ggtitle("how much does a ___ expensive object cost?") +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal, width=0), lwd=1.5)
# print(p)
# ggsave("images/exp2-plot.png", width=10, height=6)
# 
# d_summary = bootsSummary(data=d, measurevar="height_in_list", groupvars=c("surprisal", "adjective", "adverb", "adverb_list", "syllables"))
# d_summary$syllables = as.factor(d_summary$syllables)
# p = ggplot(data=d_summary, aes(x=surprisal, y=height_in_list, colour=syllables)) +
#   geom_smooth(method="lm", colour="grey", alpha=1/10) +
#   geom_point() +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal, width=0)) +
#   facet_wrap(~ adjective) +
#   theme_blackDisplay()
# print(p)
# ggsave("images/exp2-plot-presentation.png", width=10, height=6)
# ```
# 
# The same `r length(unique(d$adverb))` intensifiers were used.
# There were `r length(unique(d$workerid))` participants.
# 
# There were main effects of surprisal and syllable length, and a signficant interaction.
# 
# ```{r}


# print("overall")
# full_model = lm(height_in_list ~ c.surprisal * c.syllables, data=d)
# print(summary(full_model))
# 
# 
# print("overall")
# d$c.surprisal = d$surprisal - mean(d$surprisal)
# d$c.syllables = d$syllables - mean(d$syllables)
# full_model = lm(height_in_list ~ c.surprisal * c.syllables * adjective, data=d)
# print(summary(full_model))
# 
# for (adverb_list in unique(as.character(d$adverb_list))) {
#   print(adverb_list)
#   subd = d[d$adverb_list == adverb_list,]
#   subd$c.surprisal = subd$surprisal - mean(subd$surprisal)
#   subd$c.syllables = subd$syllables - mean(subd$syllables)
#   full_model = lm(ranking ~ c.surprisal * c.syllables, data=subd)
#   print(summary(full_model))
# }

## random adverb_list

# ## this is best, since some lists have low values, some don't, and some are more compressed than others. but it doesn't converge.
# random_list_all = lmer(ranking ~ c.surprisal * c.syllables + (1 + c.surprisal * c.syllables | adverb_list), data=d)
# 
# ## other options that don't converge:
# random_list_main = lmer(ranking ~ c.surprisal * c.syllables + (1 + c.surprisal + c.syllables | adverb_list), data=d)
# random_list_interaction = lmer(ranking ~ c.surprisal * c.syllables + (1 + c.surprisal:c.syllables | adverb_list), data=d)
# 
# ## this converges, but doesn't account for the lowest value being very different for the different lists
# random_list_slopes = lmer(ranking ~ c.surprisal * c.syllables + (0 + c.surprisal * c.syllables | adverb_list), data=d)

# ## doesn't converge:
# random_adj_all = lmer(ranking ~ c.surprisal * c.syllables + (1 + c.surprisal * c.syllables | adjective), data=d)
# 
# ## converge:
# random_adj_main = lmer(ranking ~ c.surprisal * c.syllables + (1 + c.surprisal + c.syllables | adjective), data=d)
# random_adj_interaction = lmer(ranking ~ c.surprisal * c.syllables + (1 + c.surprisal:c.syllables | adjective), data=d)
# random_adj_slopes = lmer(ranking ~ c.surprisal * c.syllables + (0 + c.surprisal * c.syllables | adjective), data=d)
# random_adj_intercept = lmer(ranking ~ c.surprisal * c.syllables + (1 | adjective), data=d)

fit = lmer(ranking ~ c.surprisal * c.syllables + (1 | adverb_list) + (1 + c.surprisal + c.syllables | adjective), data=d)
print(summary(fit))

# ```
# 
# The full model (with an interaction term) is again a better fit than the model without an interaction term.
# 
# ```{r}
# no_interaction_model = lm(height_in_list ~ c.surprisal + c.syllables, data=d)
# print(anova(no_interaction_model, full_model))
# ```
# 
# The graph again looks like there might be a multiplicative effect.
# 


### tall and expensive are (slightly) steeper (closer to x=y) than beautiful and old
# d_summary = bootsSummary(data=d, measurevar="height_in_list",
#                          groupvars=c("surprisal", "adjective"))
# d_summary$adjective = factor(d_summary$adjective,
#                              levels=c("tall", "expensive", "beautiful", "old"))
# p = ggplot(data=d_summary, aes(x=surprisal, y=height_in_list, colour=adjective)) +
#   geom_smooth(method="lm", alpha=1/30) +
#   theme_bw(22) +
#   #scale_colour_grey() +
#   theme(panel.grid=element_blank()) +
#   xlab("inverse log(frequency)") +
#   ylab("height in list") +
#   ggtitle("Experiment 2")
# print(p)
# ggsave("images/exp2-slopes.png", width=10, height=6)

# ## list-wise graphs of estimated coefficients
# coefs = data.frame(
#     surprisal=c(.46, .47, .30, .53),
#     surp.se=c(.081, .053, .081, .084),
#     syllables=c(.96, 1.3, .21, -1.3),
#     syll.se=c(.34, .11, .25, .27),
#     interaction=c(.25, .21, -.52, -.23),
#     int.se=c(.12, .04, .18, .10),
#     list=c("A", "B", "C", "D")
#   )
# p = ggplot(data=coefs, aes(x=list, y=surprisal, colour=list)) +
#   geom_point(size=4) +
#   geom_errorbar(aes(ymin=surprisal-surp.se, ymax=surprisal+surp.se, x=list, width=0), lwd=1.5) +
#   theme_bw(22) +
#   theme(panel.grid=element_blank()) +
#   xlab("list") +
#   ylab("coefficient for surprisal")
# print(p)
# p = ggplot(data=coefs, aes(x=list, y=syllables, colour=list)) +
#   geom_point(size=4) +
#   geom_errorbar(aes(ymin=syllables-syll.se, ymax=syllables+syll.se, x=list, width=0), lwd=1.5) +
#   theme_bw(22) +
#   theme(panel.grid=element_blank()) +
#   xlab("list") +
#   ylab("coefficient for syllables")
# print(p)
# p = ggplot(data=coefs, aes(x=list, y=interaction, colour=list)) +
#   geom_point(size=4) +
#   geom_errorbar(aes(ymin=interaction-int.se, ymax=interaction+int.se, x=list, width=0), lwd=1.5) +
#   theme_bw(22) +
#   theme(panel.grid=element_blank()) +
#   xlab("list") +
#   ylab("coefficient for interaction")
# print(p)
# ## approximate estimated model : total guess, using mostly lists A and B, which have wider ranges
# d$prediction = .45*d$c.surprisal + 0.5*d$c.syllables + .25*d$c.surprisal*d$c.syllables
# d = ddply(d, .(adverb_list, workerid), transform, ranking_prediction = rank(prediction))
# p = ggplot(data=d, aes(x=ranking_prediction, y=ranking)) +
#   geom_smooth(method="lm", colour="grey", alpha=1/10) +
#   geom_point(size=4, alpha=1/10) +
#   theme_bw(22) +
#   facet_grid(adverb_list ~ adjective) +
#   theme(panel.grid=element_blank()) +
#   xlab("ranking_prediction") +
#   ylab("ranking")
# print(p)
# ## model from intercept only mixed effects
# d$prediction = .46*d$c.surprisal + 0.68*d$c.syllables + .078*d$c.surprisal*d$c.syllables
# d = ddply(d, .(adverb_list, workerid), transform, ranking_prediction = rank(prediction))
# p = ggplot(data=d, aes(x=ranking_prediction, y=ranking)) +
#   geom_smooth(method="lm", colour="grey", alpha=1/10) +
#   geom_point(size=4, alpha=1/10) +
#   theme_bw(22) +
#   facet_grid(adverb_list ~ adjective) +
#   theme(panel.grid=element_blank()) +
#   xlab("ranking_prediction") +
#   ylab("ranking")
# print(p)
# p = ggplot(data=d, aes(x=prediction, y=ranking)) +
#   geom_smooth(method="lm", colour="grey", alpha=1/10) +
#   geom_point(size=4, alpha=1/10) +
#   theme_bw(22) +
#   facet_grid(adverb_list ~ adjective) +
#   theme(panel.grid=element_blank()) +
#   xlab("prediction") +
#   ylab("ranking")
# print(p)
# cor(d$ranking_prediction, d$ranking)

# d_summary = bootsSummary(data=d, measurevar="ranking",
#                          groupvars=c("c.surprisal", "c.syllables", "adverb_list"))
# d_summary = ddply(d_summary, .(adverb_list), transform, maxy=max(c.surprisal))
# d_summary = ddply(d_summary, .(adverb_list), transform, miny=min(c.surprisal))
# d_summary = ddply(d_summary, .(adverb_list), transform, maxx=max(c.syllables))
# d_summary = ddply(d_summary, .(adverb_list), transform, minx=min(c.syllables))
# p = ggplot(data=d_summary, aes(x=c.syllables, y=c.surprisal, colour=adverb_list, fill=adverb_list)) +
#   geom_point(size=4) +
#   theme_bw(22) +
#   theme(panel.grid=element_blank()) +
#   geom_rect(aes(ymax=maxy, ymin=miny, xmax=maxx, xmin=minx), alpha=1/300) +
#   xlab("centered syllables") +
#   ylab("centered surprisal")
# print(p)
d_summary = bootsSummary(data=d, measurevar="ranking",
                         groupvars=c("c.surprisal", "c.syllables", "adverb_list"))
#p = ggplot(data=d_summary, aes(xmin=c.syllables, xmax=c.syllables+1, ymin=c.surprisal, ymax=c.surprisal+1, colour=adverb_list, fill=adverb_list)) +
p = ggplot(data=d_summary, aes(xmin=c.syllables, xmax=c.syllables+1, ymin=c.surprisal, ymax=c.surprisal+0.1, colour=adverb_list, fill=adverb_list)) +
#p = ggplot(data=d_summary, aes(x=c.syllables, y=c.surprisal, colour=adverb_list)) +
  theme_bw(22) +
  facet_wrap(~ adverb_list) +
  theme(panel.grid=element_blank()) +
  #geom_point(size=10)
  geom_rect(alpha=1/100)
print(p)


d_summary = bootsSummary(data=d, measurevar="ranking",
                         groupvars=c("c.surprisal", "c.syllables", "adverb_list", "adverb"))
p = ggplot(data=d_summary, aes(fill=adverb_list)) +
  theme_bw(22) +
  geom_text(aes(label=adverb, x=c.syllables, y=c.surprisal, colour=adverb_list)) +
  #geom_rect(aes(xmin=c.syllables, xmax=c.syllables+1, ymin=c.surprisal, ymax=c.surprisal+0.01, fill=adverb_list), alpha=1/2) +
  theme(panel.grid=element_blank())
print(p)


# d_summary = bootsSummary(data=d_ranks, measurevar="ranking",
#                          groupvars=c("surprisal_rank", "adjective", "syllables_rank", "list"))
# p = ggplot(data=d_summary, aes(x=syllables_rank, y=surprisal_rank, colour=ranking)) +
#   geom_point(size=4) +
#   theme_bw(22) +
#   facet_grid(list ~ adjective) +
#   theme(panel.grid=element_blank()) +
#   xlab("syllables rank") +
#   ylab("surprisal rank") +
#   ggtitle("Experiment 2")
# print(p)

# d_summary = bootsSummary(data=d, measurevar="height_in_list", groupvars=c("surprisal", "syllables", "adjective"))
# d_summary$syllables = as.factor(d_summary$syllables)
# p = ggplot(data=d_summary, aes(x=surprisal, y=height_in_list, colour=syllables)) +
#   geom_smooth(method="lm", alpha=1/10) +
#   #geom_point(size=4) +
#   theme_bw(18) +
#   facet_wrap(~ adjective) +
#   theme(panel.grid=element_blank()) +
#   ggtitle("how much does a ___ expensive object cost?") #+
#   #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal, width=0), lwd=1.5)
# print(p)
# ```
# 
# # Experiment 3
# 
# 
# ```{r echo=F}
# ### load data
# 
# d = read.table("exp8.tsv", header=T, sep="\t")
# d$workerid = as.factor(d$workerid)
# impatient_workers = unique(d$workerid[sapply(d$workerid, function(worker) {
#   sum(as.logical(as.character(d$impatient[as.character(d$workerid) == worker])),
#       na.rm=T) > 1
# })])
# d = d[!(d$workerid %in% impatient_workers) &
#         !is.na(d$qtype) &
#         d$qtype != "summary",]
# d$target = d$Answer.target
# d = d[,c("workerid", "word", "qtype", "response", "target", "word_type")]
# d$training_condition = d$target
# d$response = as.numeric(as.character(d$response))
# 
# d_price = d[d$qtype == "price",]
# d_price$logprice = log(d_price$response)
# d_freq = d[d$qtype == "frequency",]
# 
# d_freq_vs_price = reshape(d[!(d$word %in% c("yep", "expensive", "bare")),],
#                           direction="wide", timevar="qtype",
#                           idvar=c("workerid", "word", "training_condition"))
# d_freq_vs_price$logprice = log(d_freq_vs_price$response.price)
# d_freq_vs_price$propto.surprisal = -log(d_freq_vs_price$response.frequency)
# 
# d_summary = bootsSummary(d_price, measurevar="response",
#                          groupvars=c("training_condition", "word"))
# d_freq = bootsSummary(d_freq, measurevar="response",
#                          groupvars=c("training_condition", "word"))
# 
# # d$word = as.character(d$word)
# # d$word[d$word == "bare"] = "other"
# # d$word[d$word == "yep"] = "other"
# # d$word[d$word == "expensive"] = "other"
# d_price_diff = reshape(d_price[,c("workerid", "response", "training_condition", "word")],
#                        direction="wide", timevar="word",
#                        idvar=c("workerid", "training_condition"))
# d_price_diff$diff.truly = d_price_diff$response.truly - d_price_diff$response.bare
# d_price_diff$diff.very = d_price_diff$response.very - d_price_diff$response.bare
# d_price_diff = d_price_diff[,c("workerid", "diff.truly", "diff.very", "training_condition")]
# d_price_diff = reshape(d_price_diff,
#                        direction="long", timevar="word",
#                        varying=c("diff.truly", "diff.very"),
#                        idvar=c("workerid", "training_condition"))
# 
# d_price_diff$frequency = mapply(function(word, workerid) {
#   return(
#     d$response[d$qtype == "frequency" &
#                       as.character(d$workerid) == workerid &
#                       d$word == word][1]
#     )}, d_price_diff$word, as.character(d_price_diff$workerid))
# 
# d_price_diff$word_type = rep("control", nrow(d_price_diff))
# d_price_diff$word_type[d_price_diff$word == d_price_diff$training_condition] = "target"
# d_diff_summary = bootsSummary(d_price_diff, measurevar="diff",
#                          groupvars=c("training_condition", "word"))
# d_diff_summary_type = bootsSummary(d_price_diff, measurevar="diff",
#                          groupvars=c("word_type", "word"))
# ```
# 
# There were `r length(unique(d$workerid))` participants.
# 
# Frequency estimates for an adverb were significantly higher when that adverb was the target (repeated) adverb.
# 
# ```{r}
# fit = lmer(response ~ word_type + (1 | word) + (1 | workerid), data=d[d$qtype == "frequency" & d$word_type != "yep",])
# print(summary(fit))
# 
# d_freq_summary = bootsSummary(d[d$qtype == "frequency" & d$word_type != "yep",], measurevar="response",
#                          groupvars=c("word_type", "word"))
# dodge = position_dodge(width=0.9)
# p = ggplot(data=d_freq_summary, aes(x=word_type, y=response, fill=word_type)) +
#   geom_bar(stat="identity", position=dodge) +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type),
#                 position=dodge, width=0.1) +
#   theme_bw(18) +
#   facet_wrap(~ word) +
#   xlab("word type") +
#   ylab("frequency estimate") +
#   theme(panel.grid=element_blank()) +
#   scale_fill_grey() +
#   ggtitle("frequency estimates in different training conditions")
# print(p)
# ggsave("images/exp3-freq-plot.png", width=10, height=6)
# 
# 
# p = ggplot(data=d_freq_summary, aes(x=word_type, y=response, fill=word_type)) +
#   geom_bar(stat="identity", position=dodge) +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type),
#                 position=dodge, width=0.1, colour="white") +
#   facet_wrap(~ word) +
#   xlab("word type") +
#   ylab("frequency estimate") +
#   theme_blackDisplay()
# ggsave("images/exp3-freq-plot-presentation.png", width=10, height=6)
# ```
# 
# Price estimates were significantly lower when the adverb was the target adverb.
# 
# ```{r}
# fit = lmer(diff ~ word_type + (1 | word) + (1 | workerid), data=d_price_diff)
# print(summary(fit))
# 
# dodge = position_dodge(width=0.9)
# p = ggplot(data=d_diff_summary_type, aes(x=word_type, y=diff, fill=word_type)) +
#   geom_bar(stat="identity", position=dodge) +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type),
#                 position=dodge, width=0.1) +
#   theme_bw(18) +
#   facet_wrap(~ word) +
#   xlab("word type") +
#   ylab("difference between target and bare price estimates") +
#   theme(panel.grid=element_blank()) +
#   scale_fill_grey() +
#   ggtitle("difference score of prices in different training conditions")
# print(p)
# ggsave("images/exp3-price-plot.png", width=10, height=6)
# 
# p = ggplot(data=d_diff_summary_type, aes(x=word_type, y=diff, fill=word_type)) +
#   geom_bar(stat="identity", position=dodge) +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type),
#                 position=dodge, width=0.1, colour="white") +
#   facet_wrap(~ word) +
#   xlab("word type") +
#   ylab("difference between target and bare price estimates") +
#   theme_black()
# ggsave("images/exp3-price-plot-presentation.png", width=10, height=6)
# ```
# 
# Price estimates correlated with frequency estimates.
# 
# ```{r}
# 
# fit = lm(response.price ~ response.frequency, data=d_freq_vs_price)
# print(summary(fit))
# p = ggplot(data=d_freq_vs_price, aes(x=-log(response.frequency), y=log(response.price))) +
#   geom_point(size=3) +
#   theme_bw(18) +
#   xlab("-log(frequency) from participants' frequency estimates") +
#   ylab("logprice") +
#   theme(panel.grid=element_blank()) +
#   geom_smooth(method="lm", colour="grey")
# print(p)
# ggsave("images/exp3-scatterplot.png", width=10, height=6)
# 
# 
# fit = lm(response.price ~ response.frequency, data=d_freq_vs_price)
# print(summary(fit))
# p = ggplot(data=d_freq_vs_price, aes(x=-log(response.frequency), y=log(response.price))) +
#   geom_point(size=3, colour="white") +
#   geom_smooth(method="lm", colour="grey") +
#   theme_blackDisplay() +
#   xlab("-log(frequency)") +
#   ylab("logprice")
# ggsave("images/exp3-scatterplot-presentation.png", width=10, height=6)
# 
# 
# with(d_freq_vs_price, cor.test(-log(response.frequency), log(response.price)), method="pearson")
# ```