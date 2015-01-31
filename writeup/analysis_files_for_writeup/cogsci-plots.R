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
d$chars = sapply(d$intensifier, nchar)
total_ngrams = 1024908267229
d$surprisal = log(total_ngrams) - log(d$frequency)
d = d[d$surprisal != 0,]
d$c.surprisal = d$surprisal - mean(d$surprisal)
d$c.syllables = d$syllables - mean(d$syllables)
d$c.chars = d$chars - mean(d$chars)

# white = element_rect(fill="white")

d_summary = bootsSummary(data=d, measurevar="logprice", groupvars=c("intensifier", "object", "frequency", "syllables"))
d_summary$syllables = as.ordered(d_summary$syllables)
p = ggplot(data=d_summary, aes(x=-log(frequency), y=logprice, colour=syllables)) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=4) +
  theme_bw(22) +
  facet_wrap(~ object, scale="free") +
  #scale_colour_grey() +
  scale_colour_manual(values=c("#8c510a",
                        "#bf812d",
                        "#dfc27d",
                        "#80cdc1",
                        "#35978f",
                        "#01665e")) +
  ##scale_colour_brewer(type="div") +#, palette="PRGn") +
  #scale_colour_brewer(type="div", palette="PiYG") +
  theme(panel.grid=element_blank()) +
  xlab("inverse log(frequency)") +
  ylab("log(price)") +
  ggtitle("Experiment 1") +
  #ggtitle("how much does a ___ expensive object cost?") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=-log(frequency), width=0), lwd=1.5)
print(p)
ggsave("images/exp1-plot.png", width=16, height=6)

# library(lmerTest, quietly=T)
# full_model = lmer(logprice ~ c.surprisal * c.syllables +
#                         (1 + c.surprisal + c.syllables | workerid) +
#                         (1 + c.surprisal + c.syllables | object), data=d)
# print(summary(full_model))

# full_model = lmer(logprice ~ c.surprisal * c.chars +
#                         (1 + c.surprisal + c.chars | workerid) +
#                         (1 + c.surprisal + c.chars | object), data=d)
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
d$chars = sapply(d$adverb, nchar)
#d$surprisal = -log(d$frequency)
d = ddply(d, .(workerid, adverb_list), transform, rank_order = rank(frequency))
d$ranking = 1-d$ranking+10
d$c.surprisal = d$surprisal - mean(d$surprisal)
d$c.syllables = d$syllables - mean(d$syllables)
d$c.chars = d$chars - mean(d$chars)
# d_ranks = ddply(d, .(adverb_list, workerid), transform, surprisal_rank = rank(surprisal))
# d_ranks = ddply(d_ranks, .(adverb_list, workerid), transform, syllables_rank = rank(syllables))
# d_ranks$intensifier = d_ranks$adverb
# d_ranks$list = d_ranks$adverb_list
# d_ranks = d_ranks[,c("workerid", "intensifier", "ranking", "adjective", "list", "syllables", "syllables_rank", "surprisal", "surprisal_rank")]
# d = ddply(d, .(adverb_list), transform, surp.range = max(surprisal) - min(surprisal))
# d$surprisal_scaled = (d$surprisal / d$surp.range)
# d = ddply(d, .(adverb_list), transform, syll.range = max(syllables) - min(syllables))
# d$syllables_scaled = (d$syllables / d$syll.range)

###diverging color scheme color brewer
d_summary = bootsSummary(data=d, measurevar="ranking",
                         groupvars=c("surprisal", "adjective", "syllables"#, "adverb_list"
                                     ))
d_summary$syllables = as.factor(d_summary$syllables)
d_summary = ddply(d_summary, .(adverb_list, adjective), transform, adv_adj_N = sum(N))
d_summary$adjsyll = paste(d_summary$adjective, d_summary$syllables)
p = ggplot(data=d_summary, aes(x=surprisal, y=ranking, colour=syllables)) +
  geom_smooth(method="lm", colour="grey", alpha=1/10) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=surprisal, width=0), lwd=1.5) +
  theme_bw(22) +
  #scale_colour_grey() +
  #scale_colour_brewer(type="div") +
  scale_colour_manual(values=c("#8c510a",
                               "#bf812d",
                               "#dfc27d",
                               "#80cdc1",
                               "#35978f",
                               "#01665e")) +
  #facet_grid(adverb_list ~ adjective) +
  facet_grid(~ adjective) +
  geom_text(aes(label=adv_adj_N), x=10, y=1) +
  geom_text(label="N=", x=9, y=1) +
  theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  xlab("surprisal") +
  ylab("ranking") +
  ggtitle("Experiment 2")
print(p)
ggsave("images/exp2-plot.png", width=16, height=5)



# d_summary = bootsSummary(data=d, measurevar="ranking",
#                          groupvars=c("adverb", "adjective"#, "adverb_list"
#                          ))
# d_summary$adjective = as.factor(d_summary$adjective)
# d_summary$adverb(d_summary$adverb)
# p = ggplot(data=d_summary, aes(x=adjective, y=ranking, colour=adjective)) +
#   geom_point(size=4) +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=adjective, width=0), lwd=1.5) +
#   theme_bw(22) +
#   facet_wrap(~ adverb) +
#   theme(panel.grid=element_blank()) +
#   ylab("ranking")
# print(p)
# ggsave("adverb-adjective.png", width=20, height=20)




# ## random effect of adverb_list: what converges?
# 
# ## this is best, since some lists have low values, some don't, and some are more compressed than others. but it doesn't converge.
# random_list_all = lmer(ranking ~ c.surprisal * c.syllables +
# (1 + c.surprisal * c.syllables | adverb_list), data=d)
# 

# library(MASS)
# library("AER")
# d$franking = ordered(d$ranking)
# m <- polr(franking ~ c.surprisal * c.syllables, data=d)
# m <- polr(franking ~ c.surprisal * c.syllables + c.surprisal:adjective + c.syllables:adjective, data=d)
# m <- polr(franking ~ adverb + adjective:adverb, data=d)
# coeftest(m) 
# 
# # ## calculate and store p values
#  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# # ## combined table
# # (ctable <- cbind(ctable, "p value" = p))

# 
# 
# m <- polr(franking ~ c.surprisal * c.chars, data=d)
# ## calculate and store p values
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ## combined table
# (ctable <- cbind(ctable, "p value" = p))
# 
# #is this correct?
# m <- glmer(ranking ~ c.surprisal * c.syllables +
#              (1 + c.surprisal * c.syllables | adverb_list) +
#              (1 + c.surprisal * c.syllables | workerid), data = d,
#            family = poisson
#            #, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10
#            )

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


#### plotting results from cogsci-intensifiers.church

toplot = function(adjective, cost, vals, version) {
  weights = as.numeric(unlist(strsplit(vals, " ")))
  if (length(weights) %in% c(4,5)) {
    if (adjective == "prior") {
      values = c(-2, -1, 0, 1, 2)
    } else if (adjective == "no utterance" || length(weights) == 5) {
      values = c(0, 1, 2, -2, -1)
    } else {
      values = c(0, 1, 2, -1)
    }
  } else if (length(weights) %in% c(8, 9)) {
    if (adjective == "prior") {
      values = seq(-2, 2, 0.5)
    } else if (adjective == "no utterance" || length(weights) == 9) {
      values = c(0, 1, 2, -2, -1.5, -1, -0.5, 0.5, 1.5)
    } else {
      values = c(0, 1, 2, -1.5, -1, -0.5, 0.5, 1.5)
    }
  } else if (length(weights) %in% c(16, 17)) {
    if (adjective == "prior") {
      values = seq(-2, 2, 0.25)
    } else if (adjective == "no utterance" || length(weights) == 17) {
      values = c(0, 1, 2, -2, -1.75, -1.5, -1.25, -1, -0.75, -0.5, -0.25, 0.25, 0.5, 0.75, 1.25, 1.5, 1.75)
    } else {
      values = c(0, 1, 2, -1.75, -1.5, -1.25, -1, -0.75, -0.5, -0.25, 0.25, 0.5, 0.75, 1.25, 1.5, 1.75)
    }
  }
  return(data.frame(
    values = values,
    adjective=rep(adjective, length(values)),
    cost=factor(rep(cost, length(values)), levels=c("prior", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
    version=rep(version, length(values)),
    weights = as.numeric(unlist(strsplit(vals, " ")))))
}

distributions = rbind(
#   #jan 29 13:57
#   toplot("prior", "prior", "0.05448868 0.24420134 0.40261995 0.24420134 0.05448868", "gauss [-2, 2] disc5 alpha5")
#   , toplot("no utterance", "0", "0.43530257970609326 0.18658724171823646 0.020958804569442133 0.06948751398763028 0.2876638600185978", "gauss [-2, 2] disc5 alpha5")
#   , toplot("tall", "1", "0.3270494786280969 0.41333849144913964 0.13181887357454736 0.008998925340252845 0.11879423100796317", "gauss [-2, 2] disc5 alpha5")
#   , toplot("tall", "2", "0.2626387879415386 0.4478836095380146 0.18318251357119658 0.007718952755902649 0.09857613619334767", "gauss [-2, 2] disc5 alpha5")
#   , toplot("tall", "10", "0.003191466620088624 0.008893826872849746 0.9865876811173894 0.00009888538468520658 0.001228140004987091", "gauss [-2, 2] disc5 alpha5")
#   #jan 29 13:58
#   , toplot("prior", "prior", "0.05448868 0.24420134 0.40261995 0.24420134 0.05448868", "gauss [-2, 2] disc5 alpha1")
#   , toplot("no utterance", "0", "0.41420588037323036 0.2076701921600446 0.031017151483932724 0.0677272092146356 0.2793795667681568", "gauss [-2, 2] disc5 alpha1")
#   , toplot("tall", "1", "0.36737815255979017 0.37077381246657537 0.12800037250415214 0.009195142962083437 0.12465251950739899", "gauss [-2, 2] disc5 alpha1")
#   , toplot("tall", "2", "0.3528977281751636 0.3653039471553193 0.15032818315048777 0.009275850738782577 0.12219429078024681", "gauss [-2, 2] disc5 alpha1")
#   , toplot("tall", "10", "0.3264868659608198 0.3473710618756477 0.20222836918241346 0.008946330711566483 0.11496737226955252", "gauss [-2, 2] disc5 alpha1")
  #jan 29 14:06
  toplot("prior", "prior", "0.05448868 0.24420134 0.40261995 0.24420134 0.05448868", "5 steps") # "gauss [-2, 2] disc5 alpha5 costs'")
  #, toplot("no utterance", "0", "0.42781611835520666 0.2060026902700605 0.023836692380713273 0.06547364599994512 0.2768708529940744", "gauss [-2, 2] disc5 alpha5 costs'")
  , toplot("tall", "1", "0.31802024209608015 0.4257085232776957 0.13444073267093312 0.00844166473625503 0.11338883721903598", "5 steps") # "gauss [-2, 2] disc5 alpha5 costs'")
  , toplot("tall", "5", "0.08308256369739267 0.24910325768382513 0.6340837531373683 0.0024610258837809546 0.03126939959763289", "5 steps") # "gauss [-2, 2] disc5 alpha5 costs'")
  , toplot("tall", "10", "0.0029309197588193382 0.009175147972457389 0.9867023123654616 0.00008706424969266218 0.001104555653568966", "5 steps") # "gauss [-2, 2] disc5 alpha5 costs'")
  #jan 29 14:29
  , toplot("prior", "prior", "0.02763055 0.06628225 0.12383154 0.18017382 0.20416369 0.18017382 0.12383154 0.06628225 0.02763055", "10 steps") # "gauss [-2,2] disc10 alpha5 cost'")
  #, toplot("no utterance", "0", "0.2151352540401715 0.10386250131232234 0.011861360061815259 0.03356084971096254 0.07801019690159379 0.1410814460277745 0.1983205647085487 0.17544932471725452 0.042718502519556965", "gauss [-2,2] disc10 alpha5 cost'")
  , toplot("tall", "1", "0.16734023853515334 0.2161530491765571 0.07182857335779932 0.002372601549096956 0.017065530769186637 0.05313360204639666 0.10904491233061725 0.2128683893121966 0.15019310292299612", "10 steps") #"gauss [-2,2] disc10 alpha5 cost'")
  , toplot("tall", "5", "0.04542383356011561 0.14286417972442042 0.3131382762006483 0.0007173851754905679 0.00500695800573356 0.015114209278789394 0.03010002673393302 0.06398174510643774 0.3836533862144314", "10 steps")#"gauss [-2,2] disc10 alpha5 cost'")
  , toplot("tall", "10", "0.0034840463003352412 0.011840229214773616 0.8862275847419776 0.00005518279826404978 0.000384814848420239 0.0011606534158284687 0.0023096834662740553 0.004924307572764687 0.08961349764136219", "10 steps")# "gauss [-2,2] disc10 alpha5 cost'")
#   #jan 29 18:00
  , toplot("prior", "prior", "0.01396019 0.02230832 0.03348875 0.04722671 0.06256523 0.07786368 0.09103187 0.09997895 0.10315262 0.09997895 0.09103187 0.07786368 0.06256523 0.04722671 0.03348875 0.02230832 0.01396019", "20 steps")
  , toplot("tall", "1", "0.08541325173409721 0.10876047796440926 0.0365927590635325 0.001200873319040462 0.0038369418777076244 0.00863775930351805 0.016239739766914733 0.02690559484865023 0.04028190000660848 0.05537329266017255 0.07082973555286536 0.09824913297959181 0.10824277070239069 0.11281836596457408 0.09542586772278708 0.07597296317050323 0.055218573362636476", "20 steps")
  , toplot("tall", "5", "0.02582949912693236 0.08167549293861713 0.1672179370771686 0.00040250720461812575 0.001266997584580207 0.0028093571008392195 0.005201300356006562 0.008485247476693712 0.01251229419265927 0.016959902876156417 0.021459881210916597 0.030440387339488985 0.03696740375508694 0.05023648227942237 0.14190931050428257 0.19577101900038557 0.20085497997614538", "20 steps")
  , toplot("tall", "10", "0.0028586086376081515 0.010044756432336153 0.6000649904732097 0.00004466914419372943 0.00014054702755835353 0.00031150680322479014 0.0005764882718513264 0.0009400815015195266 0.0013856966504206516 0.0018775876872904429 0.0023751171783549636 0.003371476416985411 0.004110886905794369 0.005690271839378773 0.024028579076877325 0.07550844045183262 0.26667029550156374", "20 steps")
#   #jan 29 18:41
  , toplot("tall", "prior", "0.05448868 0.24420134 0.40261995 0.24420134 0.05448868", "varying by 2")
  , toplot("tall", "2", "0.235016228242437 0.5112306022299425 0.18449324246028845 0.06925992706733199", "varying by 2")
  , toplot("tall", "4", "0.12245113323750999 0.42350916301223324 0.41727526442165824 0.03676443932859866", "varying by 2")
  , toplot("tall", "6", "0.04323534794811911 0.16621013137083454 0.7775356707169948 0.013018849964051511", "varying by 2")
  , toplot("tall", "8", "0.010342802433312485 0.040377664732521246 0.9461638945394263 0.003115638294739952", "varying by 2")
  , toplot("tall", "10", "0.003308628783495237 0.012943854899158004 0.9827507792383131 0.0009967370790336554", "varying by 2")
#   #jan 29 13:34
  , toplot("prior", "prior", "0.05448868 0.24420134 0.40261995 0.24420134 0.05448868", "varying by 1")
  , toplot("tall", "1", "0.32943994265795357 0.4520042454097056 0.11832132943056005 0.1002344825017809", "varying by 1")
  , toplot("tall", "2", "0.26093381055396564 0.4875215511366466 0.16960592372288488 0.08193871458650288", "varying by 1")
  , toplot("tall", "3", "0.20116248718775018 0.48128443476564875 0.2534082705122773 0.0641448075343237", "varying by 1")
  , toplot("tall", "4", "0.15071483836800992 0.4132842527354916 0.3876403555715535 0.04836055332494483", "varying by 1")
  , toplot("tall", "5", "0.10214303755337104 0.29721626678797197 0.567787012060194 0.0328536835984631", "varying by 1")
  #jan 29 13:34
  , toplot("prior", "prior", "0.05448868 0.24420134 0.40261995 0.24420134 0.05448868", "1-6")
  , toplot("tall", "1", "0.3321617451493133 0.45496706035204326 0.1117731332765396 0.10109806122210385", "1-6")
  , toplot("tall", "2", "0.26336616514843947 0.490988954107065 0.16290995111038623 0.08273492963410913", "1-6")
  , toplot("tall", "3", "0.20332753467498185 0.48498629534675197 0.24682468461380094 0.06486148536446514", "1-6")
  , toplot("tall", "4", "0.15301830394892613 0.41802870196360187 0.37983316769101905 0.04911982639645293", "1-6")
  , toplot("tall", "5", "0.10548628316944204 0.3056813396398894 0.5548893807056846 0.033942996484983896", "1-6")
  , toplot("tall", "6", "0.06343700126232514 0.18810700287416313 0.7280252094192676 0.02043078644424421", "1-6")
#   #jan 29 13:34
#   , toplot("prior", "prior", "", "")
#   , toplot("no utterance", "0", "", "")
#   , toplot("tall", "1", "", "")
#   , toplot("tall", "2", "", "")
#   , toplot("tall", "10", "", "")
)

distributions$adjective = factor(distributions$adjective)
distributions$cost = factor(distributions$cost)
p = ggplot(distributions, aes(x=values, y=weights, colour=cost)) +
  geom_line(stat="identity", lwd=1.5) +
  facet_wrap(~ version) +
  ylab("L1(height | utterance)") +
  ## color = tall/short
  ## linetype = cost
  #scale_colour_brewer(type="qual") +
  xlab("normed heights") +
  #scale_colour_manual(values=c("black", "yellow", "cyan", "deeppink", "turquoise4", "magenta", "chartreuse4", "purple")) +
  #   ggtitle("") +
  xlab("heights") +
  theme_bw(22) +
  theme(panel.grid=element_blank())
print(p)
# 
distributions$adjective = factor(distributions$adjective)
distributions$cost = factor(distributions$cost, levels=c("prior", "1", "5", "10"))
p = ggplot(distributions[distributions$version == "20 steps",], aes(x=values, y=weights, colour=cost)) +
  geom_line(stat="identity", lwd=1.5) +
  ylab("L1(price | utterance)") +
  ## color = tall/short
  ## linetype = cost
  #xlab("normed heights") +
  scale_colour_brewer(type="qual", palette=6) +
  #scale_colour_brewer(type="qual", palette="Set1") +
  #scale_colour_brewer(type="seq", drop=F) +
  #scale_colour_manual(values=c("black", "yellow", "cyan", "deeppink", "turquoise4", "magenta", "chartreuse4", "purple")) +
  #   ggtitle("") +
  xlab("standardiezed \"prices\"") +
  theme_bw(22) +
  theme(panel.grid=element_blank())
print(p)
ggsave("images/model_results.png", width=10, height=6)
# 

dists = distributions[distributions$version == "1-6" & distributions$cost!="prior",]
# #distributions = ddply(distributions, .(adjective, cost, version), transform, E=sum(weights * values))
expectations = ddply(dists, .(adjective, cost, version), summarize, E=sum(weights * values))
expectations$cost = as.numeric(as.character(expectations$cost))

p = ggplot(expectations, aes(x=cost, y=E)) +
  geom_point(size=4) +
  geom_line(lwd=2) +
  ylab("expected standardized price") +
  xlab("utterance cost") +
  #scale_colour_brewer(type="qual") +
  theme_bw(22) +
  theme(panel.grid=element_blank())
print(p)
ggsave("images/height-by-cost.png", width=10, height=6)