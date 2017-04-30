# load dependencies
library(ggplot2)
library(boot)
library(plyr)
library(ordinal)
library(languageR)

myCenter <- function(x) {
  if (is.numeric(x)) { return(x - mean(x)) }
  if (is.factor(x)) {
    x <- as.numeric(x)
    return(x - mean(x))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m <- matrix(nrow=nrow(x), ncol=ncol(x))
    colnames(m) <- paste("c", colnames(x), sep="")
    for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) {
        y <- as.numeric(x[,i])
        m[,i] <- y - mean(y, na.rm=T)
      }
      if (is.numeric(x[,i])) {
        m[,i] <- x[,i] - mean(x[,i], na.rm=T)
      }
    }
    return(as.data.frame(m))
  }
}

# read in data
d = read.csv("../data/Experiment2.csv")
total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))

df = d

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

total_workers = length(unique(df$workerid))
uncooperative = length(unique(subset(df, asses=="No")$workerid))
df = subset(df, asses == "Yes" | is.na(asses))
good_workers = length(unique(df$workerid))
print(total_workers)
print(uncooperative)
print(good_workers)
df = df[,c("workerid", "adverb", "ranking")]
df$adverb = as.character(df$adverb)
df$adjective = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][2])})
df$adverb = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][1])})
df$surprisal = sapply(df$adverb, function(adv) {return(ngrams$surprisal[adv == as.character(ngrams$ngram)][1])})
df$syllables = sapply(df$adverb, function(adv) {return(ngrams$syllables[adv == as.character(ngrams$ngram)][1])})
df$height_in_list = 9 - df$ranking
df$adverb_list = rep("A", nrow(df))
df$adverb_list[df$adverb %in% list_b] = "B"
df$adverb_list[df$adverb %in% list_c] = "C"
df$adverb_list[df$adverb %in% list_d] = "D"

## add predictor for surprisal that is the residual of surprisal after being predicted by syllables
m.resid = lm(surprisal~syllables,data=df)
df$resid_surprisal = resid(m.resid)

aggdf = ddply(df, .(adverb, adjective), function(subd) {
  resampled = boot(subd, function(orig, indices) {
    return( mean(orig[indices,]$height_in_list) )
  }, 100)$t
  newd = data.frame(
    adverb = subd$adverb[[1]],
    adjective = subd$adjective[[1]],
    surprisal = subd$surprisal[[1]],
    resid_surprisal = subd$resid_surprisal[[1]],
    syllables = subd$syllables[[1]],
    adverb_list = subd$adverb_list[[1]],
    height_in_list = mean(subd$height_in_list),
    height_in_list_high = quantile(resampled, 0.025),
    height_in_list_low = quantile(resampled, 0.975)
  )
  return(newd)
})
p = ggplot(data=aggdf, aes(x=surprisal, y=height_in_list, colour=as.numeric(syllables))) +
  geom_smooth(method="lm", lwd=0, colour="gray", alpha=1/10) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=height_in_list_low, ymax=height_in_list_high, x=surprisal), width=0.3) +
  theme_bw(14) +
  facet_grid(. ~ adjective, scale="free") +
  xlab("surprisal") +
  ylab("height in list") +
  scale_colour_continuous(name="syllables") +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  ggtitle("Experiment 2") +
  #   scale_colour_brewer(type="div", palette=7) +
  #   scale_fill_brewer(type="div", palette=7) +
  theme(panel.grid=element_blank())
print(p)
ggsave("output/Experiment2/scatter.pdf", width=10, height=3)
p = ggplot(data=aggdf, aes(x=surprisal, y=height_in_list, colour=as.numeric(syllables))) +
  geom_smooth(method="lm", lwd=0, colour="gray", alpha=1/10) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=height_in_list_low, ymax=height_in_list_high, x=surprisal), width=0.3) +
  theme_bw(14) +
  facet_grid(adverb_list ~ adjective, scale="free") +
  xlab("surprisal") +
  ylab("height in list") +
  scale_colour_continuous(name="syllables") +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  ggtitle("Experiment 2") +
#   scale_colour_brewer(type="div", palette=7) +
#   scale_fill_brewer(type="div", palette=7) +
  theme(panel.grid=element_blank())
print(p)
ggsave("output/Experiment2/full_scatter.pdf", width=10, height=10)

response_order = ddply(aggdf, .(adverb), summarise, height_in_list = mean(height_in_list))
aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$height_in_list)])

ordering = ggplot(aggdf, aes(y=adverb, x=height_in_list, colour=adverb)) +
  geom_point(size=3) +
  geom_errorbarh(aes(y=adverb, xmin=height_in_list_low, xmax=height_in_list_high)) +
  theme_bw(14) +
  theme(panel.grid=element_blank()) +
  guides(colour=FALSE) +
  facet_grid(.~adjective, scale="free") +
  ggtitle("Experiment 2: intensifiers")
print(ordering)
ggsave("output/Experiment2/ordering.pdf", width=20, height=6)

centered = cbind(df, myCenter(df[,c("surprisal","syllables","resid_surprisal")]))

pdf(file="output/Experiment2/predictors.pdf", width=17, height=17)
pairscor.fnc(centered[,c("surprisal","syllables","csurprisal","csyllables","height_in_list","resid_surprisal","cresid_surprisal")])
dev.off()

library(mlogit)
sink(file="output/Experiment2/model.txt")
df$ch = df$ranking + 1
df$chid = paste(df$adverb_list, df$workerid)
G <- mlogit.data(df, choice = "ch", shape = "long", chid.var="chid",
                 alt.var="adverb", ranked = TRUE)
model = mlogit(ch ~ surprisal + syllables | 0, G)
print(summary(model))
print(summary(mlogit(ch ~ surprisal + syllables + surprisal:adjective + syllables:adjective | 0, G)))
sink(NULL)


# Gbeautiful <- mlogit.data(subset(df, adjective == "beautiful"), choice = "ch", shape = "long", chid.var="chid",
#                  alt.var="adverb", ranked = TRUE)
# Gold <- mlogit.data(subset(df, adjective == "old"), choice = "ch", shape = "long", chid.var="chid",
#                           alt.var="adverb", ranked = TRUE)
# Gtall <- mlogit.data(subset(df, adjective == "tall"), choice = "ch", shape = "long", chid.var="chid",
#                     alt.var="adverb", ranked = TRUE)
# Gexpensive <- mlogit.data(subset(df, adjective == "expensive"), choice = "ch", shape = "long", chid.var="chid",
#                      alt.var="adverb", ranked = TRUE)
# summary(mlogit(ch ~ surprisal + syllables | 0, Gbeautiful))
# summary(mlogit(ch ~ surprisal + syllables | 0, Gold))
# summary(mlogit(ch ~ surprisal + syllables | 0, Gtall))
# summary(mlogit(ch ~ surprisal + syllables | 0, Gexpensive))
params = data.frame(
  type=c("surprisal", "syllables", "surprisal", "syllables", "surprisal", "syllables", "surprisal", "syllables"),
  estimate=c(0.073518, 0.318197, 0.110228, 0.225962, 0.261593, 0.215122, 0.243998, 0.217833),
  error=c(0.029621, 0.086995, 0.031160, 0.080922, 0.034492, 0.083455, 0.035874, 0.081535),
  adjective=c("beautiful", "beautiful", "old", "old", "tall", "tall", "expensive", "expensive")
)
p = ggplot(params, aes(x=adjective, y=estimate, colour=adjective)) +
  facet_wrap(~ type) +
  geom_point() +
  geom_errorbar(aes(x=adjective, ymin=estimate-error, ymax=estimate+error))
print(p)
ggsave("output/Experiment2/params.pdf", width=8.5, height=4)

# res = data.frame(
#   residual = residuals(model),
#   predicted = fitted(model),
#   actual = getME(model, name=c("y")),
#   workerid = getME(model, name=c("flist"))$workerid,
#   object = getME(model, name=c("flist"))$object,
#   surprisal = getME(model, name=c("X"))[,2],
#   syllables = getME(model, name=c("X"))[,3],
#   intensifier = factor(
#     sapply(getME(model, name=c("X"))[,2], function(surp) {
#       return(as.character(ngrams$ngram)[ ngrams$surprisal == surp][[1]])
#     }),
#     levels = as.character(response_order$adverb)[order(response_order$response)]
#   )
# )
# res_scatter_object = ggplot(res, aes(x=object, y=residual, colour=object), alpha=1/10) +
#   geom_point() +
#   ggtitle("residuals: object")
# print(res_scatter_object)
# ggsave("output/Experiment1/residuals-object.pdf", width=8.5, height=4)
# res_scatter_worker = ggplot(res, aes(x=workerid, y=residual, colour=workerid), alpha=1/10) +
#   geom_point() +
#   ggtitle("residuals: worker")
# print(res_scatter_worker)
# ggsave("output/Experiment1/residuals-worker.pdf", width=8.5, height=4)
# res_scatter_intensifier = ggplot(res, aes(x=intensifier, y=residual, colour=intensifier), alpha=1/10) +
#   geom_point() +
#   ggtitle("residuals: intensifier") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# print(res_scatter_intensifier)
# ggsave("output/Experiment1/residuals-intensifier.pdf", width=16, height=10)
# res_scatter_surprisal = ggplot(res, aes(x=surprisal, y=residual, colour=surprisal), alpha=1/10) +
#   geom_point() +
#   ggtitle("residuals: surprisal")
# print(res_scatter_surprisal)
# ggsave("output/Experiment1/residuals-surprisal.pdf", width=8.5, height=4)
# res_scatter_syllables = ggplot(res, aes(x=syllables, y=residual, colour=syllables), alpha=1/10) +
#   geom_point() +
#   ggtitle("residuals: syllables")
# print(res_scatter_syllables)
# ggsave("output/Experiment1/residuals-syllables.pdf", width=8.5, height=4)
# 
# write.csv(res, "output/Experiment1/residuals.csv", row.names=F)