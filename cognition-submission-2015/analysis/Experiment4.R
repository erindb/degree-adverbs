# load dependencies
library(ggplot2)
library(boot)
library(plyr)
library(ordinal)
library(languageR)
library(mlogit)

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
d = read.csv("../data/Experiment4.csv")
total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))

total_workers = length(unique(d$workerid))
uncooperative = length(unique(subset(d, asses=="No")$workerid))
d = subset(d, asses == "Yes" | asses == "Confused" | is.na(asses))
good_workers = length(unique(d$workerid))
print(total_workers)
print(uncooperative)
print(good_workers)

replication = function(df) {

  df = df[!(df$adverb %in% c("bugornly expensive", "tupabugornly expensive",
                           "ratumly expensive", "gaburatumly expensive",
                           "lopusly expensive", "fepolopusly expensive",
                           "bugornly tall", "tupabugornly tall",
                           "ratumly tall", "gaburatumly tall",
                           "lopusly tall", "fepolopusly tall")),
         c("workerid", "ranking", "adverb", "adjective", "asses")]
  df = ddply(df, .(workerid), transform, ranking = rank(ranking))
  
  df = df[,c("workerid", "adverb", "ranking", "adjective")]
  df$adverb = as.character(df$adverb)
  df$adjective = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][2])})
  df$adverb = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][1])})
  df$surprisal = sapply(df$adverb, function(adv) {return(ngrams$surprisal[adv == as.character(ngrams$ngram)][1])})
  df$syllables = sapply(df$adverb, function(adv) {return(ngrams$syllables[adv == as.character(ngrams$ngram)][1])})
  df$height_in_list = 9 - df$ranking
  
  # ## add predictor for surprisal that is the residual of surprisal after being predicted by syllables
  # m.resid = lm(surprisal~syllables,data=df)
  # df$resid_surprisal = resid(m.resid)
  
  aggdf = ddply(df, .(adverb, adjective), function(subd) {
    resampled = boot(subd, function(orig, indices) {
      return( mean(orig[indices,]$height_in_list) )
    }, 100)$t
    newd = data.frame(
      adverb = subd$adverb[[1]],
      adjective = subd$adjective[[1]],
      surprisal = subd$surprisal[[1]],
  #     resid_surprisal = subd$resid_surprisal[[1]],
      syllables = subd$syllables[[1]],
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
    ggtitle("Experiment 4") +
    #   scale_colour_brewer(type="div", palette=7) +
    #   scale_fill_brewer(type="div", palette=7) +
    theme(panel.grid=element_blank())
  print(p)
  ggsave("output/Experiment4/scatter.pdf", width=8.5, height=4)
  
  pdf(file="output/Experiment4/predictors.pdf", width=17, height=17)
  pairscor.fnc(df[,c("surprisal","syllables","height_in_list")])
  dev.off()
  
  logit_df = df[,c("workerid", "adverb", "ranking", "surprisal", "syllables", "adjective")]
  logit_df$ranking = as.integer(logit_df$ranking)
  logit_df$adverb = as.factor(logit_df$adverb)
  logit_df$workerid = as.factor(logit_df$workerid)
  G <- mlogit.data(logit_df, choice = "ranking", shape = "long", chid.var="workerid",
                   alt.var="adverb", ranked = TRUE)
  sink(file="output/Experiment4/model.txt")
  model = mlogit(ranking ~ surprisal + syllables | 0, G)
  print(summary(model))
  print(summary(mlogit(ranking ~ surprisal + syllables + surprisal:adjective + syllables:adjective | 0, G)))
  sink(NULL)
}

novel = function(df) {
  df = df[(df$adverb %in% c("bugornly expensive", "tupabugornly expensive",
                            "ratumly expensive", "gaburatumly expensive",
                            "lopusly expensive", "fepolopusly expensive",
                            "bugornly tall", "tupabugornly tall",
                            "ratumly tall", "gaburatumly tall",
                            "lopusly tall", "fepolopusly tall")),
          c("workerid", "adverb", "ranking", "adjective")]
  
  adverb_length = c(
    "bugornly"= "short",
    "tupabugornly"= "long",
    "ratumly"= "short",
    "lopusly"= "short",
    "gaburatumly"= "long",
    "fepolopusly"="long")
  adverb_root = c(
    "bugornly"= "bugorn",
    "tupabugornly"= "bugorn",
    "ratumly"= "ratum",
    "lopusly"= "lopus",
    "gaburatumly"= "ratum",
    "fepolopusly"="lopus")
  
  df$adverb = as.character(df$adverb)
  df$adverb = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][1])})
  # df$syllables = sapply(df$adverb, function(adv) {return(ngrams$syllables[adv == as.character(ngrams$ngram)][1])})
  df$height_in_list = 9 - df$ranking
  
  df$length = sapply(as.character(df$adverb), function(adv) {return(adverb_length[adv])})
  df$root = sapply(as.character(df$adverb), function(adv) {return(adverb_root[adv])})
  
  aggdf = ddply(df, .(adverb), function(subd) {
    resampled = boot(subd, function(orig, indices) {
      return( c(
        mean(orig[indices,]$height_in_list),
        mean(orig[indices,]$ranking) )
      )
    }, 100)$t
    newd = data.frame(
      adverb = subd$adverb[[1]],
  #     adjective = subd$adjective[[1]],
      #     resid_surprisal = subd$resid_surprisal[[1]],
  #     syllables = subd$syllables[[1]],
      height_in_list = mean(subd$height_in_list),
      height_in_list_high = quantile(resampled[,1], 0.025),
      height_in_list_low = quantile(resampled[,1], 0.975),
      ranking = mean(subd$ranking),
      ranking_high = quantile(resampled[,2], 0.025),
      ranking_low = quantile(resampled[,2], 0.975)
    )
    return(newd)
  })
  ggplot(aggdf, aes(x=adverb, y=height_in_list, colour=adverb)) +
    geom_point(size=3) +
    geom_errorbar(aes(x=adverb, ymin=height_in_list_low, ymax=height_in_list_high))
  
  sink(file="output/Experiment4/novel-model.txt")
  df$ranking = ordered(df$ranking)
  df$height_in_list = ordered(df$height_in_list)
  model = clm(height_in_list ~ length, data=df)
  model_with_root = clm(height_in_list ~ length + root, data=df)
  print(summary(model))
  print(summary(model_with_root))
  sink(NULL)
}

replication(d)
novel(d)

df = d

df$adverb = as.character(df$adverb)
df$adjective = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][2])})
df$adverb = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][1])})

aggdf = ddply(df, .(adverb, adjective), function(subd) {
  resampled = boot(subd, function(orig, indices) {
    return( mean(orig[indices,]$height_in_list) )
  }, 100)$t
  newd = data.frame(
    adverb = subd$adverb[[1]],
    adjective = subd$adjective[[1]],
    height_in_list = mean(subd$height_in_list),
    height_in_list_high = quantile(resampled, 0.025),
    height_in_list_low = quantile(resampled, 0.975)
  )
  return(newd)
})

response_order = ddply(aggdf, .(adverb), summarise, height_in_list = mean(height_in_list))
aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$height_in_list)])
ordering = ggplot(aggdf, aes(y=adverb, x=height_in_list, colour=adverb)) +
  geom_point(size=3) +
  geom_errorbarh(aes(y=adverb, xmin=height_in_list_low, xmax=height_in_list_high)) +
  theme_bw(14) +
  theme(panel.grid=element_blank()) +
  guides(colour=FALSE) +
  facet_grid(.~adjective, scale="free") +
  ggtitle("Experiment 4: intensifiers")
print(ordering)
ggsave("output/Experiment4/ordering.pdf", width=20, height=6)
