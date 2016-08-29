# load dependencies
library(ggplot2)
library(boot)
library(plyr)
library(lme4)
library(lmerTest)
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
d = read.csv("../data/Experiment3.csv")
total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))

total_workers = length(unique(d$workerid))
non_native = length(unique(subset(d, language == "English, Polish")$workerid))
uncooperative = length(unique(subset(d, asses=="No")$workerid))
d = subset(d, language != "English, Polish" &  asses != "No")
good_workers = length(unique(d$workerid))
print(total_workers)
print(non_native)
print(uncooperative)
print(good_workers)

check_buyer_gender_effects = function(df) {
  df$binwidth = 1000
  df$object_binwidth = sapply(df$object, function(object) {
    if (object == "watch") {
      return(2000)
    } else if (object == "laptop") {
      return(1000)
    } else if (object == "coffee maker") {
      return(500)
    }
  })
  df = ddply(df, .(object, response), transform,
             bin=floor(response/binwidth),
             object_bin=floor(response/object_binwidth))
  totals = c(table(df$gender))
  histd = ddply(df, .(bin), function(subd) {
    actual = c(table(subd$gender)) / totals
    counts = boot(subd, function(orig, indices) {
      resampled = orig[indices,]
      return( c(table(resampled$gender)) / totals )
    }, 100)$t
    get_ci = function(percentile) {
      return(function(index) {
        return(quantile(counts[,index], probs=percentile))
      })
    }
    newd = data.frame(
      buyer_gender = names(actual),
      count = actual,
      count_low = sapply(1:2, get_ci(0.025)),
      count_high = sapply(1:2, get_ci(0.975))
    )
    return(newd)
  })
  by_gender_histogram = ggplot(histd, aes(x=bin, y=count, fill=buyer_gender, colour=buyer_gender)) +
    geom_bar(stat="identity", position="dodge", width=0.9, alpha=1/2) +
    geom_errorbar(aes(x=bin, ymin=count_low, ymax=count_high),
                  width=0.5, position=position_dodge(0.9), width=0.1) +
    xlab("thousands of dollars") +
    ylab("percent responses in this range") +
    ggtitle("buyer gender effects on overall responses")
  print(by_gender_histogram)
  ggsave("output/Experiment3/buyer-gender-hist.pdf", width=8.5, height=6)
  
  object_histd = ddply(df, .(object_bin, object), function(subd) {
    actual = c(table(subd$gender)) / totals
    counts = boot(subd, function(orig, indices) {
      resampled = orig[indices,]
      return( c(table(resampled$gender)) / totals )
    }, 100)$t
    get_ci = function(percentile) {
      return(function(index) {
        return(quantile(counts[,index], probs=percentile))
      })
    }
    newd = data.frame(
      buyer_gender = names(actual),
      count = actual,
      count_low = sapply(1:2, get_ci(0.025)),
      count_high = sapply(1:2, get_ci(0.975))
    )
    return(newd)
  })
  
  by_object_and_gender_histogram = ggplot(object_histd, aes(x=object_bin, y=count, fill=buyer_gender, colour=buyer_gender)) +
    geom_bar(stat="identity", position="dodge", width=0.9, alpha=1/2) +
    geom_errorbar(aes(x=object_bin, ymin=count_low, ymax=count_high),
                  width=0.5, position=position_dodge(0.9), width=0.1) +
    facet_wrap(~ object, scale="free") +
    xlab("thousands of dollars") +
    ylab("percent responses in this range") +
    ggtitle("buyer gender effects on per-object responses")
  print(by_object_and_gender_histogram)
  ggsave("output/Experiment3/buyer-gender-object-hist.pdf", width=8.5, height=3)
}

experiment3_replication = function(df) {
  df = df[!(as.character(df$adverb) %in%
              c("lopusly", "ratumly", "gaburatumly", "fepolopusly", "tupabugornly", "bugornly")),
          c("workerid", "adverb", "gender", "object", "response")]
  df$adverb = as.character(df$adverb)
  ngrams$ngram = as.character(ngrams$ngram)
  df$syllables = sapply(df$adverb,function(adverb) { return(ngrams$syllables[ngrams$ngram == adverb]) })
  df$logprice = log(as.numeric(as.character(df$response)))
  df$surprisal = sapply(df$adverb,function(adverb) { return(ngrams$surprisal[ngrams$ngram == adverb]) })
  
  ## add predictor for surprisal that is the residual of surprisal after being predicted by syllables
  m.resid = lm(surprisal~syllables,data=df)
  df$resid_surprisal = resid(m.resid)
  
  aggdf = ddply(df, .(adverb, object), function(subd) {
    resampled = boot(subd, function(orig, indices) {
      return( c(
        mean(orig[indices,]$response),
        mean(orig[indices,]$logprice)
        ) )
      }, 100)$t
    newd = data.frame(
      adverb = subd$adverb[[1]],
      object = subd$object[[1]],
      surprisal = subd$surprisal[[1]],
      syllables = subd$syllables[[1]],
      resid_surprisal = subd$resid_surprisal[[1]],
      response = mean(subd$response),
      logprice = mean(subd$logprice),
      response_high = quantile(resampled[,1], 0.025),
      response_low = quantile(resampled[,1], 0.975),
      logprice_high = quantile(resampled[,2], 0.025),
      logprice_low = quantile(resampled[,2], 0.975)
    )
    return(newd)
  })
  p = ggplot(data=aggdf, aes(x=surprisal, y=logprice, colour=syllables)) +
    geom_smooth(method="lm", lwd=0, colour="gray", alpha=1/10) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=logprice_low, ymax=logprice_high, x=surprisal), width=0.3) +
    theme_bw(14) +
    facet_wrap(~ object, scale="free") +
    xlab("surprisal") +
    ylab("log(price)") +
    ggtitle("Experiment 3") +
    theme(panel.grid=element_blank())
  print(p)
  ggsave("output/Experiment3/scatter.pdf", width=8.5, height=3)
  
  response_order = ddply(aggdf, .(adverb), summarise, response = mean(response))
  aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$response)])  
    
  ordering = ggplot(aggdf, aes(y=adverb, x=response, colour=adverb)) +
    geom_point(size=3) +
    geom_errorbarh(aes(y=adverb, xmin=response_low, xmax=response_high)) +
    theme_bw(14) +
    theme(panel.grid=element_blank()) +
    guides(colour=FALSE) +
    facet_wrap(~object, scale="free") +
    ggtitle("Experiment #: intensifiers")
  print(ordering)
  ggsave("output/Experiment3/ordering.pdf", width=20, height=6)
  
  centered = cbind(df, myCenter(df[,c("surprisal","syllables","resid_surprisal")]))
  
  pdf(file="output/Experiment3/predictors.pdf", width=17, height=17)
  pairscor.fnc(centered[,c("surprisal","syllables","csurprisal","csyllables","logprice","resid_surprisal","cresid_surprisal")])
  dev.off()
  
  model = lmer(logprice ~ surprisal + syllables +
                 (1 + surprisal + syllables | workerid) +
                 (1 + surprisal + syllables | object), data=df)
  model_resid = lmer(logprice ~ resid_surprisal + syllables +
                       (1 + resid_surprisal + syllables | workerid) +
                       (1 + resid_surprisal + syllables | object), data=df)
  model_with_interaction = lmer(logprice ~ surprisal * syllables +
                                  (1 + surprisal + syllables | workerid) +
                                  (1 + surprisal + syllables | object), data=df)
  model_resid_with_interaction = lmer(logprice ~ resid_surprisal * syllables +
                                        (1 + resid_surprisal + syllables | workerid) +
                                        (1 + resid_surprisal + syllables | object), data=df)
  model_with_interaction_centering = lmer(logprice ~ csurprisal * csyllables +
                                            (1 + csurprisal + csyllables | workerid) +
                                            (1 + csurprisal + csyllables | object), data=centered)
  model_resid_with_interaction_centering = lmer(logprice ~ cresid_surprisal * csyllables +
                                                  (1 + cresid_surprisal + csyllables | workerid) +
                                                  (1 + cresid_surprisal + csyllables | object), data=centered)
  
  sink(file="output/Experiment3/model.txt")
  print("model")
  print(summary(model))
  print("model_resid")
  print(summary(model_resid))
  print("model_with_interaction")
  print(summary(model_with_interaction))
  print("model_resid_with_interaction")
  print(summary(model_resid_with_interaction))
  print("model_with_interaction_centering")
  print(summary(model_with_interaction_centering))
  print("model_resid_with_interaction_centering")
  print(summary(model_resid_with_interaction_centering))
  sink(NULL) 
  
  res = data.frame(
    residual = residuals(model),
    predicted = fitted(model),
    actual = getME(model, name=c("y")),
    workerid = getME(model, name=c("flist"))$workerid,
    object = getME(model, name=c("flist"))$object,
    surprisal = getME(model, name=c("X"))[,2],
    syllables = getME(model, name=c("X"))[,3],
    intensifier = factor(
      sapply(getME(model, name=c("X"))[,2], function(surp) {
        return(as.character(ngrams$ngram)[ ngrams$surprisal == surp][[1]])
      }),
      levels = as.character(response_order$adverb)[order(response_order$response)]
    )
  )
  res_scatter_object = ggplot(res, aes(x=object, y=residual, colour=object), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: object")
  print(res_scatter_object)
  ggsave("output/Experiment3/residuals-object.pdf", width=8.5, height=4)
  res_scatter_worker = ggplot(res, aes(x=workerid, y=residual, colour=workerid), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: worker")
  print(res_scatter_worker)
  ggsave("output/Experiment3/residuals-worker.pdf", width=8.5, height=4)
  res_scatter_intensifier = ggplot(res, aes(x=intensifier, y=residual, colour=intensifier), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: intensifier") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(res_scatter_intensifier)
  ggsave("output/Experiment3/residuals-intensifier.pdf", width=16, height=10)
  res_scatter_surprisal = ggplot(res, aes(x=surprisal, y=residual, colour=surprisal), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: surprisal")
  print(res_scatter_surprisal)
  ggsave("output/Experiment3/residuals-surprisal.pdf", width=8.5, height=4)
  res_scatter_syllables = ggplot(res, aes(x=syllables, y=residual, colour=syllables), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: syllables")
  print(res_scatter_syllables)
  ggsave("output/Experiment3/residuals-syllables.pdf", width=8.5, height=4)
  
  write.csv(res, "output/Experiment3/residuals.csv", row.names=F)
}

experiment3_novel = function(df) {
  adverb_length = c(
    "bugornly"= "short",
    "tupabugornly"= "long",
    "ratumly"= "short",
    "lopusly"= "short",
    "gaburatumly"= "long",
    "fepolopusly"="long")
  
  df = df[(as.character(df$adverb) %in%
             c("lopusly", "ratumly", "gaburatumly", "fepolopusly", "tupabugornly", "bugornly")),
          c("workerid", "adverb", "object", "response")]
  df$logprice = log(df$response)
  df$length = sapply(as.character(df$adverb), function(adv) {return(adverb_length[adv])})
  aggdf = ddply(df, .(adverb, object), function(subd) {
    adverb = as.character(subd$adverb)[[1]]
    resampled = boot(subd, function(orig, indices) {
      return( c(
        mean(orig[indices,]$response),
        mean(orig[indices,]$logprice)
      ) )
    }, 100)$t
    newd = data.frame(
      adverb = subd$adverb[[1]],
      length = adverb_length[adverb],
      object = subd$object[[1]],
      response = mean(subd$response),
      logprice = mean(subd$logprice),
      response_low = quantile(resampled[,1], 0.025),
      response_high = quantile(resampled[,1], 0.975),
      logprice_low = quantile(resampled[,2], 0.025),
      logprice_high = quantile(resampled[,2], 0.975)
    )
    return(newd)
  })
  
  aggggdf = ddply(df, .(length, object), function(subd) {
    adverb = as.character(subd$adverb)[[1]]
    resampled = boot(subd, function(orig, indices) {
      return( c(
        mean(orig[indices,]$response),
        mean(orig[indices,]$logprice)
      ) )
    }, 100)$t
    newd = data.frame(
      length = subd$length[[1]],
      object = subd$object[[1]],
      response = mean(subd$response),
      logprice = mean(subd$logprice),
      response_low = quantile(resampled[,1], 0.025),
      response_high = quantile(resampled[,1], 0.975),
      logprice_low = quantile(resampled[,2], 0.025),
      logprice_high = quantile(resampled[,2], 0.975)
    )
    return(newd)
  })
  
  p = ggplot(aggggdf, aes(x=length, y=logprice, fill=length)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(x=length, ymin=logprice_low, ymax=logprice_high), width=0.1) +
    theme_bw(14) +
    theme(panel.grid=element_blank()) +
    guides(colour=FALSE) +
    facet_wrap(~object, scale="free") +
    xlab("length of nonce word") +
    ylab("log of estimated price") +
    scale_fill_brewer(type="qual", palette=6) +
    ggtitle("Experiment 3")
  print(p)
  ggsave("output/Experiment3/novel.pdf", width=8.5, height=3)
    
  
  response_order = ddply(aggdf, .(adverb), summarise, response = mean(response))
  aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$response)])
  
  ordering = ggplot(aggdf, aes(y=adverb, x=response, colour=adverb)) +
    geom_point(size=3) +
    geom_errorbarh(aes(y=adverb, xmin=response_low, xmax=response_high)) +
    theme_bw(14) +
    theme(panel.grid=element_blank()) +
    guides(colour=FALSE) +
    facet_wrap(~object, scale="free") +
    ggtitle("Experiment 3: intensifiers")
  print(ordering)
  ggsave("output/Experiment3/ordering.pdf", width=20, height=6)
  
  novel = subset(df, adverb %in% c("lopusly", "ratumly", "gaburatumly", "fepolopusly", "tupabugornly", "bugornly"))
  novel$root = "lopus"
  novel$length = "short"
  novel$root[novel$adverb %in% c("ratumly", "gaburatumly")] = "ratum"
  novel$root[novel$adverb %in% c("bugornly", "tupabugornly")] = "bugorn"
  novel$length[novel$adverb %in% c("gaburatumly", "tupabugornly", "fepolopusly")] = "long"
  
  novel_model = lmer(logprice ~ length + (1 | root) + (1 + length | object), data=novel)
  novel_model_all_fixed = lmer(logprice ~ length + root + (1 + length + root | object), data=novel)
  sink(file="output/Experiment3/novel_model.txt")
  print(summary(novel_model))
  print(summary(novel_model_all_fixed))
  print(anova(novel_model_all_fixed))
  sink(NULL)
}

### questions:
###   * do syllable-length * surprisal have an effect on estimated prices?
#          experiment3_replication(d)
###      - does where we get surprisal or how we measure length affect the results?
###         ~ (unigrams|bigrams) X (books|web) X (nchar|nsyll)
###      - what about phonological complexity?
###      - what do the residuals look like?
###   * does gender of buyer have an effect on estimated prices?
###       A: the buyer gender probably doesn't have an effect on estimated prices.
#         check_buyer_gender_effects(d)
###   * does this effect extend to novel adverbs?
          experiment3_novel(d)