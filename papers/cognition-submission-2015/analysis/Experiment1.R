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
d = read.csv("../data/Experiment1.csv")
total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))

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
  ggsave("output/Experiment1/buyer-gender-hist.pdf", width=8.5, height=6)
  
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
  ggsave("output/Experiment1/buyer-gender-object-hist.pdf", width=8.5, height=3)
}

experiment1 = function(df) {
  total_workers = length(unique(df$workerid))
  non_native = length(unique(subset(df, language == "French")$workerid))
  uncooperative = length(unique(subset(df, asses=="No")$workerid))
  df = subset(df, language != "French" &  asses != "No")
  good_workers = length(unique(df$workerid))
  print(total_workers)
  print(non_native)
  print(uncooperative)
  print(good_workers)
  df = df[,c("workerid", "adverb", "gender", "object", "response")]
  df = ddply(df, .(workerid, adverb, object), transform,
             surprisal = ngrams$surprisal[as.character(ngrams$ngram) == adverb],
             syllables = ngrams$syllables[as.character(ngrams$ngram) == adverb],
             logprice = log(response))
  
  ## add predictor for surprisal that is the residual of surprisal after being predicted by syllables
  m.resid = lm(surprisal~syllables,data=df)
  df$resid_surprisal = resid(m.resid)
  
  aggdf = ddply(df, .(adverb, object), function(subd) {
    
    bootObj = boot(subd, function(orig, indices) {
      return( c(
        mean(orig[indices,]$response),
        mean(orig[indices,]$logprice)
      ) )
    }, 100)
    
    newd = data.frame(
      adverb = subd$adverb[[1]],
      object = subd$object[[1]],
      surprisal = subd$surprisal[[1]],
      resid_surprisal = subd$resid_surprisal[[1]],
      syllables = subd$syllables[[1]],
      response = mean(subd$response),
      logprice = mean(subd$logprice),
      response_high = boot.ci(bootObj, index=1, type="perc")$percent[4],
      response_low = boot.ci(bootObj, index=1, type="perc")$percent[5],
      logprice_high = boot.ci(bootObj, index=2, type="perc")$percent[4],
      logprice_low = boot.ci(bootObj, index=2, type="perc")$percent[5]
    )
    return(newd)
  })
  p = ggplot(data=aggdf, aes(x=surprisal, y=logprice, colour=as.numeric(syllables))) +
    geom_smooth(method="lm", lwd=0, colour="gray", alpha=1/10) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=logprice_low, ymax=logprice_high, x=surprisal), width=0.3) +
    theme_bw(14) +
    facet_wrap(~ object, scale="free") +
    xlab("surprisal") +
    ylab("log(price)") +
    scale_colour_continuous(name="syllables") +
    scale_x_continuous(breaks=c(10, 14, 18)) +
    ggtitle("Experiment 1") +
    theme(panel.grid=element_blank())
  print(p)
  ggsave("output/Experiment1/scatter.pdf", width=10, height=3)
  
  response_order = ddply(aggdf, .(adverb), summarise, response = mean(response))
  aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$response)])
  
  ordering = ggplot(aggdf, aes(y=adverb, x=response, colour=adverb)) +
    geom_point(size=3) +
    geom_errorbarh(aes(y=adverb, xmin=response_low, xmax=response_high)) +
    theme_bw(14) +
    theme(panel.grid=element_blank()) +
    guides(colour=FALSE) +
    facet_wrap(~object, scale="free") +
    ggtitle("Experiment 1: intensifiers")
  print(ordering)
  ggsave("output/Experiment1/ordering.pdf", width=20, height=6)
  
  centered = cbind(df, myCenter(df[,c("surprisal","syllables","resid_surprisal")]))
  
  pdf(file="output/Experiment1/predictors.pdf", width=17, height=17)
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
  
  sink(file="output/Experiment1/model.txt")
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
  ggsave("output/Experiment1/residuals-object.pdf", width=8.5, height=4)
  res_scatter_worker = ggplot(res, aes(x=workerid, y=residual, colour=workerid), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: worker")
  print(res_scatter_worker)
  ggsave("output/Experiment1/residuals-worker.pdf", width=8.5, height=4)
  res_scatter_intensifier = ggplot(res, aes(x=intensifier, y=residual, colour=intensifier), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: intensifier") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(res_scatter_intensifier)
  ggsave("output/Experiment1/residuals-intensifier.pdf", width=16, height=10)
  res_scatter_surprisal = ggplot(res, aes(x=surprisal, y=residual, colour=surprisal), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: surprisal")
  print(res_scatter_surprisal)
  ggsave("output/Experiment1/residuals-surprisal.pdf", width=8.5, height=4)
  res_scatter_syllables = ggplot(res, aes(x=syllables, y=residual, colour=syllables), alpha=1/10) +
    geom_point() +
    ggtitle("residuals: syllables")
  print(res_scatter_syllables)
  ggsave("output/Experiment1/residuals-syllables.pdf", width=8.5, height=4)
  
  write.csv(res, "output/Experiment1/residuals.csv", row.names=F)
}

### questions:
###   * do syllable-length * surprisal have an effect on estimated prices?
         experiment1(d)
###      - does where we get surprisal or how we measure length affect the results?
###         ~ (unigrams|bigrams) X (books|web) X (nchar|nsyll)
###      - what about phonological complexity?
###      - what do the residuals look like?
###   * does gender of buyer have an effect on estimated prices?
###       A: the buyer gender probably doesn't have an effect on estimated prices.
#         check_buyer_gender_effects(d)