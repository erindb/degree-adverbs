# ---
# title: "Power Analysis"
# author: "Erin Bennett"
# output: 
#   html_document:
#       toc: false
# ---
# 
# ```{r global_options, include=FALSE}
# rm(list=ls())
# knitr::opts_chunk$set(echo=F, warning=F, cache=F, message=F, sanitiz =T, fig.width = 5, fig.height = 3)
# ```
# 
# ```{r load_settings}
# source("~/Settings/mini-startup.R")
source("../startup.R")
# ```

# ## Experiment
# 
# Rating experiment.
# 
# ## Planned analysis
# 
# ### 1. Reformat data
# 
# We first excluded any participants who are not native speakers of English or who stated that they did not think they followed instructions.
# 
# People represent prices logarithmically, so we log-transform all of the prices that participants entered.
# 
# We don't care about modeling the prices of the individual objects. We think that this factor might interact with participants (different participants have different beliefs about price distributions for different objects), but we do not have enough data to fit an accurate mixed model with interacting random effects. So we z-score within each participant's ratings for each object (Thanks, Reviewer, for the suggestion!).
# 
# ```{r}
## load intensifier data
unigrams = read.csv("../data/web_1grams.csv")
total_ngrams = 1024908267229
freq = unigrams$frequency
syll = unigrams$syllables
names(freq) = names(syll) = unigrams$ngram

## load experiment data
df = read.csv("../data/study1a_data.csv") %>%
  rename(price=response, intensifier = adverb) %>%
  filter(language %in% c("English", "ENGLISH", "ENG", "eng", "english") &
           assess == "Yes") %>%
  select(workerid, price, intensifier, object) %>%
  mutate(workerid = factor(workerid),
         logprice = as.numeric(log(price))) %>%
  group_by(object) %>%
  mutate(logprice.scaled.by.object = as.numeric(scale(logprice)),
         logprice.scaled.by.object = ifelse(
           is.nan(logprice.scaled.by.object), 0,
           logprice.scaled.by.object)) %>%
  ungroup %>%
  group_by(object, workerid) %>%
  mutate(logprice = as.numeric(log(price)),
         logprice.centered = logprice - mean(logprice),
         z = as.numeric(scale(log(price))),
         z = ifelse(is.nan(z), 0, z),
         logprice.scaled = z) %>%
  ungroup %>%
  mutate(freq = freq[as.character(intensifier)],
         syll = syll[as.character(intensifier)],
         syll.centered = syll - mean(syll),
         surprisal = -log(freq),
         surprisal.centered = surprisal - mean(surprisal),
         syll.scaled = scale(syll),
         surprisal.scaled = scale(surprisal),
         word.cost = 0.18124*syll.centered + 0.10704*surprisal.centered)

# ```
# 
# Here's the original data. The slopes and intercepts vary by both participants and objects. In particular, watches seem to have larger slopes than laptops.
# 
# ```{r}
df %>% ggplot(., aes(x=word.cost,
                     y=logprice,
                     group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "lm", alpha = 0.1) +
  facet_wrap(~object) +
  geom_point(alpha=0.1)
# ```
# 
# And here's what the data look like when we z-score within each participant's responses for each object. Now each participant has an intercept of zero (kind of obviously...), but the slopes vary across participants.
# 
# ```{r}
df %>% ggplot(., aes(x=word.cost,
                     y=logprice.scaled,
                     group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "lm", alpha = 0.1) +
  facet_wrap(~object) +
  geom_point(alpha=0.1)
# ```
# 
# #### Preprocessing for replication design
# 
# We will have a larger set of intensifiers in the replication experiment, so ideally we would like to run a "replication design" where we run a certain number of participants on each of 3 or so disjoint sets of intensifiers.
# 
# How would this interact with the zscoring step in preprocessing?
# 
# ```{r}
create_data_subset = function(data, intensifiers, workers) {
  indices = c(apply(
    expand.grid(intensifiers, workers),
    1,
    function(lst) {
      indices = which(df$intensifier==lst[1] &
                        df$workerid==lst[2])
      return(indices)
    }))
  x = data[indices,]
  return(x)
}

create_replication_design_data = function(data, intensifiersList, workeridsList) {
  shuffled_intensifiers = sample(intensifiersList, replace=F)
  shuffled_workers = sample(workeridsList, replace=F)
  create_replication_set = function(set_index) {
    ## split full intensifiers list into 3 disjoint sets
    ## split workers into corresponding 3 disjoint sets
    n_intensifiers_set = length(shuffled_intensifiers)/3
    n_workers_set = length(shuffled_workers)/3
    w.ind = c(0,0)
    i.ind = c(0,0)
    w.ind[1] = ((set_index-1)*n_workers_set)+1
    w.ind[2] = ((set_index)*n_workers_set)
    i.ind[1] = ((set_index-1)*n_intensifiers_set)+1
    i.ind[2] = ((set_index)*n_intensifiers_set)
    if (set_index==3) {
      w.ind[2] = length(shuffled_workers)
      i.ind[2] = length(shuffled_intensifiers)
    }
    intensifiers_subset = shuffled_intensifiers[i.ind[1]:i.ind[2]]
    workers_subset = shuffled_workers[w.ind[1]:w.ind[2]]
    ## sample the rows of the data frame for each set
    data_subset = create_data_subset(data,
                                     intensifiers_subset,
                                     workers_subset) %>%
      mutate(set = set_index)
    return(data_subset)
  }
  return(do.call(
    rbind,
    lapply(1:3, create_replication_set)))
}

replication_design_data = create_replication_design_data(df,
                                                         unique(df$intensifier),
                                                         unique(df$workerid))
# ```
# 
# Here is a subset of the data where each participant sees items from only one of three disjoint sets of intensifiers.
# 
# ```{r}
replication_design_data %>%
  ggplot(., aes(x=word.cost,
                y=logprice,
                group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "lm", alpha = 0.1) +
  facet_wrap(~object) +
  geom_point(alpha=0.1)
# ```
# 
# And here is that same data, zscored within participant and object. (We have very little data in each normalized subset in this simulation: we will have much more data in the proposed future replication experiment.)
# 
# ```{r}
replication_design_data %>%
  group_by(object, workerid) %>%
  mutate(
    z = as.numeric(scale(log(price))),
    z = ifelse(is.nan(z), 0, z),
    logprice.scaled = z
  ) %>%
  ungroup %>%
  ggplot(., aes(x=word.cost,
                y=logprice.scaled,
                colour=factor(set),
                group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "lm", alpha = 0.1) +
  facet_wrap(~object) +
  geom_point(alpha=0.1)
# ```
# 
# At a glance, it looks like this might not warrant the intercept-less model we used when z-scoring within participants and objects on the full dataset.
# 
# But the model with random intercepts doesn't actually converge, so maybe it's fine to keep the model the same.
# 
# Now, in the replication design, the slopes are much steeper! Are we increasing the possibility of Type 1 errors?
# 
# ### 2. Model
# 
# Given that preprocessing involves normalizing responses from each participant for each item, we would use the following model, which includes a random slope for participant but no random intercept. We include random intercepts for intensifier (the particular words). We center the continuous predictors by subtracting their means.
# 
# ```{r, echo=T}
m = lmer(logprice.scaled ~ 1 + syll.centered * surprisal.centered +
    (0 + syll.centered + surprisal.centered | workerid) +
    (1 | intensifier), df)
# ```
# 
# ### 3. Power analysis
# 
# #### Bootstrapping
# 
# We could assume that the two effects are there, and resample the original experiment's data to see how often we pick up on the significance of both the predictors.
# 
# ```{r}
workers = unique(df$workerid)
intensifiers = unique(df$intensifier)
run.model = function(data) {
  preprocessed.data = data %>%
    group_by(object, workerid) %>%
    mutate(
      z = as.numeric(scale(log(price))),
      z = ifelse(is.nan(z), 0, z),
      logprice.scaled = z
    ) %>%
    select(logprice.scaled, syll.centered, surprisal.centered, workerid, intensifier, object) %>%
    ungroup
  resampled.model = lmer(
    logprice.scaled ~ 1 + syll.centered * surprisal.centered +
      (0 + syll.centered + surprisal.centered | workerid) +
      (1 | intensifier), preprocessed.data)
  coefficients = summary(resampled.model)$coefficients
  pvalues = c(
    syll=coefficients[["syll.centered", "Pr(>|t|)"]],
    surprisal=coefficients[["surprisal.centered", "Pr(>|t|)"]]
  )
  significance = c(
    syll = pvalues[["syll"]]<0.05,
    surprisal = pvalues[["surprisal"]]<0.05
  )
  significance["both"] = significance[["syll"]] & significance[["surprisal"]]
  return(significance)
}
bootstrap.power = function(reps=1, participantsN=28, intensifiersN=40) {
  results = sapply(1:reps, function(r) {
    resampled.workers = sample(workers, participantsN, replace=T)
    resampled.intensifiers = sample(intensifiers, intensifiersN, replace=T)
    ## for each worker, for each intensifier, what's their data?
    resampled.data = create_data_subset(df, resampled.intensifiers, workers=resampled.workers)
    return(
      tryCatch(
        run.model(resampled.data),
        warning=function(e) {return(c(syllables=NA, surprisal=NA, both=NA))},
        error=function(e) {return(c(syllables=NA, surprisal=NA, both=NA))}
      )
    )
  })
  power = rowMeans(results, na.rm=T)
  return(c(
    syll=power[["syll"]],
    surprisal=power[["surprisal"]],
    both=power[["both"]],
    intensifiersN=intensifiersN,
    participantsN=participantsN,
    reps=reps))
}
bootstrap.power(100, 100, 72)
# power.28.40 = bootstrap.power(100, 28, 40)
# power.28.72 = bootstrap.power(100, 28, 72)
# power.28.100 = bootstrap.power(100, 28, 100)
# power.50.40 = bootstrap.power(100, 50, 40)
# power.50.72 = bootstrap.power(100, 50, 72)
# power.50.100 = bootstrap.power(100, 50, 100)
# ```
# 
# ```{r}
power.sketch.to.plot = data.frame(rbind(
  power.28.40,
  power.28.72,
  power.28.100,
  power.50.40,
  power.50.72,
  power.50.100
)) %>% rename(syll = syll.syll, surprisal = surprisal.surprisal, both = both.both)
power.sketch.to.plot %>%
  gather("coefficient", "power", c(syll, surprisal, both)) %>%
  # filter(participantsN < 1000) %>%
  ggplot(., aes(x=participantsN, y=intensifiersN,
                colour=power, size=power)) +
  geom_point() +
  facet_wrap(~coefficient)
ggsave("power.sketch.png", width=10, height=4)
# ```
# 
# Similarly, we can simulate using a replication design with a preprocessing step where we z-score within objects and participants.
# 
# Such a preprocessing step may or may not be reasonable, but this is the effect it would have on power:
# 
# ```{r}
## for each number of participants,
## for each number of intensifiers,
## divide them all into sets of 3
## and run a replication design with z-scoring

workers = unique(df$workerid)
intensifiers = unique(df$intensifier)
bootstrap.power.rep.design = function(reps=1, participantsN=28, intensifiersN=40) {
  results = sapply(1:reps, function(r) {
    resampled.workers = sample(workers, participantsN, replace=T)
    resampled.intensifiers = sample(intensifiers, intensifiersN, replace=T)
    ## for each worker, for each intensifier, what's their data?
    resampled.data = create_data_subset(df, resampled.intensifiers, workers=resampled.workers)
    replication.design.data = create_replication_design_data(resampled.data, resampled.intensifiers, resampled.workers)
    return(
      tryCatch(
        run.model(replication.design.data),
        warning=function(e) {return(c(syllables=NA, surprisal=NA, both=NA))},
        error=function(e) {return(c(syllables=NA, surprisal=NA, both=NA))}
      )
    )
  })
  power = rowMeans(results, na.rm=T)
  return(c(
    syll=power[["syll"]],
    surprisal=power[["surprisal"]],
    both=power[["both"]],
    intensifiersN=intensifiersN,
    participantsN=participantsN,
    reps=reps))
}
power.rep.20.71 = bootstrap.power.rep.design(100, 20, 71)
power.rep.50.71 = bootstrap.power.rep.design(100, 50, 71)
# power.rep.100.71 = bootstrap.power.rep.design(100, 100, 71)
# ```
# 
# ### 4. Items
# 
# See [intensifiers-list.html](./intensifiers-list.html)
# 
# ### 5. Writeup
# 
# Reviewer 1 helpfully suggested normalizing each participant's price responses for each object. Because we have few objects, we are unable to model the variation due to object, and because we have many intensifiers, fully crossed with objects, normalizing is fairly effective and converting all objects to the same scale. We appreciate this improvement, and have made the corresponding revisions in the analyses for Experiment 1 and Experiment 2.
# 
# Reviewer 1 mentioned two important issues in our design: our choice of intensifiers and the power of our study. Our choice of intensifiers was not fully systematic or replicable, and any one of our experiments may have been underpowered (though we did replicate our findings in several pilot studies with varying sets of intensifiers). We therefore ran a further replication with a more systematic set of intensifiers.
# 
# Our process for collecting intensifiers in this replication was to find 12 grammars of English that mentioned one of the following terms: "intensifiers", "adverbs of degree", or "amplifiers". Most of these grammars mentioned examples of such words, and many contained lists of them. The average length of such a list or collection was LLL. Some "downtoners" were also mixed into some of these lists (e.g. "slightly", "barely"). We excluded obvious downtoners (which were often labeled as downtoners by at least one grammar) and otherwise collected an aggregate list of all words that occurred in an "intensifiers", "adverbs of degre" or "amplifiers" list in at least one grammar. This resulted in a total of 71 unique intensifiers. Of these, only 19 had been in our previous experiments. 21 words that appeared in our previous experiments did not appear in any list, including "insanely", "wildly", "exceptionally", and "frightfully".
# 
# For the replication experiment (Experiment 1b in the revised paper), we wanted enough participants for a power level of 0.8 for the principle effect of surprisal. Power analyses for mixed effects models with continuous predictors are not algebraically straightforward, and so we approximated the number of participants necessary for our desired power by bootstrapping. For each of 100 iterations, we sampled with replacement from our original experiment (Experiment 1a in the revised paper) a set of P participants and a set of I intensifiers. We created a "resampled" dataset where we crossed the resampled participants with the resampled intensifiers and collected the corresponding responses for each pair. From this analysis, we determined that with the 71 intensifiers we collected from grammars of English, 50 was the minimum number of subjects for power at the 0.8 level for the surprisal term. We doubled this amount and ran 100 participants in the replication experiment (Experiment 1b). Despite having sufficient power for the surprisal term, our bootstrapped power analysis actually suggests that even with 100 participants, the power for the syllables term in the model is only 0.29.
# 
# Reviewer 1 also pointed out that our choices for novel words in Experiment 3 and Experiment 4 could potentially have introduced a confound if our choice were not systematic and independent of the hypothesis. Our items for the novel words in this experiment were taken from those in previous studies on complexity bias (Lewis et al., 20--). As these items were developed with a different hypothesis in mind, we do not believe their use in our experiments introduces a confound. We appreciate Reviewer 1's conscientiousness in pointing out this possible issue.
# 
# Reviewer 2 pointed out that our reasoning for this account would benefit from more clarity, especially when appealing to parsimony. Reviewer 2 also suggested that discussing our formal model in the body of the paper might fasciliate this argument. We have revised our discussion to make the point that this understanding of intensifiers is quite minimal in its assumptions, given that it involves barely any extension to an existing model of language understanding which has been applied to many other linguistic phenomena. We need only introduce the assumption that the composite phrase "{intensifer} expensive" has a similar semantics "expensive" in order to derive the relationship between cost and intensity. Under this account, we can compute the intensity of an arbitrary intensified adjective phrase pragmatically using the existing framework of RSA.