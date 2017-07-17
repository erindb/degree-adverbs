source("~/Settings/startup.R")

# load dependencies
library(boot)
library(ordinal)
# library(plyr)
library(languageR)
library(mlogit)

## load data

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
d = read.csv("../data/study2_data.csv")
total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))



n_nonenglish = length(unique((d %>%
                                filter(!(language %in% c(
                                  "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
                                  "ENGLISH", "ENG", "eng"))))$workerid))
n_did_not_follow_instructions = length(unique((d %>% filter(asses == "No"))$workerid))

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

df = d %>% filter(asses!="No" | is.na(asses))

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

## colinearity
surp_by_syll = lm(surprisal ~ syllables, df)
syll_by_surp = lm(syllables ~ surprisal, df)
df = df %>% mutate(
  surprisal_resid = resid(surp_by_syll),
  syll_resid = resid(syll_by_surp))

conditions = df %>% group_by(adverb, adjective) %>% summarise() %>% as.data.frame
aggdf = do.call(rbind, lapply(1:nrow(conditions), function(i) {
  av = char(conditions$adverb)[i]
  aj = char(conditions$adjective)[i]
  subd = df %>% filter(adverb==av & adjective==aj)
  resampled = boot(subd, function(orig, indices) {
    return( mean(orig[indices,]$height_in_list) )
  }, 100)$t
  newd = data.frame(
    adverb = av,
    adjective = aj,
    surprisal = subd$surprisal[[1]],
    surprisal_resid = subd$surprisal_resid[[1]],
    syll_resid = subd$syll_resid[[1]],
    syllables = subd$syllables[[1]],
    adverb_list = subd$adverb_list[[1]],
    height_in_list = mean(subd$height_in_list),
    height_in_list_high = quantile(resampled, 0.025),
    height_in_list_low = quantile(resampled, 0.975)
  )
  return(newd)
}))

p = aggdf %>%
  mutate(syllables = as.numeric(syllables)) %>%
  ggplot(data=., aes(x=surprisal, y=height_in_list, colour=syllables)) +
  geom_smooth(method="lm", colour="gray", alpha=1/10) +
  geom_point(
    # size=1
  ) +
  geom_errorbar(aes(ymin=height_in_list_low, ymax=height_in_list_high, x=surprisal), width=0) +
  theme_bw() +
  facet_grid(. ~ adjective, scale="free") +
  xlab("surprisal") +
  ylab("height in list") +
  # scale_colour_continuous(name="syllables") +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  ggtitle("Study 2") +
  #   scale_colour_brewer(type="div", palette=7) +
  #   scale_fill_brewer(type="div", palette=7) +
  scale_colour_gradient(low="gray", high="black") +
  theme(panel.grid=element_blank())
print(p)
ggsave("../paper/images/plot_study2.pdf", width=10, height=3)

response_order = aggdf %>% group_by(adverb) %>%
  summarise(height_in_list = mean(height_in_list))
aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$height_in_list)])

centered = cbind(df, myCenter(df[,c("surprisal","syllables","surprisal_resid", "syll_resid")]))



r = with(df %>% group_by(adverb) %>% 
           summarise(surprisal=surprisal[1],
                     syllables = syllables[1]), cor(surprisal, syllables))




df$ch = df$ranking + 1
df$chid = paste(df$adverb_list, df$workerid)
G <- mlogit.data(df, choice = "ch", shape = "long", chid.var="chid",
                 alt.var="adverb", ranked = TRUE)
m_colinear = mlogit(ch ~ surprisal + syllables | 0, G)
model_with_adj_interaction = mlogit(ch ~ surprisal + syllables + surprisal:adjective + syllables:adjective | 0, G)

message("running simplified models...")
m_only_syll = mlogit(ch ~ syllables | 0, G)
m_only_surp = mlogit(ch ~ surprisal | 0, G)

message("running residualized models...")
m_resid_syll = mlogit(ch ~ surprisal + syll_resid | 0, G)
m_resid_surp = mlogit(ch ~ surprisal_resid + syllables | 0, G)

message("running likelihood ratio tests...")
# anova(m_colinear)
lr_diff_due_to_syll = lrtest(m_colinear, m_only_surp)
lr_diff_due_to_surp = lrtest(m_colinear, m_only_syll)

intensities = df %>% 
  rename(intensifier = adverb) %>%
  group_by(intensifier) %>%
  summarise(
    low = ci.low(height_in_list),
    high = ci.high(height_in_list),
    mean_height_in_list = mean(height_in_list)) %>%
  mutate(
    low = (low - 
      mean(mean_height_in_list))/sd(mean_height_in_list),
    high = (high - 
      mean(mean_height_in_list))/sd(mean_height_in_list),
    intensity = (mean_height_in_list -
      mean(mean_height_in_list))/sd(mean_height_in_list)) %>%
  select(intensifier, intensity, low, high)
intensities = intensities[order(intensities$intensity),]
write.csv(intensities, "output/intensities_study2.csv", row.names=F)

