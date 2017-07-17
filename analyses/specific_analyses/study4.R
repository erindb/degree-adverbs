
source("~/Settings/startup.R")

# load dependencies
library(boot)
library(ordinal)
library(languageR)
library(mlogit)


## load data
# library(plyr)

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
d = read.csv("../data/study4_data.csv")
total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))



n_nonenglish = length(unique((d %>%
                                filter(!(language %in% c(
                                  "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
                                  "ENGLISH", "ENG", "eng"))))$workerid))
n_did_not_follow_instructions = length(unique((d %>% filter(asses == "No"))$workerid))


total_workers = length(unique(d$workerid))
uncooperative = length(unique(subset(d, asses=="No")$workerid))
d = subset(d, asses == "Yes" | asses == "Confused" | is.na(asses))
good_workers = length(unique(d$workerid))
print(total_workers)
print(uncooperative)
print(good_workers)


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




filler_df = df %>%
  filter(!(adverb %in% c(
    "bugornly", "tupabugornly",
    "ratumly", "gaburatumly",
    "lopusly", "fepolopusly"
  ))) %>%
  mutate(
    surprisal = sapply(adverb, function(adv) {
      return(ngrams$surprisal[adv == as.character(ngrams$ngram)][1])
    }),
    syllables = sapply(adverb, function(adv) {
      return(ngrams$syllables[adv == as.character(ngrams$ngram)][1])
    }))


aggdf = filler_df %>% 
  group_by(adverb, adjective) %>%
  summarise(
    syllables = syllables[[1]],
    surprisal = surprisal[[1]],
    high = ci.high(height_in_list),
    low = ci.low(height_in_list),
    height_in_list = mean(height_in_list)
  ) %>%
  mutate(syllables = as.numeric(syllables),
         surprisal = num(surprisal))

aggdf = aggdf %>% ungroup %>% as.data.frame %>%
  mutate(centered.surprisal = (surprisal - mean(surprisal)))

aggdf %>%
  ggplot(data=., aes(x=centered.surprisal, y=height_in_list,
                     colour=syllables)) +
  geom_smooth(method="lm", lwd=0, colour="gray", alpha=1/10) +
  geom_point(#size=3
  ) +
  geom_errorbar(aes(ymin=low, ymax=high), width=0) +
  theme_bw() +
  facet_grid(. ~ adjective, scale="free") +
  xlab("surprisal") +
  ylab("height in list") +
  # scale_colour_continuous(name="syllables") +
  scale_x_continuous(breaks=c(10, 14, 18)) +
  ggtitle("Study 4") +
  #   scale_colour_brewer(type="div", palette=7) +
  #   scale_fill_brewer(type="div", palette=7) +
  theme(panel.grid=element_blank()) +
  scale_colour_gradient(low="gray", high="black")
ggsave("../edited_draft/images/plot_study4.pdf", width=5, height=2.5)


#### fix me: dplyr or plyr or something

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
  
  aggdf = df %>% group_by(adverb) %>%
    summarise(
      height_in_list_high = ci.high(height_in_list),
      height_in_list_low = ci.low(height_in_list),
      height_in_list = mean(height_in_list),
      ranking_high = ci.high(ranking),
      ranking_low = ci.low(ranking),
      ranking = mean(ranking))
  
  # ggplot(aggdf, aes(x=adverb, y=height_in_list, colour=adverb)) +
  #   geom_point(size=3) +
  #   geom_errorbar(aes(x=adverb, ymin=height_in_list_low, ymax=height_in_list_high))
  
  df$ranking = ordered(df$ranking)
  df$height_in_list = ordered(df$height_in_list)
  
  
  
  model = clm(height_in_list ~ length, data=df)
  
  model_with_root = clm(height_in_list ~ length + root, data=df)
  print(summary(model))
  print(summary(model_with_root))
  
}

novel(d)

# df = d
# 
# df$adverb = as.character(df$adverb)
# df$adjective = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][2])})
# df$adverb = sapply(df$adverb, function(adv) {return(strsplit(adv, " ")[[1]][1])})
# # 
# # aggdf = ddply(df, .(adverb, adjective), function(subd) {
# #   resampled = boot(subd, function(orig, indices) {
# #     return( mean(orig[indices,]$height_in_list) )
# #   }, 100)$t
# #   newd = data.frame(
# #     adverb = subd$adverb[[1]],
# #     adjective = subd$adjective[[1]],
# #     height_in_list = mean(subd$height_in_list),
# #     height_in_list_high = quantile(resampled, 0.025),
# #     height_in_list_low = quantile(resampled, 0.975)
# #   )
# #   return(newd)
# # })
# 
# response_order = ddply(aggdf, .(adverb), summarise, height_in_list = mean(height_in_list))
# aggdf$adverb = factor(aggdf$adverb, levels = as.character(response_order$adverb)[order(response_order$height_in_list)])



df2 = df %>% group_by(adverb) %>%
  summarise(ranking=mean(ranking))
# df1b$intensifier[order(df1b$logprice)] %>%
df2 %>% write.csv("intensifiers_mean_logprice_study2.csv",
                   row.names=F)



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
write.csv(intensities, "intensities_study4.csv", row.names=F)




df = filler_df

df$ch = df$height_in_list + 1
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
