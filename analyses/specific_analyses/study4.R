
source("~/Settings/startup.R")

# load dependencies
library(boot)
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
d = read.csv("../data/study4_data.csv")


n_nonenglish = length(unique((d %>%
                                filter(!(language %in% c(
                                  "English",  "english",  "Engli", "en",
                                  "Englsih",  "engish",   "English ", "Enlgish",
                                  "ENGLISH", "ENG", "eng"))))$workerid))
n_did_not_follow_instructions = length(unique((d %>% filter(asses == "No"))$workerid))


total_ngrams = 1024908267229
ngrams = read.csv("../data/web_1grams.csv")
ngrams$surprisal = - (log(ngrams$frequency) - log(total_ngrams))

total_workers = length(unique(d$workerid))
d = subset(d, asses == "Yes" | asses == "Confused" | is.na(asses))
good_workers = length(unique(d$workerid))

df = d %>%
  mutate(adjective = char(adjective),
         adverb = sapply(char(adverb), function(adv) {
           return(strsplit(adv, " ")[[1]][1])
         })) %>%
  group_by(workerid, adjective) %>%
  mutate(ranking = rank(ranking),
         height_in_list = 9 - ranking) %>%
  ungroup() %>% as.data.frame %>%
  select(workerid, height_in_list, adverb, adjective)



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
  # ggplot(aggdf, aes(x=adverb, y=height_in_list, colour=adverb)) +
  #   geom_point(size=3) +
  #   geom_errorbar(aes(x=adverb, ymin=height_in_list_low, ymax=height_in_list_high))
  
  sink(file="output/Experiment4/novel-model.txt")
  df$ranking = ordered(df$ranking)
  df$height_in_list = ordered(df$height_in_list)
  model = clm(height_in_list ~ length, data=df)
  model_with_root = clm(height_in_list ~ length + root, data=df)
  print(summary(model))
  print(summary(model_with_root))
  sink(NULL)
}

novel(d)

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

detach(package:plyr, unload=T)
