source("~/Settings/startup.R")

load_intensifiers_data = function() {
  ## load intensifier data
  unigrams = read.csv("../data/web_1grams.csv")
  total_ngrams = 1024908267229
  freq = unigrams$frequency
  syll = unigrams$syllables
  names(freq) = names(syll) = unigrams$ngram
  
  ## load experiment data
  raw.df = read.csv("../data/study1a_data.csv")
  df = raw.df %>%
    filter(language %in% c(
      "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
      "ENGLISH", "ENG", "eng") &
        asses != "No") %>%
    mutate(price = response,
           intensifier = as.character(adverb)) %>%
    select(workerid, price, intensifier, object) %>%
    mutate(logprice = as.numeric(log(price))) %>%
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
           surprisal = -log(freq/total_ngrams),
           surprisal.centered = surprisal - mean(surprisal),
           syll.scaled = scale(syll),
           surprisal.scaled = scale(surprisal),
           word.cost = 0.18124*syll.centered + 0.10704*surprisal.centered)

  ## colinearity
  surp_by_syll = lm(surprisal.centered ~ syll.centered, df)
  syll_by_surp = lm(syll.centered ~ surprisal.centered, df)
  df = df %>% mutate(
    surprisal_resid = resid(surp_by_syll),
    syll_resid = resid(syll_by_surp))
  
  return(df)
}

df = load_intensifiers_data()
message("data loaded")

r = with(df, cor(surprisal.centered, syll.centered))

message("running colinear model...")
m_colinear = lmer(logprice.scaled ~ 1 + syll.centered + surprisal.centered +
                    (0 + syll.centered + surprisal.centered | workerid) +
                    (1 | intensifier), df)

message("running residualized models...")
m_resid_surp = lmer(logprice.scaled ~ 1 + syll.centered + surprisal_resid +
    (0 + syll.centered + surprisal_resid | workerid) +
      (1 | intensifier), df)
m_resid_syll = lmer(logprice.scaled ~ 1 + syll_resid + surprisal.centered +
    (0 + syll_resid + surprisal.centered | workerid) +
      (1 | intensifier), df)

message("running simplified models...")
m_only_syll = lmer(logprice.scaled ~ 1 + syll.centered +
    (0 + syll.centered | workerid) +
      (1 | intensifier), df)
m_only_surp = lmer(logprice.scaled ~ 1 + surprisal.centered +
    (0 + surprisal.centered | workerid) +
      (1 | intensifier), df)

message("running anovas...")
# anova(m_colinear)
lr_diff_due_to_syll = anova(m_colinear, m_only_surp)
lr_diff_due_to_surp = anova(m_colinear, m_only_syll)


# Draw plots to check things:

residuals_normality_plot = ggplot(NULL, aes(x=resid(m_colinear))) +
  geom_density() +
  xlab("residuals")

residuals_by_surprisal = ggplot(NULL, aes(x=df$surprisal.centered, y=resid(m_colinear))) +
  geom_point() +
  geom_smooth(method="loess") +
  xlab("surprisal") +
  ylab("residuals")

residuals_by_length = ggplot(NULL, aes(x=df$syll.centered, y=resid(m_colinear))) +
  geom_point() +
  xlab("length") +
  ylab("residuals")

predicted_vs_actual = ggplot(NULL, aes(x=predict(m_colinear), y=df$logprice.scaled)) +
         geom_point() + geom_smooth(method="loess") +
         geom_smooth(method="lm") +
  xlab("predicted") +
  ylab("actual")

# Results:

library(ggrepel)
plot1a = df %>% group_by(intensifier, object) %>%
  summarise(surprisal.centered = surprisal.centered[[1]],
            syll = syll[[1]],
            logprice.scaled = mean(logprice.scaled)) %>%
  summarise(surprisal.centered = surprisal.centered[1],
            syllables = syll[1],
            low = ci.low(logprice.scaled),
            high = ci.high(logprice.scaled),
            logprice.scaled = mean(logprice.scaled)) %>%
  ggplot(., aes(x=surprisal.centered,
                y=logprice.scaled,
                colour=syllables)) +
  geom_smooth(method="lm", colour="gray", alpha=0.1) +
  geom_point() +
  geom_errorbar(aes(ymin=low, ymax=high)) +
  # geom_text_repel(aes(label=intensifier)) +
  # facet_wrap(~object, scale="free") +
  ylab("normalized log price") +
  scale_colour_gradient(low="gray", high="black") +
  xlab("centered surprisal") +
  ggtitle("Study 1a") +
  theme_bw()
print(plot1a)
ggsave("../paper/images/plot_study1a.pdf", width=5, height=3)


df %>% group_by(intensifier) %>%
  summarise(freq=freq[[1]],
            syll=syll[[1]]) %>%
  write.csv("output/intensifiers_for_table.csv", row.names=F)


intensities = df %>% group_by(intensifier) %>%
  summarise(
    low = ci.low(logprice.scaled),
    high = ci.high(logprice.scaled),
    mean_logprice = mean(logprice.scaled)) %>%
  mutate(
    low = low/sd(mean_logprice) - mean(mean_logprice),
    high = high/sd(mean_logprice) - mean(mean_logprice),
    intensity = mean_logprice/sd(mean_logprice) - mean(mean_logprice)) %>%
  select(intensifier, intensity, low, high)
intensities = intensities[order(intensities$intensity),]
write.csv(intensities, "output/intensities_study1a.csv", row.names=F)

proportion_variance_explained = cor(predict(m_colinear), df$logprice.scaled)^2
