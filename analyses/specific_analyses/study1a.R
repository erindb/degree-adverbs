source("~/Settings/startup.R")

n_nonenglish = 0
n_did_not_follow_instructions = 0

load_intensifiers_data = function() {
  ## load intensifier data
  unigrams = read.csv("../data/web_1grams.csv")
  total_ngrams = 1024908267229
  freq = unigrams$frequency
  syll = unigrams$syllables
  names(freq) = names(syll) = unigrams$ngram
  
  ## load experiment data
  raw.df = read.csv("../data/study1a_data.csv")
  
  n_nonenglish <<- length(unique((raw.df %>%
                    filter(!(language %in% c(
                      "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
                      "ENGLISH", "ENG", "eng"))))$workerid))
  n_did_not_follow_instructions <<- length(unique((raw.df %>% filter(asses == "No"))$workerid))
  
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

r = with(df %>% group_by(intensifier) %>% 
           summarise(surprisal.centered=surprisal.centered[1],
                     syll.centered = syll.centered[1]), cor(surprisal.centered, syll.centered))

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

message("running likelihood ratio tests...")
# anova(m_colinear)
lr_diff_due_to_syll = anova(m_colinear, m_only_surp, test = "LRT")
lr_diff_due_to_surp = anova(m_colinear, m_only_syll, test = "LRT")


# Draw plots to check things:

residuals_normality_plot = ggplot(NULL, aes(x=resid(m_colinear))) +
  geom_density(alpha=0.1, size=0.3) +
  xlab("residuals")

residuals_by_surprisal = ggplot(NULL, aes(x=df$surprisal.centered, y=resid(m_colinear))) +
  geom_point(alpha=0.1, size=0.3) +
  geom_smooth(method="loess") +
  xlab("surprisal") +
  ylab("residuals")

residuals_by_length = ggplot(NULL, aes(x=df$syll.centered, y=resid(m_colinear))) +
  geom_point(alpha=0.1, size=0.3) +
  xlab("length") +
  ylab("residuals")

predicted_vs_actual = ggplot(NULL, aes(x=predict(m_colinear), y=df$logprice.scaled)) +
  geom_point(alpha=0.1, size=0.3) + geom_smooth(method="loess") +
  geom_smooth(method="lm") +
  xlab("predicted") +
  ylab("actual")

residuals_by_predicted = ggplot(NULL, aes(x=predict(m_colinear),  y=resid(m_colinear))) +
  geom_point(alpha=0.1, size=0.3) +
  geom_smooth(method="loess") +
  xlab("predicted") +
  ylab("actual")
ggsave("output/resid_by_predictor_1a.png", width=4, height=3)

png(filename="output/resid_qqplot_1a.png")
qqnorm( resid(m_colinear) )
qqline( resid(m_colinear) )
dev.off()

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



## Rescaling plots:

raw_dv_plot = df %>%
  ggplot(., aes(x=surprisal.centered, y=price,
                colour=syll, group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "loess", alpha = 0.1) +
  geom_line(aes(group=object), stat="smooth",method = "loess") +
  facet_wrap(~object) +
  geom_point(alpha=0.1, size=0.3) +
  ylab("price") +
  scale_colour_gradient(low="gray", high="black") +
  xlab("centered surprisal") + theme(legend.position = "none")

log_dv_plot = df %>%
  ggplot(., aes(x=surprisal.centered, y=log(price),
                colour=syll, group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "loess", alpha = 0.1) +
  geom_line(aes(group=object), stat="smooth",method = "loess") +
  facet_wrap(~object) +
  geom_point(alpha=0.1, size=0.3) +
  ylab("log price") +
  scale_colour_gradient(low="gray", high="black") +
  xlab("centered surprisal") + theme(legend.position = "none")

scaled_dv_plot = df %>%
  ggplot(., aes(x=surprisal.centered, y=logprice.scaled,
                colour=syll, group=paste(workerid, object))) +
  geom_line(stat="smooth",method = "loess", alpha = 0.1) +
  geom_line(aes(group=object), stat="smooth",method = "loess") +
  facet_wrap(~object) +
  geom_point(alpha=0.1, size=0.3) +
  ylab("scaled log price") +
  scale_colour_gradient(low="gray", high="black") +
  xlab("centered surprisal") + theme(legend.position = "none")

# install.packages("piecewiseSEM")
library(piecewiseSEM)
marginal_r_squared = sem.model.fits(m_colinear)$Marginal
conditional_r_squared = sem.model.fits(m_colinear)$Conditional
  


prop_variance_explained = with(df %>% group_by(intensifier) %>%
       summarise(surprisal = surprisal[[1]],
                 response = mean(logprice.scaled)),
     cor(surprisal, response))^2

df %>% group_by(intensifier) %>%
  summarise(surprisal = surprisal[[1]],
            response = mean(logprice.scaled)) %>%
  lm(response ~ surprisal, .) %>%
  summary




  
