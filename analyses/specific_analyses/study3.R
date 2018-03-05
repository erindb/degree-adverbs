source("~/Settings/startup.R")

## load intensifier data
unigrams = read.csv("../data/web_1grams.csv")
total_ngrams = 1024908267229
freq = unigrams$frequency
syll = unigrams$syllables
names(freq) = names(syll) = unigrams$ngram

novel_intensifiers = c(
  "lopusly", "fepolopusly",
  "ratumly", "gaburatumly",
  "bugornly", "tupabugornly"
)

## load experiment data
raw.df = read.csv("../data/study3_data.csv")

n_nonenglish = length(unique((raw.df %>%
                                  filter(!(language %in% c(
                                    "English",  "english",  "Engli",    "Englsih",  "engish",   "English ",
                                    "ENGLISH", "ENG", "eng"))))$workerid))
n_did_not_follow_instructions = length(unique((raw.df %>% filter(asses == "No"))$workerid))

full_df = raw.df %>%
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
         logprice.scaled = z)
df = full_df %>%
  filter(!(intensifier %in% novel_intensifiers)) %>%
  ungroup %>%
  mutate(freq = freq[as.character(intensifier)],
         syll = syll[as.character(intensifier)],
         syll.centered = syll - mean(syll),
         surprisal = -log(freq/total_ngrams),
         surprisal.centered = surprisal - mean(surprisal),
         syll.scaled = scale(syll),
         surprisal.scaled = scale(surprisal),
         word.cost = 0.18124*syll.centered + 0.10704*surprisal.centered)

r = with(df %>% group_by(intensifier) %>% 
           summarise(surprisal.centered=surprisal.centered[1],
                     syll.centered = syll.centered[1]), cor(surprisal.centered, syll.centered))

## colinearity
surp_by_syll = lm(surprisal.centered ~ syll.centered, df)
syll_by_surp = lm(syll.centered ~ surprisal.centered, df)
df = df %>% mutate(
  surprisal_resid = resid(surp_by_syll),
  syll_resid = resid(syll_by_surp))
  
lookup_root_of_novel_intensifier = function(word) {
  if (word=="lopusly" | word=="fepolopusly") {
    return("lopus")
  } else if (word=="ratumly" | word=="gaburatumly") {
    return("ratum")
  } else {
    return("bugorn")
  }
}
novel_df = full_df %>% filter(intensifier %in% novel_intensifiers) %>%
  mutate(root = lookup_root_of_novel_intensifier(intensifier),
         length = ifelse(nchar(char(intensifier)) < 9, "short", "long"))
plot3a = novel_df %>%
  group_by(intensifier, workerid) %>%
  summarise(
    root = root[[1]],
    length = length[[1]],
    logprice.scaled = mean(logprice.scaled)) %>%
  summarise(
    root = root[[1]],
    length = length[[1]],
    low = ci.low(logprice.scaled),
    high = ci.high(logprice.scaled),
    logprice.scaled = mean(logprice.scaled)) %>%
  ggplot(., aes(x=length, y=logprice.scaled)) +
  geom_bar(stat="identity") +
  facet_wrap(~root) +
  geom_errorbar(aes(ymin=low, ymax=high), width=0) +
  xlab("length of nonce word") +
  ylab("normalized log price") +
  ggtitle("Study 3")
print(plot3a)
ggsave("../paper/images/plot_study3_A.pdf", width=6, height=3)

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



novel_m = novel_df %>%
  mutate(length = ifelse(length=="short", -1, 1)) %>%
  lmer(logprice.scaled ~ length +
         (0 + length | workerid) +
         (1 | root), data=.)


novel_m_with_root = novel_df %>%
  mutate(length = ifelse(length=="short", -1, 1)) %>%
  lmer(logprice.scaled ~ length + root +
         (0 + length | workerid), data=.)




library(ggrepel)
df %>%
  group_by(intensifier) %>%
  summarise(surprisal.centered = surprisal.centered[[1]],
            syllables = syll[[1]],
            low = ci.low(logprice.scaled),
            high = ci.high(logprice.scaled),
            logprice.scaled = mean(logprice.scaled)) %>%
  ggplot(., aes(x=surprisal.centered,
                y=logprice.scaled,
                colour=syllables)) +
  geom_smooth(method="lm", colour="gray", alpha=0.1) +
  geom_point() +
  geom_text_repel(size=3, aes(label=intensifier)) +
  geom_errorbar(aes(ymin=low, ymax=high), width=0) +
  # geom_text_repel(aes(label=intensifier)) +
  # facet_wrap(~object, scale="free") +
  ylab("normalized log price") +
  scale_colour_gradient(low="gray", high="black") +
  xlab("centered surprisal") +
  ggtitle("Study 3") +
  theme_bw()
# ggsave("../edited_draft/images/plot_study1b.pdf", width=8, height=2.5)
ggsave("../paper/images/plot_study3_B.pdf", width=5, height=3)



intensities = full_df %>% group_by(intensifier) %>%
  summarise(
    low = ci.low(logprice.scaled),
    high = ci.high(logprice.scaled),
    mean_logprice = mean(logprice.scaled)) %>%
  mutate(
    low = (low - mean(mean_logprice))/sd(mean_logprice),
    high = (high - mean(mean_logprice))/sd(mean_logprice),
    intensity = (mean_logprice - mean(mean_logprice))/sd(mean_logprice)) %>%
  select(intensifier, intensity, low, high)
intensities = intensities[order(intensities$intensity),]
write.csv(intensities, "output/intensities_study3.csv", row.names=F)


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

library(pbkrtest)

# get the KR-approximated degrees of freedom
coefs = summary(novel_m)$coefficients
df.KR <- get_ddf_Lb(novel_m, fixef(novel_m))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
p.KR <- 2 * (1 - pt(abs(coefs[,"t value"]), df.KR))
p.KR

# get the KR-approximated degrees of freedom
coefs = summary(novel_m_with_root)$coefficients
df.KR <- get_ddf_Lb(novel_m_with_root, fixef(novel_m_with_root))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
p.KR <- 2 * (1 - pt(abs(coefs[,"t value"]), df.KR))
p.KR

anova(novel_m_with_root)
coefs = summary(novel_m_with_root)$coefficients
f = t(anova(novel_m_with_root)["F value"])
df1 = t(anova(novel_m_with_root)["F value"])
df.KR = get_ddf_Lb(novel_m_with_root, fixef(novel_m_with_root))
2 * (1 - pf(abs(f), df1, df.KR))
2 * (1 - pt(abs(coefs[,"t value"]), df.KR))


novel_m_with_root_ratum = novel_df %>%
  mutate(length = ifelse(length=="short", -1, 1)) %>%
  mutate(lopusvbugorn = ifelse(root=="lopus", 1, ifelse(root=="bugorn", -1, 0))) %>%
  mutate(ratumvother = ifelse(root=="ratum", 2, -1)) %>%
  lmer(logprice.scaled ~ length + lopusvbugorn + ratumvother +
         (0 + length | workerid), data=.)
coefs = summary(novel_m_with_root_ratum)$coefficients
df.KR <- get_ddf_Lb(novel_m_with_root_ratum, fixef(novel_m_with_root_ratum))
# 2 * (1 - pf(abs(t(anova(novel_m_with_root_ratum)["F value"])), df1, df.KR))
2 * (1 - pt(abs(coefs[,"t value"]), df.KR))


get_ddf_Lb(m_colinear, fixef(m_colinear))
summary(m_colinear)
