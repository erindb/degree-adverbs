# ---
# title: "Intensifiers: Aggregate effect size"
# author: "Erin Bennett"
# output: 
#   html_document:
#       toc: false
# ---
# 
# 
# ```{r}
df1a = read.csv("output/intensities_study1a.csv") %>% mutate(study="1a")
df1b = read.csv("output/intensities_study1b.csv") %>% mutate(study="1b")
df2 = read.csv("output/intensities_study2.csv") %>% mutate(study="2")
df3 = read.csv("output/intensities_study3.csv") %>% mutate(study="3")
df4 = read.csv("output/intensities_study4.csv") %>% mutate(study="4")
# ```
# 
# ```{r}
df = rbind(df1a, df1b, df2, df3, df4)
mean_intensities = df %>%
  group_by(intensifier) %>%
  summarise(intensity = mean(intensity))
df = df %>%
  mutate(
    intensifier = factor(
      intensifier,
      levels=mean_intensities$intensifier[
        order(mean_intensities$intensity)
      ]
    )
  )
# ```
# 
# 
# ```{r, fig.height=10}
df = df %>%
  mutate(novel = intensifier %in% c(
    "bugornly", "tupabugornly",
    "ratumly", "gaburatumly",
    "lopusly", "fepolopusly"
  ))

colvec = ifelse(levels(df$intensifier) %in% c(
    "bugornly", "tupabugornly",
    "ratumly", "gaburatumly",
    "lopusly", "fepolopusly"
  ), "red", "black")

df %>%
  ggplot(., aes(y=intensifier, x=intensity,
                #colour=study,
                colour=novel,
                shape=study)) +
  geom_point(alpha=1/5) +
  geom_errorbarh(aes(xmin=low, xmax=high), height=0, alpha=1/5) +
  # scale_colour_brewer(type="qual", palette = 2) +
  scale_colour_manual(values=c("black", "red")) +
  # + theme(axis.text.x = element_text(angle=-90, hjust=0)) +
  theme(axis.text.y = element_text(colour=colvec))
ggsave("../paper/images/intensities.pdf", height=15, width=6)
# ```


