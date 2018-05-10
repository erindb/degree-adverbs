

# title: "Intensifiers List"
# author: "Erin Bennett"
# output: 
#   html_document:
#       toc: false
source("startup.R")

summarise_category = function(lst) {
  if ("amplifier" %in% lst) {
    return("amplifier")
  } else if ("diminisher" %in% lst) {
    return("downtoner")
  } else {
    return("intensifier")
  }
}
raw.df = read.csv("../data/intensifiers_list_from_grammars.csv")
n.grammars = length(unique(raw.df$source))
df = raw.df %>%
  filter(obvious.downtoner=="no") %>%
  # filter(!(obvious.downtoner=="yes")) %>%
  filter(!(weird=="weird")) %>%
  filter(region != "BrE") %>%
  group_by(word) %>%
  summarise(category = summarise_category(category),
            obvious.downtoner=obvious.downtoner[1]) %>%
  filter(category != "downtoner")

old.df = read.csv("../data/web_1grams.csv")
old.intensifiers = old.df$ngram
new.intensifiers = df$word

intersection = intersect(old.intensifiers, new.intensifiers)
newly.introduced = setdiff(new.intensifiers, old.intensifiers)
lost = setdiff(old.intensifiers, new.intensifiers)

# Total grammars with reference to "intensifiers", "amplifiers", or "degree adverbs" in their index: `r n.grammars`
# 
# Relation to old list:
# 
# * Intersection: `r length(intersection)`
# * New words from grammars: `r length(newly.introduced)`
# * My words not in grammars: `r length(lost)`
# 
# New list:

print(as.character(
  new.intensifiers))
write.csv(x=new.intensifiers, file="output/new-intensifiers.csv")

counts = raw.df %>% group_by(source) %>%
  summarise(N=length(word))
mean(counts$N)

frequencies = read.csv("../data/web_1grams.csv")
freq_list = frequencies$frequency
names(freq_list) = char(frequencies$ngrams)

