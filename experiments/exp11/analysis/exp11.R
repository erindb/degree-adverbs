### load required packages
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(rjson, quietly=T)
source("~/opt/r_helper_scripts/bootsSummary.r")

### parameters to vary in the analysis
exclude_impatient = F
exclude_manipulation_check = F

d = read.table("~/everything-cocolab/degree-adverbs/experiments/exp11/analysis/exp11.csv", sep=",", header=T)
if (exclude_impatient) {
  d = ddply(d, .(workerid), .fun=function(subd) {
    subject_is_impatient = sum(as.logical(subd$impatient[is.na(subd$qtype)]))>1
    subd$impatient = rep(subject_is_impatient, nrow(subd))
    return(subd)
  })
  n_total_workers = length(unique(d$workerid))
  d = d[d$impatient == F,]
  n_good_workers = length(unique(d$workerid))
  n_excluded_workers_impatience = n_total_workers - n_good_workers
}
if (exclude_manipulation_check) {
  d = ddply(d, .(workerid), .fun=function(subd) {
    target_freq = as.numeric(as.character(subd$response[subd$qtype == "frequency" & subd$word_type == "target" & !is.na(subd$response)]))
    control_freq = as.numeric(as.character(subd$response[subd$qtype == "frequency" & subd$word_type == "control" & !is.na(subd$response)]))
    subject_got_manipulation = target_freq > control_freq
    subd$got_manipulation = rep(subject_got_manipulation, nrow(subd))
    return(subd)
  })
  n_total_workers = length(unique(d$workerid))
  d = d[d$got_manipulation == T,]
  n_good_workers = length(unique(d$workerid))
  n_excluded_workers_manipulation = n_total_workers - n_good_workers
}
d$condition = d$Answer.target
d = d[d$qtype %in% c("frequency", "price"),c("workerid", "word_type", "word", "response", "qtype", "condition")]
d$response = as.numeric(as.character(d$response))
d = ddply(d, .(workerid, qtype), .fun=function(subd) {
  subd$diff = subd$response - subd$response[!(subd$word_type %in% c("control", "target"))]
  return(subd)
})
#d$diff[d$qtype == "frequency"] = NA
prices = d[d$qtype == "price",]
freqs = d[d$qtype == "frequency",]

# #by worker responses: condition = "madly"
# p = ggplot(prices[prices$condition == '"madly"',], aes(x=word_type, y=response, fill=word)) +
#   geom_bar(stat="identity", position="identity") +
#   facet_wrap(~ workerid) +
#   ylab("price") +
#   ggtitle("MADLY")
# print(p)
# 
# #by worker responses: condition = "truly"
# p = ggplot(prices[prices$condition == '"truly"',], aes(x=word_type, y=response, fill=word)) +
#   geom_bar(stat="identity", position="identity") +
#   facet_wrap(~ workerid) +
#   ylab("price") +
#   ggtitle("TRULY")
# print(p)
# 
#by worker frequency responses
p = ggplot(freqs, aes(x=word_type, y=response, fill=word_type)) +
  geom_bar(stat="identity", position="identity") +
  facet_wrap(~ workerid, scale="free") +
  ylab("frequency") +
  ggtitle("by worker frequency responses")
print(p)

#bare price between conditions
bare_summary = bootsSummary(prices[prices$word_type == "bare",],
                            measurevar="response", groupvars=c("condition"))
p = ggplot(bare_summary, aes(x=condition, y=response, fill=condition)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=condition, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  ylab("price") +
  ggtitle("bare price between conditions")
print(p)
#no difference

#all worker responses
p = ggplot(prices, aes(x=word_type, y=response, colour=word_type)) +
  geom_point(alpha=1/3, size=5) +
  facet_wrap(~ condition) +
  ggtitle("bare price between conditions")
print(p)

#responses by word_type for different words
intensifiers_summary = bootsSummary(prices[prices$word_type %in% c("target", "control"),], measurevar="response", groupvars=c("word", "word_type"))
p = ggplot(intensifiers_summary, aes(x=word_type, y=response, fill=word_type)) +
  geom_bar(stat="identity", position="identity") +
  facet_wrap(~ word) +
  geom_errorbar(aes(x=word_type, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  ylab("price") +
  ggtitle("responses by word_type for different words")
print(p)

#difference by word_type
intensifiers_summary = bootsSummary(prices[prices$word_type %in% c("target", "control"),], measurevar="diff", groupvars=c("word", "word_type"))
p = ggplot(intensifiers_summary, aes(x=word_type, y=diff, fill=word_type)) +
  geom_bar(stat="identity", position="identity") +
  facet_wrap(~ word) +
  geom_text(aes(x=word_type, y=4, label=N)) +
  geom_errorbar(aes(x=word_type, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  ylab("difference from bare form") +
  ggtitle("difference by word_type for different words")
print(p)

#difference by word_type
intensifiers_summary = bootsSummary(freqs[freqs$word_type %in% c("target", "control"),], measurevar="response", groupvars=c("word", "word_type"))
p = ggplot(intensifiers_summary, aes(x=word_type, y=response, fill=word_type)) +
  geom_bar(stat="identity", position="identity") +
  facet_wrap(~ word) +
  geom_text(aes(x=word_type, y=4, label=N)) +
  geom_errorbar(aes(x=word_type, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  ylab("frequency") +
  ggtitle("difference by word_type for different words")
print(p)

#responses by word_type in different conditions
intensifiers_summary = bootsSummary(prices, measurevar="response", groupvars=c("condition", "word_type"))
p = ggplot(intensifiers_summary, aes(x=word_type, y=response, fill=word_type)) +
  geom_bar(stat="identity", position="identity") +
  facet_wrap(~ condition) +
  geom_errorbar(aes(x=word_type, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  ylab("price") +
  ggtitle("responses by word_type in different conditions")
print(p)

#responses by word_type in different conditions
p = ggplot(d, aes(x=response)) +
  geom_histogram() +
  facet_wrap(~ qtype) +
  ggtitle("")
print(p)
# 
#frequency by price
dfp = reshape(d[d$qtype %in% c("frequency", "price") & d$word_type %in% c("target", "control"),],
              direction="wide", timevar="qtype", idvar=c("workerid", "word_type", "word", "condition"))
dfp$response.frequency[dfp$response.frequency == 0] = 0.5
dfp$response.price[dfp$response.price == 0] = 0.5
p = ggplot(data=dfp, aes(x=response.frequency, y=response.price#, colour=word
                         ))+
  geom_point() +
  theme_bw(18) +
  #facet_wrap(~ word_type) +
  ylab("price") +
  theme(panel.grid=element_blank()) +
  geom_smooth(method="lm")
print(p)

ddd = reshape(d[d$qtype %in% c("frequency", "price") & d$word_type %in% c("target", "control"),],
              direction="wide", timevar="qtype", idvar=c("workerid", "word_type", "word", "condition"))
dfp$diff.frequency[dfp$diff.frequency == 0] = 0.5
dfp$diff.price[dfp$diff.price == 0] = 0.5
p = ggplot(data=dfp, aes(x=diff.frequency, y=diff.price#, colour=word
))+
  geom_point() +
  theme_bw(18) +
  #facet_wrap(~ word_type) +
  theme(panel.grid=element_blank()) +
  geom_smooth(method="lm")
print(p)


# 
library(lme4)
# library(lmerTest)
fit = lmer(diff ~ word_type + (1 | word), data=prices[prices$word_type %in% c("control", "target"),])
print(summary(fit))
# 
d = ddply(d, .(workerid), .fun=function(subd) {
  target_freq = as.numeric(as.character(subd$response[subd$qtype == "frequency" & subd$word_type == "target" & !is.na(subd$response)]))
  control_freq = as.numeric(as.character(subd$response[subd$qtype == "frequency" & subd$word_type == "control" & !is.na(subd$response)]))
  subject_got_manipulation = target_freq > control_freq
  subd$got_manipulation = rep(subject_got_manipulation, nrow(subd))
  return(subd)
})


hello = ddply(dfp, .(workerid), .fun=function(subd) {
  f = subd$response.frequency[subd$word_type == "control"] / subd$response.frequency[subd$word_type == "target"]
  p = subd$response.price[subd$word_type == "control"] / subd$response.price[subd$word_type == "target"]
  subd = subd[1,]
  subd$f = rep(f, nrow(subd))
  subd$p = rep(p, nrow(subd))
  return(subd)
})

tmp = dcast(~word_type)
hello = ddply(dfp, .(workerid), summarise, f=response.f)

hello = hello[,c("workerid", "word_type", "condition", "response.frequency", "response.price", "f", "p")]

p = ggplot(data=hello, aes(x=f, y=p#, colour=word
))+
  geom_point() +
  theme_bw(18) +
  geom_text(aes(label=workerid)) +
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1) +
  #facet_wrap(~ word_type) +
  theme(panel.grid=element_blank()) +
  geom_smooth()
print(p)




hello = ddply(dfp, .(workerid), .fun=function(subd) {
  f = subd$response.frequency[subd$word_type == "control"] - subd$response.frequency[subd$word_type == "target"]
  p = subd$response.price[subd$word_type == "control"] - subd$response.price[subd$word_type == "target"]
  subd = subd[1,]
  subd$f = rep(f, nrow(subd))
  subd$p = rep(p, nrow(subd))
  return(subd)
})
hello = hello[,c("workerid", "word_type", "condition", "response.frequency", "response.price", "f", "p")]

p = ggplot(data=hello, aes(x=f, y=p#, colour=word
))+
  geom_point() +
  theme_bw(18) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_text(aes(label=workerid)) +
  #facet_wrap(~ word_type) +
  theme(panel.grid=element_blank()) +
  geom_smooth()
print(p)

minidfp = dfp[dfp$word_type %in% c("target", "control"),
          c("workerid", "word_type", "response.price", "response.frequency")]

head(minidfp)

w = reshape(minidfp, direction="wide", timevar="word_type", idvar=c("workerid"))
w$freq_diff = w$response.frequency.control - w$response.frequency.target
w$price_diff = w$response.price.control - w$response.price.target
w$freq_ratio = w$response.frequency.control / w$response.frequency.target
w$price_ratio = w$response.price.control / w$response.price.target
head(w)

p = ggplot(data=w, aes(x=freq_diff, y=price_diff#, colour=word
))+
  geom_point() +
  theme_bw(18) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_text(aes(label=workerid)) +
  #facet_wrap(~ word_type) +
  theme(panel.grid=element_blank()) +
  geom_smooth()
print(p)


p = ggplot(data=w, aes(x=freq_ratio, y=price_ratio#, colour=word
))+
  geom_point() +
  theme_bw(18) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_text(aes(label=workerid)) +
  #facet_wrap(~ word_type) +
  theme(panel.grid=element_blank()) +
  geom_smooth()
print(p)