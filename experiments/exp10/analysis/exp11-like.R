### load required packages
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(rjson, quietly=T)
source("~/opt/r_helper_scripts/bootsSummary.r")

### parameters to vary in the analysis
exclude_impatient = T
exclude_manipulation_check = T

d = read.table("~/everything-cocolab/degree-adverbs/experiments/exp10/analysis/exp10.csv", sep=",", header=T)
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
    subject_got_manipulation = target_freq >= control_freq
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
d$diff[d$qtype == "frequency"] = NA
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
# #by worker frequency responses
# p = ggplot(freqs, aes(x=word_type, y=response, fill=word_type)) +
#   geom_bar(stat="identity", position="identity") +
#   facet_wrap(~ workerid, scale="free") +
#   ylab("frequency") +
#   ggtitle("by worker frequency responses")
# print(p)

#bare price between conditions
bare_summary = bootsSummary(prices[prices$word_type == "bare",], measurevar="response", groupvars=c("condition"))
p = ggplot(bare_summary, aes(x=condition, y=response, fill=condition)) +
  geom_bar(stat="identity", position="identity") +
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

# #responses by word_type for different words
# intensifiers_summary = bootsSummary(prices[prices$word_type %in% c("target", "control"),], measurevar="response", groupvars=c("word", "word_type"))
# p = ggplot(intensifiers_summary, aes(x=word_type, y=response, fill=word_type)) +
#   geom_bar(stat="identity", position="identity") +
#   facet_wrap(~ word) +
#   geom_errorbar(aes(x=word_type, ymin=bootsci_low, ymax=bootsci_high), width=0) +
#   ylab("price") +
#   ggtitle("responses by word_type for different words")
# print(p)

#difference by word_type
intensifiers_summary = bootsSummary(prices[prices$word_type %in% c("target", "control"),], measurevar="diff", groupvars=c("word", "word_type"))
p = ggplot(intensifiers_summary, aes(x=word_type, y=diff, fill=word_type)) +
  geom_bar(stat="identity", position="identity") +
  facet_wrap(~ word) +
  geom_errorbar(aes(x=word_type, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  ylab("difference from bare form") +
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
# 
# #frequency by price
# dfp = reshape(d[d$qtype %in% c("frequency", "price") & d$word_type %in% c("target", "control"),
#                 c("workerid", "word_type", "word", "response", "qtype", "condition")],
#               direction="wide", timevar="qtype", idvar=c("workerid", "word_type", "word", "condition"))
# dfp$response.frequency[dfp$response.frequency == 0] = 0.1
# dfp$response.price[dfp$response.price == 0] = 0.1
# p = ggplot(data=dfp, aes(x=-log(response.frequency/1000), y=response.price, colour=word))+
#   geom_point() +
#   theme_bw(18) +
#   facet_wrap(~ condition) +
#   xlab("frequency") +
#   ylab("price") +
#   theme(panel.grid=element_blank()) +
#   geom_smooth(method="lm")
# print(p)
# 
library(lme4)
library(lmerTest)
fit = lmer(diff ~ word_type + (1 | word), data=prices[prices$word_type %in% c("control", "target"),])
print(summary(fit))
# 
