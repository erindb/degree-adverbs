### load required packages
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(rjson, quietly=T)
source("~/opt/r_helper_scripts/bootsSummary.r")

d = read.table("exp10.csv", sep=",", header=T)
#d = d[as.numeric(as.character(d$workerid)) > 39,]
d = d[d$qtype %in% c("frequency", "price"),c("workerid", "word_type", "word", "response", "qtype")]
d$response = as.numeric(as.character(d$response))
d = ddply(d, .(workerid, qtype), .fun=function(subd) {
  subd$diff = subd$response - subd$response[!(subd$word %in% c("madly", "truly"))]
  return(subd)
  })
df = bootsSummary(d[d$qtype == "frequency",], measurevar="response", groupvars=c("word_type", "word"))
p = ggplot(data=df, aes(x=word_type, y=response, fill=word_type)) +
  geom_bar(stat="identity") +
  facet_wrap(~ word) +
  ggtitle("frequency") +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type, width=0), lwd=1.5)
print(p)
# dp = bootsSummary(d[d$qtype == "price",], measurevar="response", groupvars=c("word_type", "word"))
# p = ggplot(data=dp, aes(x=word_type, y=response, fill=word_type)) +
#   geom_bar(stat="identity") +
#   facet_wrap(~ word) +
#   geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type, width=0), lwd=1.5)
# print(p)
dd = bootsSummary(d[d$qtype == "price" & d$word %in% c("truly", "madly"),], measurevar="diff", groupvars=c("word_type", "word"))
p = ggplot(data=dd, aes(x=word_type, y=diff, fill=word_type)) +
  geom_bar(stat="identity") +
  facet_grid( ~ word) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type, width=0), lwd=1.5)
print(p)

dd = bootsSummary(d[d$qtype == "price",], measurevar="price", groupvars=c("word_type", "word"))
p = ggplot(data=dd, aes(x=word_type, y=price, fill=word_type)) +
  geom_bar(stat="identity") +
  facet_grid( ~ word) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type, width=0), lwd=1.5)
print(p)

dd = bootsSummary(d[d$qtype == "price" & d$word %in% c("truly", "madly") & d$diff >= 0,],
                  measurevar="diff", groupvars=c("word_type", "word"))
p = ggplot(data=dd, aes(x=word_type, y=diff, fill=word_type)) +
  geom_bar(stat="identity") +
  facet_grid( ~ word) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=word_type, width=0), lwd=1.5)
print(p)

# p = ggplot(data=d[d$qtype == "price" & d$word %in% c("truly", "madly"),],
#            aes(x=word_type, y=diff, fill=word)) +
#   geom_bar(stat="identity") +
#   facet_wrap( ~ workerid)
# print(p)
fit = lmer(diff ~ word_type + (1 | word), data=d[d$word_type %in% c("control", "target"),])
print(summary(fit))

dfp = reshape(d[d$qtype %in% c("frequency", "price") & d$word_type %in% c("target", "control"),
                c("workerid", "word_type", "word", "response", "qtype")],
              direction="wide", timevar="qtype", idvar=c("workerid", "word_type", "word"))
dfp$response.frequency[dfp$response.frequency == 0] = 0.1
dfp$response.price[dfp$response.price == 0] = 0.1
p = ggplot(data=dfp, aes(x=-log(response.frequency), y=response.price))+
  geom_point() +
  theme_bw(18) +
  facet_wrap(~ word) +
  xlab("propto surprisal") +
  ylab("price") +
  theme(panel.grid=element_blank()) +
  geom_smooth(method="lm")
print(p)