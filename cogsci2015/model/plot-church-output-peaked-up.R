library(ggplot2)
source("~/opt/r_helper_scripts/gg_themes.R")

toplot = function(utt, dist, vals) {
  if (utt == "no utterance" | utt=="prior" | utt == "no utterance (cost=0)") {
    values = c(0, 1, 0.2,  0.4, 0.6, 0.8)
  } else {
    values = c(1, 0.2, 0.4, 0.6, 0.8)
  }
  return(data.frame(
    values = values,
    utterance=rep(utt, length(values)),
    prior=rep(dist,  length(values)),
    weights = as.numeric(unlist(strsplit(vals, " ")))))
}

distributions = rbind(
  #"peaked mid is gaussian w/ mean 0.5 sd 0.2"
  toplot("prior", "peaked both", "0.04761905 0.28571429 0.09523810 0.14285714 0.19047619 0.23809524"),
  toplot("no utterance (cost=0)", "peaked both", "0.07386140429880274 0.20405473151835773 0.13217405069600632 0.17614072421442842 0.20669121582042552 0.20707787345197912"),
  toplot("feppy (cost=0.1)", "peaked both", "0.4414817063534034 0.026794957189326035 0.079456950006118 0.15751764844889923 0.2947487380022533"),
  toplot("very feppy (cost=2)", "peaked both", "0.3896326761261084 0.03649400723661538 0.09998207420124992 0.18166887547873756 0.29222236695728876"),
  toplot("extremely feppy (cost=100)", "peaked both", "0.38046458311242654 0.03928377003249322 0.10484009910880437 0.18587179895955402 0.28953974878672195")
)

p = ggplot(distributions, aes(x=values, y=weights)) +
  geom_line(stat="identity", aes(colour=factor(utterance))) +
  #facet_wrap(~ prior) +
  ylab("") +
  xlab("heights") +
  theme_blackDisplay() +
  xlim(0, 1)
print(p)
