library(ggplot2)
source("~/opt/r_helper_scripts/gg_themes.R")

toplot = function(utt, dist, vals) {
  if (utt == "no utterance" | utt=="prior" | utt == "no utterance (cost=0)") {
    values = c(0, 1, 0.2,  0.4, 0.6, 0.8)
  } else if (utt == "not-rain (cost=1)") {
    values = c(0, 0.2, 0.4, 0.6, 0.8)
  } else {
  values = c(1, 0.2, 0.4, 0.6, 0.8)
}
  return(data.frame(
    values = values,
    utterance=rep(utt, length(values)),
    prior=rep(dist,  length(values)),
    weights = as.numeric(unlist(strsplit(vals, " ")))))
}

# ((0 1 0.2 0.4 0.6 0.8) (0.020535786963305776 0.010427353067289103 0.1449385578656691 0.37545000191725547 0.34630999389569517 0.10233830629078541))
# ((1 0.2 0.4 0.6 0.8) (0.05963805238990981 0.04012789804630107 0.2184253694152116 0.39034649418183043 0.29146218596674717))
# undefined
# feste@miranda ~/everything-cocolab/degree-adverbs/model $ church theta.church 
# ((0 1 0.2 0.4 0.6 0.8) (0.012829974412362668 0.01221156932894735 0.11947148838828535 0.38383207537251834 0.35648656511053367 0.11516832738735258))
# ((1 0.2 0.4 0.6 0.8) (0.06461654819909826 0.0317050009399258 0.21154496383746424 0.3815002572138911 0.3106332298096206))
# ((0 0.2 0.4 0.6 0.8) (0.06019045056433935 0.281241396336394 0.32436156174644565 0.30402301886108873 0.030183572491732272))
# undefined



distributions = rbind(
  #"peaked mid is gaussian w/ mean 0.5 sd 0.2"
  toplot("prior", "peaked-mid", "0.00876415 0.00876415 0.06475880 0.17603266 0.17603266 0.06475880"),
  toplot("no utterance (cost=0)", "peaked-mid", "0.012829974412362668 0.01221156932894735 0.11947148838828535 0.38383207537251834 0.35648656511053367 0.11516832738735258"),
  toplot("rain (cost=1)", "peaked-mid", "0.06461654819909826 0.0317050009399258 0.21154496383746424 0.3815002572138911 0.3106332298096206"),
  toplot("not-rain (cost=1)", "peaked-mid", "0.06019045056433935 0.281241396336394 0.32436156174644565 0.30402301886108873 0.030183572491732272"),
  #"peaked mid is gaussian w/ mean 0.5 sd 0.2"
  toplot("prior", "peaked-mid-no-alt", "0.00876415 0.00876415 0.06475880 0.17603266 0.17603266 0.06475880"),
  toplot("no utterance (cost=0)", "peaked-mid-no-alt", "0.020535786963305776 0.010427353067289103 0.1449385578656691 0.37545000191725547 0.34630999389569517 0.10233830629078541"),
  toplot("rain (cost=1)", "peaked-mid-no-alt", "0.05963805238990981 0.04012789804630107 0.2184253694152116 0.39034649418183043 0.29146218596674717")
)

p = ggplot(distributions, aes(x=values, y=weights)) +
  geom_line(stat="identity", aes(colour=factor(utterance))) +
  facet_wrap(~ prior) +
  ylab("") +
  ggtitle("m-implicature resulting in a *less* extreme meaning") +
  xlab("heights") +
  xlim(0, 1)
print(p)
