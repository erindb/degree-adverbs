library(ggplot2)
source("~/opt/r_helper_scripts/gg_themes.R")

toplot = function(utt, dist, vals) {
  values = c(0, 1, 0.2,  0.4, 0.6, 0.8)
  return(data.frame(
    values = values,
    utterance=rep(utt, length(values)),
    prior=rep(dist,  length(values)),
    weights = as.numeric(unlist(strsplit(vals, " ")))))
}

distributions = rbind(
  #"peaked mid is gaussian w/ mean 0.5 sd 0.2"
  toplot("prior", "peaked", "00.25000000 0.25000000 0.16666667 0.08333333 0.08333333 0.16666667"),
  toplot("no utterance (cost=0)", "peaked", "0.23449457159416062 0.23442455330353357 0.17756626621288885 0.09354849762316793 0.08244187646921869 0.1775242347970304"),
  toplot("rain (cost=0.1)", "peaked", "0 0.539851802509366 0.05050674476467876 0.05665069175287586 0.08573958961335719 0.2672511713597221"),
  toplot("not-rain (cost=0.1)", "peaked", "0.5210746336002561 0 0.2579317994666407 0.08952251817562867 0.08271818365245887 0.048752865105015794"),
  toplot("must (cost=100)", "peaked", "0 0.4824425613686876 0.06490365151095955 0.07233747288966211 0.09748610688496716 0.2828302073457236"),
  toplot("must-not (cost=100)", "peaked", "0.46476104567869475 0 0.27239051917703005 0.1064768103681654 0.09383526481172202 0.06253635996438765"),
  toplot("prior", "uniform", ".17 .17 .17 .17 .17 .17"),
  toplot("no utterance (cost=0)", "uniform", "0.1457504449318613 0.14213352327769566 0.176928845891194 0.19191970328259972 0.1696504149228148 0.17361706769383461"),
  toplot("rain (cost=0.1)", "uniform", "0 0.39553348954934286 0.044543714489661275 0.10710629435152468 0.1720291245240794 0.28078737708539175"),
  toplot("not-rain (cost=0.1)", "uniform", "0.37192372777130683 0 0.26171818943757896 0.16922698498669345 0.15506431321758102 0.04206678458683976"),
  toplot("must (cost=100)", "uniform", "0 0.40466653957257465 0.04412249854336343 0.10768706350974439 0.16570399950783124 0.2778198988664862"),
  toplot("must-not (cost=100)", "uniform", "0.39066231915127864 0 0.2586716566722589 0.16367658583440584 0.14468447077092905 0.04230496757112753"),
  toplot("prior", "more peaked", "0.3125 0.3125 0.1250 0.0625 0.0625 0.1250"),
  toplot("no utterance (cost=0)", "more peaked", "0.2991551527875135 0.29914734121625686 0.13429752615023774 0.07077178397170834 0.06233346779904022 0.1342947280752434"),
  toplot("rain (cost=0.1)", "more peaked", "0 0.6585993977198331 0.03874939005154424 0.042287801471423435 0.06348293263351545 0.1968804781236838"),
  toplot("not-rain (cost=0.1)", "more peaked", "0.6416910936970592 0 0.19182444016912645 0.06687908320630352 0.061850603846446826 0.037754779081063915"),
  toplot("must (cost=100)", "more peaked", "0 0.6003316606131978 0.053342140926831524 0.05650497306633434 0.07478491085240334 0.2150363145412331"),
  toplot("must-not (cost=100)", "more peaked", "0.5837592201381071 0 0.20909567824954364 0.08255920508224288 0.07271544198413285 0.051870454545973534")#,
#   toplot("prior", "peaked", "")#,
#   toplot("no utterance (cost=0)", "peaked", ""),
#   toplot("rain (cost=0.1)", "peaked", ""),
#   toplot("not-rain (cost=0.1)", "peaked", ""),
#   toplot("must (cost=100)", "peaked", ""),
#   toplot("must-not (cost=100)", "peaked", "")
)

p = ggplot(distributions, aes(x=values, y=weights)) +
  geom_line(stat="identity", aes(colour=factor(utterance))) +
  facet_wrap(~ prior) +
  ylab("") +
  scale_colour_manual(values=c("white", "yellow", "cyan", "magenta", "darkcyan", "purple")) +
  ggtitle("m-implicature resulting in a *less* extreme meaning") +
  xlab("heights") +
  theme_black() +
  xlim(0, 1)
print(p)
