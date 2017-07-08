report_coef = function(m, regressor) {
  coefs = summary(m)$coefficients[regressor,]
  b = coefs[["Estimate"]]
  df = coefs[["df"]]
  t = coefs[["t value"]]
  p = coefs[["Pr(>|t|)"]]
  return(paste(
    "$b=",
    round(b, 3),
    ", t(",
    # "df=",
    round(df, 1),
    ")=",
    round(t, 3),
    ", ",
    ifelse(0==round(p, 4), "p<0.0005", paste("p=", round(p, 4), sep="")),
    # ifelse(p<0.001, "p<0.001" "p<0.05", paste("p=", round(p, 3), sep="")),
    "$",
    sep=""))
}

report_chisq = function(df, chisq, p) {
  return(paste(
    "$\\chi^2(",
    # "df=",
    df,
    ")=",
    round(chisq, 3),
    ", ",
    ifelse(0==round(p, 4), "p<0.0005", paste("p=", round(p, 4), sep="")),
    "$",
    sep=""))
}