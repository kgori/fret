# How the quantiles were calculated on the ferret dataframe

quants <- function(df, q) {
	return(as.numeric(with(df, cut(df$ANGLE.DEG, breaks=quantile(df$ANGLE.DEG, probs=seq(0,1, by=1/q)), include.lowest=TRUE))))
}

magquants <- function(df, q) {
  return(as.numeric(with(df, cut(df$MAG, breaks=quantile(df$MAG, probs=seq(0,1, by=1/q)), include.lowest=TRUE))))
}
