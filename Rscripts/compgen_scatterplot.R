library(ggplot2)
library(googleVis)
library(RColorBrewer)
colours <- brewer.pal(8, "Set1")
colours[6] <- "#11dddd"

d <- read.delim('/Users/kgori/Desktop/ferret/data/FerretDataFrame.tsv')

mypl <- function(d, xlab="", ylab="", title="") {

  colours <- c("#3040F0", "#20CDB0", "#B0F020", "#F0B020")
  ferret_dist = d[[7]]
  mouse_dist = d[[8]]
  
  quantile = factor(quants(d, 4), labels=c("0-25%", "25-50%", "50-75%", "75-100%"))
  axlim = max(ferret_dist, mouse_dist)
  
  p <- ggplot(pam, aes(x=ferret_dist, y=mouse_dist, colour=quantile), environment=environment()) + 
    geom_point() + 
    xlim(0,axlim) + 
    ylim(0,axlim) +
    coord_fixed() +
    labs(title=title, x="vs Ferret  /PAM", y="vs Mouse  /PAM") +
    geom_abline(colour='red', linetype='dashed', size=0.5) +
    scale_colour_manual(values = colours) +
    theme_bw()
  return(p)
}

mypl45 <- function(d, q=1, xlab="", ylab="", title="") {
  ferret_dist = d[[5]]
  mouse_dist = d[[6]]
  quantile = cut(d[[11]], breaks=c(0,45,90), labels=c("lower","upper"))
  axlim = max(ferret_dist, mouse_dist)
  p <- ggplot(pam, aes(x=ferret_dist, y=mouse_dist, colour=quantile), environment=environment()) + 
    geom_point() + 
    xlim(0,axlim) + 
    ylim(0,axlim) +
    geom_abline(colour='red', linetype='dotted') 
  return(p)
}


quants <- function(df, q) {
	return(as.numeric(with(df, cut(df$ANGLE.DEG, breaks=quantile(df$ANGLE.DEG, probs=seq(0,1, by=1/q)), include.lowest=TRUE))))
}

magquants <- function(df, q) {
  return(as.numeric(with(df, cut(df$MAG, breaks=quantile(df$MAG, probs=seq(0,1, by=1/q)), include.lowest=TRUE))))
}

dists <- read.delim('/Users/kgori/Desktop/ferret/data/FerretDataFrame.tsv')
#Quartiles by angle (within method groups)
pam <- subset(dists, METHOD=='pam' & RANK!=15985)
gtr <- subset(dists, METHOD=='gtr' & RANK!=15985)

gtr$MAG <- sqrt((gtr$HUMAN.FERRET.DIST^2 + gtr$HUMAN.MOUSE.DIST^2 ))
pam$MAG <- sqrt((pam$HUMAN.FERRET.DIST^2 + pam$HUMAN.MOUSE.DIST^2 ))
gtr$QUANTS <- quants(gtr, 4)
pam$QUANTS <- quants(pam, 4)

#Quartiles by magnitude (within groups)
pam1 <- subset(dists, METHOD=='pam' & RANK!=15985 &QUANTS==1 )
pam2 <- subset(dists, METHOD=='pam' & RANK!=15985 &QUANTS==2 )
pam3 <- subset(dists, METHOD=='pam' & RANK!=15985 &QUANTS==3 )
pam4 <- subset(dists, METHOD=='pam' & RANK!=15985 &QUANTS==4 )
gtr1 <- subset(dists, METHOD=='gtr' & RANK!=15985 &QUANTS==1 )
gtr2 <- subset(dists, METHOD=='gtr' & RANK!=15985 &QUANTS==2 )
gtr3 <- subset(dists, METHOD=='gtr' & RANK!=15985 &QUANTS==3 )
gtr4 <- subset(dists, METHOD=='gtr' & RANK!=15985 &QUANTS==4 )
pam1$MAGQ <- magquants(pam1, 3)
pam2$MAGQ <- magquants(pam2, 3)
pam3$MAGQ <- magquants(pam3, 3)
pam4$MAGQ <- magquants(pam4, 3)
gtr1$MAGQ <- magquants(gtr1, 3)
gtr2$MAGQ <- magquants(gtr2, 3)
gtr3$MAGQ <- magquants(gtr3, 3)
gtr4$MAGQ <- magquants(gtr4, 3)
df <- rbind(pam1, pam2, pam3, pam4, gtr1, gtr2, gtr3, gtr4)

df <- as.data.frame(rbind(gtr[1:1000,],pam[1:1000,]))
#df$ID <- paste(df$GROUP, df$KEYWORD, sep="/")
#df$METHOD <- as.numeric(df$METHOD)
#df$SIZE <- rep.int(1, nrow(df))
#df <- as.data.frame(cbind(df$ID, df$METHOD, df$QUANTS, df$HUMAN.FERRET.DIST, df$HUMAN.MOUSE.DIST, df$SIZE))#id time colour x y size
#colnames(df) <- c("id", "time", "quartile", "HumanFerretDistance", "HumanMouseDistance", "size")
#df$time <- as.numeric(df$time)
#df$quartile <- factor(df$quartile, labels=c("0-25%", "25-50%", "50-75%", "75-100%"))
#df$HumanFerretDistance <- as.numeric(as.character(df$HumanFerretDistance))
#df$HumanMouseDistance <- as.numeric(as.character(df$HumanMouseDistance))
#M <- gvisMotionChart(df, xvar="HumanFerretDistance", yvar="HumanMouseDistance", colorvar="quartile", options=list(height=800, width=1000, showXScalePicker=FALSE, showYScalePicker=FALSE, state='{"iconType": "POINT", "xZoomedDataMax": 0.7, "yZoomedDataMax": 0.7}'))
