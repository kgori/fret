mypl <- function(d, xlab="", ylab="", title="") {
  require(ggplot2)
  colours <- c(
  	"#1D2690", "#3040F0", "#838CF6", #blues
  	"#137b6a", "#20CDB0", "#79e1d0", 
  	"#6a9013", "#B0F020", "#d0f679", 
  	"#906a13", "#F0B020", "#f6d079")
  labels <- c( #placeholders
  	"a", "b", "c",
  	"d", "e", "f",
  	"g", "h", "i", 
  	"j", "k", "l")
  ferret_dist = d$HUMAN.FERRET.DIST
  mouse_dist = d$HUMAN.MOUSE.DIST
  
  groups = factor(paste(d$QUANTS, d$MAGQ), labels=labels)
  axlim = max(ferret_dist, mouse_dist)
  
  p <- ggplot(pam, aes(x=ferret_dist, y=mouse_dist, colour=groups), environment=environment()) + 
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
