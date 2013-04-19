mypl <- function(d, datatype="dna", xlab="", ylab="", title="") {
  require(ggplot2)
  colours <- c(
  	"#1D2690", "#3040F0", "#838CF6", #blues
  	"#137b6a", "#20CDB0", "#79e1d0", #cyans
  	"#6a9013", "#B0F020", "#d0f679", #greens
  	"#906a13", "#F0B020", "#f6d079") #oranges
  labels <- c( #placeholders
  	"0-25%, slowest third", "0-25%, middle third", "0-25%, fastest third",
  	"25-50%, slowest third", "25-50%, middle third", "25-50%, fastest third",
  	"50-75%, slowest third", "50-75%, middle third", "50-75%, fastest third", 
  	"75-100%, slowest third", "75-100%, middle third", "75-100%, fastest third")
  
  data <- subset(d, DATATYPE==datatype)
  ferret_dist <- data$HUMAN.FERRET.DIST
  mouse_dist <- data$HUMAN.MOUSE.DIST
  
  groups <- factor(paste(data$QUANTS, data$MAGQ), labels=labels)
  axlim <- max(ferret_dist, mouse_dist)
  
  p <- ggplot(data, aes(x=ferret_dist, y=mouse_dist, colour=groups), environment=environment()) + 
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
