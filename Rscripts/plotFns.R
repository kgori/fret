this.dir <- dirname(parent.frame(2)$ofile)
parent.dir <- dirname(this.dir)
data.file <- file.path(parent.dir, 'data', 'FerretDataFrame.tsv')

colours <- c(
    "#1D2690", "#3040F0", "#838CF6", #blues
    "#137b6a", "#20CDB0", "#79e1d0", #cyans
    "#6a9013", "#B0F020", "#d0f679", #greens
    "#906a13", "#F0B020", "#f6d079") #oranges

load.data <- function() {
  # Call `d <- load.data()` to assign FerretDataFrame.tsv to d
  # Alternatively, call `d <- read.delim(path.to.data.file)`
  d <- read.delim(data.file)
  return(d)
}

prep.data <- function(data, datatype, quartile, magnitude) {
  # d = (some subset of) FerretDataFrame.tsv
  # build new data.frame with these columns:
  #   id, method, quartile, HumanFerretDistance, HumanMouseDistance, size
  # corresponding to motion plot requirements - id time colour x y size

  d <- subset(data, DATATYPE==datatype & QUANTS==quartile & MAGQ==magnitude)
  id <- d$GENE.NAME
  method <- as.numeric(d$METHOD)
  quartile <- factor(d$QUANTS)
  HumanFerretDistance <- as.numeric(as.character(d$HUMAN.FERRET.DIST))
  HumanMouseDistance <- as.numeric(as.character(d$HUMAN.MOUSE.DIST))
  # size <- rep.int(1, nrow(d))
  size <- rep(1, nrow(d))
  dataframe <- data.frame(id, method, quartile, HumanFerretDistance, HumanMouseDistance, size) #id time colour x y size
  colnames(dataframe) <- c("id", "time", "quartile", "HumanFerretDistance", "HumanMouseDistance", "size")
  return(dataframe)
}

prep.bubble <- function(data, datatype) {
  d <- subset(data, DATATYPE==datatype)
  id <- d$GENE.NAME
  colour <- as.numeric((3*(d$QUANTS-1))+d$MAGQ)
  size <- as.numeric(d$LUNG.ENRICHMENT)
  HumanFerretDistance <- as.numeric(as.character(d$HUMAN.FERRET.DIST))
  HumanMouseDistance <- as.numeric(as.character(d$HUMAN.MOUSE.DIST))
  dataframe <- data.frame(id, HumanFerretDistance, HumanMouseDistance, colour) #id time colour x y size
  colnames(dataframe) <- c("id", "HumanFerretDistance", "HumanMouseDistance", "colour")
  return(dataframe)
}

bubble2 <- function(data, datatype, title="", bubbleSize=5) {
  require(googleVis)
  data.subset <- prep.bubble(data, datatype)
  data.subset <- data.subset[order(data.subset$colour), ]
  colours.JSON <- "['#1D2690', '#3040F0', '#838CF6', '#137b6a', '#20CDB0', '#79e1d0', '#6a9013', '#B0F020', '#d0f679', '#906a13', '#F0B020', '#f6d079']"
  max.value <- max(data.subset$HumanFerretDistance, data.subset$HumanMouseDistance)
  x.title <- "'Human-Ferret distance /PAM'"
  y.title <- "'Human-Mouse distance /PAM'"

  M <- gvisBubbleChart(data.subset,
    xvar="HumanFerretDistance", 
    yvar="HumanMouseDistance",
    colorvar="colour",
    options=list(height=1000, 
      width=1000, 
      sizeAxis=paste("{minvalue: 1, minSize: 1, maxSize: ", bubbleSize,"}", sep=""),
      colors=colours.JSON,
      legend="{position: 'none'}",
      bubble="{textStyle: {color: 'none'}, stroke: 'none'}", sep="",
      hAxis=paste("{maxValue: ", max.value,", title: ", x.title, "}", sep=""),
      vAxis=paste("{maxValue: ", max.value,", title: ", y.title, "}", sep=""),
      title=title,
      titleTextStyle="{fontSize: 24}",
      fontName="Helvetica")
    )

  return(M)
}

motion <- function(data) {
  # d = (some subset of) FerretDataFrame.tsv

  require(googleVis)

  M <- gvisMotionChart(data, 
    xvar="HumanFerretDistance", 
    yvar="HumanMouseDistance", 
    colorvar="quartile", 
    options=list(height=800, 
      width=1000, 
      showXScalePicker=FALSE, 
      showYScalePicker=FALSE, 
      state='{"iconType": "POINT", "xZoomedDataMax": 0.7, "yZoomedDataMax": 0.7}')
    )

  return(M)
}

bubble <- function(data, datatype, quartile, magnitude, bubbleSize=15) {

  require(googleVis)

  data.subset <- prep.data(data, datatype, quartile, magnitude)
  colour <- colours[ (3*(quartile-1) + magnitude) ]


  M <- gvisBubbleChart(data.subset, 
    xvar="HumanFerretDistance", 
    yvar="HumanMouseDistance", 
    sizevar="size",
    options=list(height=800, 
      width=1000, 
      showXScalePicker=FALSE, 
      showYScalePicker=FALSE, 
      state='{"iconType": "POINT", "xZoomedDataMax": 0.7, "yZoomedDataMax": 0.7}',
      sizeAxis=paste("{minvalue: 0, maxSize: ", bubbleSize,"}", sep=""),
      colorAxis=paste("{colors: ['green', '", colour, "'], legend: {position: 'none'}}", sep=""),
      bubble="{textStyle: {color: 'none'}}", sep="")
    )

  return(M)  
}


scatter <- function(d, datatype="dna", xlab="", ylab="", title="") {
  require(ggplot2)

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
    geom_rug(col=rgb(0.35,0.35,0.35,alpha=0.2)) +
    theme_bw()
  return(p)
}
