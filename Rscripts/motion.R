prep.data <- function(data, datatype, quartile, magnitude) {
  # d = (some subset of) FerretDataFrame.tsv
  # build new data.frame with these columns:
  #   id, method, quartile, HumanFerretDistance, HumanMouseDistance, size
  # corresponding to motion plot requirements - id time colour x y size

  d <- subset(data, DATATYPE==datatype & QUANTS==quartile & MAGQ==magnitude)
  # id <- paste(d$GENE.NAME, d$KEYWORD, sep="/")
  id <- d$GENE.NAME
  method <- as.numeric(d$METHOD)
  quartile <- factor(d$QUANTS)
  HumanFerretDistance <- as.numeric(as.character(d$HUMAN.FERRET.DIST))
  HumanMouseDistance <- as.numeric(as.character(d$HUMAN.MOUSE.DIST))
  # size <- rep.int(1, nrow(d))
  size <- rep(0.1, nrow(d))
  dataframe <- data.frame(id, method, quartile, HumanFerretDistance, HumanMouseDistance, size) #id time colour x y size
  colnames(dataframe) <- c("id", "time", "quartile", "HumanFerretDistance", "HumanMouseDistance", "size")
  return(dataframe)
}

motion <- function(data) {
  # d = (some subset of) FerretDataFrame.tsv

  require(googleVis)

  M <- gvisMotionChart(data, 
    xvar="HumanFerretDistance", 
    yvar="HumanMouseDistance", 
    colorvar="quartile", 
    options=list(height=800, width=1000, showXScalePicker=FALSE, showYScalePicker=FALSE, state='{"iconType": "POINT", "xZoomedDataMax": 0.7, "yZoomedDataMax": 0.7}')
    )

  return(M)
}

bubble <- function(data, bubbleSize=15) {

  require(googleVis)

  M <- gvisBubbleChart(data, 
    xvar="HumanFerretDistance", 
    yvar="HumanMouseDistance", 
    sizevar="size",
    options=list(height=800, 
      width=1000, 
      showXScalePicker=FALSE, 
      showYScalePicker=FALSE, 
      state='{"iconType": "POINT", "xZoomedDataMax": 0.7, "yZoomedDataMax": 0.7}',
      sizeAxis=paste("{minvalue: 0, maxSize: ", bubbleSize,"}", sep=""),
      colorAxis="{colors: ['green', '#f6d079'], legend: {position: 'none'}}",
      bubble="{textStyle: {color: 'none'}}", sep="")
    )

  return(M)  
}