prep.data <- function(d) {
  # d = (some subset of) FerretDataFrame.tsv
  # build new data.frame with these columns:
  #   id, method, quartile, HumanFerretDistance, HumanMouseDistance, size
  # corresponding to motion plot requirements - id time colour x y size

  id <- paste(d$GENE.NAME, d$KEYWORD, sep="/")
  method <- as.numeric(d$METHOD)
  quartile <- factor(d$QUANTS)
  HumanFerretDistance <- as.numeric(as.character(d$HUMAN.FERRET.DIST))
  HumanMouseDistance <- as.numeric(as.character(d$HUMAN.MOUSE.DIST))
  size <- rep.int(1, nrow(d))
  data <- as.data.frame(cbind(id, method, quartile, HumanFerretDistance, HumanMouseDistance, size)) #id time colour x y size
  colnames(data) <- c("id", "time", "quartile", "HumanFerretDistance", "HumanMouseDistance", "size")
  return(data)
}

motion <- function(d) {
  # d = (some subset of) FerretDataFrame.tsv

  require(googleVis)
  data <- prep.data(d)

  M <- gvisMotionChart(df, 
    xvar="HumanFerretDistance", 
    yvar="HumanMouseDistance", 
    colorvar="quartile", 
    options=list(
      height=800, 
      width=1000, 
      showXScalePicker=FALSE, 
      showYScalePicker=FALSE, 
      state='{"iconType": "POINT", "xZoomedDataMax": 0.7, "yZoomedDataMax": 0.7}',
      ),
    )

  return(M)
}
