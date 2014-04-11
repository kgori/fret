#!/usr/bin/env Rscript

# Code to make a scatterplot with a specific set of genes
# highlighted. Thanks to Nives for the highlighting code.
#
# Prerequisites: optparse, ggplot2
#                (run install.packages(c("optparse", "ggplot2")))
#
# Usage: scatter_highlight.R --dataframe <dataframe.tsv>
#            --genelist <genelist.txt> (--seqtype [dna|protein])
#            --color (e.g. "red", "blue", "#CC4530"...)
#            --title
#           [--help]


###################################
### Function and global definitions
colours <- c(
    "#1D2690", "#3040F0", "#838CF6", #blues
    "#137b6a", "#20CDB0", "#79e1d0", #cyans
    "#6a9013", "#B0F020", "#d0f679", #greens
    "#906a13", "#F0B020", "#f6d079") #oranges

scatter <- function(d, datatype="dna", xlab="vs Ferret  /PAM", ylab="vs Mouse  /PAM", title="", abline.slope=1) {
  # Returns a scatterplot, to be plotted using R's built-in plot() function,
  # or saved using ggplot2's ggsave('filename.pdf', useDingbats=False, ...)

  # Arguments: d - the ferret data frame, or some subset of it. Load with d <- read.delim(path.to.data.file)
  #            datatype - "dna" or "protein", depending on which data to plot
  #            xlab, ylab, title - axis labels and plot title

  require(ggplot2)

  labels <- c( #placeholders
    "0-25%, slowest third", "0-25%, middle third", "0-25%, fastest third",
    "25-50%, slowest third", "25-50%, middle third", "25-50%, fastest third",
    "50-75%, slowest third", "50-75%, middle third", "50-75%, fastest third",
    "75-100%, slowest third", "75-100%, middle third", "75-100%, fastest third")

  data <- subset(d, DATATYPE==datatype) # only dna OR protein on one plot
  ferret_dist <- data$HUMAN.FERRET.DIST
  mouse_dist <- data$HUMAN.MOUSE.DIST

  groups <- factor(paste(data$QUANTS, data$MAGQ), labels=labels)
  axlim <- max(ferret_dist, mouse_dist)

  p <- ggplot(data, aes(x=ferret_dist, y=mouse_dist, colour=groups), environment=environment()) +
    geom_point() +
    xlim(0,axlim) +
    ylim(0,axlim) +
    coord_fixed() +
    labs(title=title, x=xlab, y=ylab) +
    geom_abline(colour='red', linetype='dashed', size=0.5, slope=abline.slope) +
    #geom_abline(colour='grey', linetype='dashed', size=0.5, slope=1) +
    scale_colour_manual(values = colours) +
    geom_rug(col=rgb(0.35,0.35,0.35,alpha=0.2)) +
    theme_bw()
  return(p)
}
### END DEFS
############

suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("ggplot2"))

##############################
# Handle commandline arguments
option_list <- list(
  make_option(c("-d", "--dataframe"), type="character",
    help="Path to dataframe"),
  make_option(c("-g", "--genelist"), type="character", default=NULL,
    help="Path to gene list file (OPTIONAL)"),
  make_option(c("-s", "--seqtype"), type="character", default="protein",
    help="Set the datatype = [dna | protein] (DEFAULT: PROTEIN)"),
  make_option(c("-c", "--color"), type="character", default="red",
    help="Set the highlighting colour (DEFAULT: RED)"),
  make_option(c("-t", "--title"), type="character", default="",
    help="Set the plot title (DEFAULT: BLANK)"),
  make_option(c("-o", "--outfile"), type="character", default="scatter.pdf",
    help="Outfile to save to (can be pdf, png, jpg, etc...) (DEFAULT: scatter.pdf)")
)
opt <- parse_args(OptionParser(option_list=option_list))
### END ARGS
############

### read files
cat("loading files, takes a few seconds...", file=stdout())
data.frame <- read.delim((opt$dataframe))
if (!is.null(opt$genelist)) {
  gene.list <- scan((opt$genelist), what="character", quiet=TRUE)
}
cat("\r\x1b[KDone.", file=stdout()) #\r\x1b[K = carriage return + clear line

### set plotting params
seq.type <- (opt$seqtype)
highlight.color <- (opt$color)
plot.title <- (opt$title)
outfile <- (opt$outfile)

### get relevant data
background.set <- subset(data.frame, DATATYPE==seq.type)
if (!is.null(opt$genelist)) {
  highlight.set <- background.set[background.set$ENSEMBL.GENE.ID %in% gene.list, ]
}

### do plot and save
my.plot <- scatter(background.set, datatype=seq.type, title=plot.title)
if (!is.null(opt$genelist)) {
  my.plot <- my.plot + geom_point(data=highlight.set, aes(HUMAN.FERRET.DIST,HUMAN.MOUSE.DIST),
    colour=highlight.color)
}
cat("\r\x1b[K", file=stdout())
ggsave(outfile)
