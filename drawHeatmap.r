library(lattice)
library(ggplot2)

theme_set(theme_gray(base_size = 18))

split <- function(y, n)
{
  yn = t(apply(y, 1, function(x) tapply(x, ceiling(seq_along(x) / n), sum)))
  yn = t(yn)
  yn = t(apply(yn, 1, function(x) tapply(x, ceiling(seq_along(x) / n), sum)))
  yn = t(yn)
  return(yn)
}

loadData <- function(fname, separator)
{
  df <- read.csv(fname, sep=separator)
  return(df)
}

toMatrix <- function(df, m, n) {
  nMatrix = as.matrix(df)
  dim(nMatrix) = c(m,n)
  return(nMatrix)
}

touches <- function(fname, separator="\n", m=1280, n=720, k=15)
{
  d = loadData(fname, separator)
  d = toMatrix(d, m, n)
  d = split(d, k)
  return(d)
}

drawHeatmap <- function(fname,label)
{
  rgb.palette <- colorRampPalette(c("darkgreen","yellow", "red"), space = "rgb")
  
  d = touches(fname)
  l = levelplot(d, xlab="X", ylab="Y", scales=list(draw=FALSE), col.regions=rgb.palette(100), main=label)
  return(l)
}
