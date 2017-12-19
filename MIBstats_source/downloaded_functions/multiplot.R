# _Adapted from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# _Multiple plot function
#
# _ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# _- cols:   Number of columns in layout
# _- layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# _If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# _then plot 1 will go in the upper left, 2 will go in the upper right, and
# _3 will go all the way across the bottom.
# _
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # _Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # _If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # _Make the panel
    # _ncol: Number of columns of plots
    # _nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  
  # _Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  
  # _Make each plot, in the correct location
  withProgress(message = 'Drawing plots', value = 0, {
    # _Number of times we'll go through the loop
    n <- numPlots
    
    for (i in 1:numPlots) {
      # _Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      incProgress(1/n, detail = paste("Doing part", i))
    }
    
  })
    
    
}