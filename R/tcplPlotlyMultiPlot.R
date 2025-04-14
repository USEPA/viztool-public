#' Function for plotting multiple plots at the same time
#'
#' @param dat a dataframe with each plot's associated parameters per row
#'
#' @return A plot or multiple plots in a single html widget
#' @export
#' @import dplyr
#' @importFrom manipulateWidget combineWidgets
tcplPlotlyMultiPlot <- function(dat) {
  #variable binding
  conc <- NULL
  plotList <- function(nplots) {
    lapply(seq_len(nrow(nplots)), function(x) tcplPlot::tcplPlotlyPlot(nplots %>% filter(row_number() == x)))
  }
  plot_dat <- dat %>% filter(!is.na(conc))
  if(nrow(plot_dat)>1){
  s2 <- plotList(plot_dat)
    return(manipulateWidget::combineWidgets(list = s2,width = '100%', height = paste0(length(s2)*400,"px"),ncol = 1))
  } else {
    return(tcplPlot::tcplPlotlyPlot(plot_dat))
  }

}
