#' Bioactivity assay annotations clustering matrix plot
#'
#' @param assays dataframe with assay annotations from output of tcplAssayAnnotationsLoad
#' @param xaxis Character, the x-axis field name from 'assays'
#' @param yaxis Character, the y-axis field name from 'assays'
#' @return Annotations plot
#' @export
#'
#' @importFrom ggplot2 ggplot scale_color_gradient theme element_text aes
#' @import dplyr
#' @importFrom ggiraph geom_point_interactive opts_selection girafe
tcplPlotAssays <- function(assays, xaxis = "intendedTargetFamily", yaxis = "organism") {
  dat <- NULL
  if (xaxis == yaxis) {
    dat <- assays |> dplyr::group_by(!!as.name(xaxis)) |> reframe(freq = n(), desc = paste(!!as.name(yaxis), "x", !!as.name(xaxis), "\n", freq, " assay endpoints:\n", paste(assayComponentEndpointName, collapse = "\n")))
  } else {
    dat <- assays |> dplyr::group_by(!!as.name(xaxis),!!as.name(yaxis)) |> reframe(freq = n(), desc = paste(!!as.name(yaxis), "x", !!as.name(xaxis), "\n", freq, " assay endpoints:\n", paste(assayComponentEndpointName, collapse = "\n")))
    dat[[2]] <- stringr::str_trunc(dat[[2]], 30, side = "right")
  }
  dat <- unique(dat)
  dat[[1]] <- stringr::str_trunc(dat[[1]], 30, side = "right")
  dat$desc <- lapply(dat$desc, function(text) {
    lines <- str_count(text, "\n")
    if (lines >= 20) {
      text <- paste(paste(str_split(text, "\n")[[1]][1:19], collapse = "\n"), "\n...")
    }
    text
  })
  nx <- dat |> select(!!as.name(xaxis)) |> unique() |> nrow()
  ny <- dat |> select(!!as.name(yaxis)) |> unique() |> nrow()
  gg_cluster <- ggplot(dat, aes(x = !!as.name(xaxis), y = !!as.name(yaxis))) +
    geom_point_interactive(aes(color = freq, tooltip = desc, data_id = desc),
                           hover_nearest = TRUE, size = ifelse(nx*ny > 1000, 3, 4)) +
    scale_color_gradient(name = "Endpoints", low=rgb(0.2,0.7,0.5,0.5), high = "black", trans = "log", breaks = c(1,2,4,8,16,32,64,128,256,512,1024), labels = c(1,2,4,8,16,32,64,128,256,512,1024)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = max(9-ifelse(nx>50, (nx-50) / 7, 0), 4)),
          axis.text.y = element_text(angle = 45, vjust = 0, hjust = 1, size = max(9-ifelse(ny>15, (ny-15) / 7, 0), 4)),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.title.y = element_text(face = "bold", size = 10))
  girafe(ggobj = gg_cluster,
         width_svg = 13,
         height_svg = 5,
         options = list(
           opts_selection(
             type = "multiple",
             only_shiny = FALSE)))
}
