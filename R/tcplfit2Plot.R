#' Plotting for data related to tcplfit2 (Httr, HTPP)
#'
#' @param dat single row dataframe loaded from api for a single signature
#' @importFrom stringr str_split
#' @import dplyr
#' @import plotly
#' @import tcplfit2
#' @return plotly figure
#' @export
tcplfit2Plot <- function(dat){
  #variable binding
  response <- a <- bmd <- NULL

  conc <- as.numeric(stringr::str_split(dat %>% dplyr::pull(conc),"\\|")[[1]])
  resp <- as.numeric(stringr::str_split(dat %>% dplyr::pull(response),"\\|")[[1]])
  l3_dat <- dplyr::tibble(conc = unlist(conc), resp = unlist(resp))

  resolution <- 100
  x_min_max <- range(conc)
  x_range <- exp(seq(from = log(x_min_max[1]), to = log(x_min_max[2]), length.out = resolution))

  models <- dat$fitMethod
  dat <- dplyr::rename_with(dat,~gsub("^",paste0(models,"_"),.x),a:er)


  #cast model params as numeric
  dat <- dat %>% mutate_at(vars(starts_with(models[1])),as.numeric)

  # calculate y values for each function
  if ("hill" %in% models) y_hill <- tcplfit2::hillfn(ps = c(dat$hill_tp,dat$hill_ga,dat$hill_p), x = x_range)
  #tp = ps[1], ga = ps[2], p = ps[3], la = ps[4], q = ps[5]
  if ("gnls" %in% models) y_gnls <- tcplfit2::gnls(ps = c(dat$gnls_tp,dat$gnls_ga,dat$gnls_p,dat$gnls_la,dat$gnls_q),x = x_range)
  #a = ps[1], b = ps[2]
  if ("exp2" %in% models) y_exp2 <- tcplfit2::exp2(ps = c(dat$exp2_a,dat$exp2_b), x = x_range)
  #a = ps[1], b = ps[2], p = ps[3]
  if ("exp3" %in% models) y_exp3 <- tcplfit2::exp3(ps = c(dat$exp3_a,dat$exp3_b,dat$exp3_p), x = x_range)
  #tp = ps[1], ga = ps[2]
  if ("exp4" %in% models) y_exp4 <- tcplfit2::exp4(ps = c(dat$exp4_tp,dat$exp4_ga), x = x_range)
  #tp = ps[1], ga = ps[2], p = ps[3]
  if ("exp5" %in% models) y_exp5 <- tcplfit2::exp5(ps = c(dat$exp5_tp,dat$exp5_ga,dat$exp5_p), x = x_range)
  #a = ps[1]
  if ("poly1" %in% models) y_poly1 <- tcplfit2::poly1(ps = c(dat$poly1_a), x = x_range)
  #a = ps[1], b = ps[2]
  if ("poly2" %in% models) y_poly2 <- tcplfit2::poly2(ps = c(dat$poly2_a,dat$poly2_b), x = x_range)
  #a = ps[1], p = ps[2]
  if ("pow" %in% models) y_pow <- tcplfit2::pow(ps = c(dat$pow_a,dat$pow_p), x = x_range)


  # function for truncating decimals
  specify_decimal <- function(x, k) {
    if (!is.numeric(x)) {
      return(NA)
    }
    y <- lapply(x, function(x) {
      if (!is.na(x)) {
        if (abs(x) <= .005) {
          return(formatC(x, format = "e", digits = k))
        }
      }
      trimws(format(round(x, k), nsmall = k))
    })
    unlist(y)
  }


  # add error bar calc to conc/resp data
  er <- dat %>% pull(paste0(models,"_er"))
  l3_dat$bar <- abs(exp(er)*qt(.025,4)) * 2


  # start creation of actual plot
  fig <- plotly::plot_ly(
    data = l3_dat,
    x = ~conc,
    y = ~resp,
    type = "scatter",
    mode = "markers",
    name = "response",
    error_y = ~list(array = bar),
    hoverinfo = "text",
    text = ~ paste(
      # "</br> Assay Plate ID: ", apid,
      "</br> Concentration: ", specify_decimal(conc,2),
      "</br> Response: ", specify_decimal(resp,2)
    )
  )
  # formatting for y axis
  y <- list(
    title = "Response",
    range = c(-1,1)
    # set zeroline to false otherwise there would be a big horizontal line at y = 0
    #zeroline = FALSE
  )
  # formatting for x axis
  x <- list(
    title = "Concentration",
    # set zeroline to false so there is no vertical line at x = 0
    type = "log",
    zeroline = FALSE
  )

  # function to generate vertical line
  vline <- function(x = 0, color = "black") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      layer = "below",
      line = list(color = color, dash = "dash")
    )
  }

  # function to generate horizontal line
  hline <- function(y = 0, color = "black") {
    list(
      type = "line",
      y0 = y,
      y1 = y,
      x0 = 0,
      x1 = 1,
      xref = "paper",
      layer = "below",
      line = list(color = color, dash = "dash")
    )
  }

  dat$cutOff <- as.numeric(dat$cutOff)

  # # add cutoff annotation
  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = dat$cutOff),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "Cutoff",
    legendgroup = "Cutoff",
    line = list(dash = "dash", width = 1.5, color = "orange"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off (", specify_decimal(dat$cutOff,2), ")")
    )
  )

  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = dat$cutOff*-1),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "Cutoff",
    legendgroup = "Cutoff",
    showlegend = FALSE,
    line = list(dash = "dash", width = 1.5, color = "orange"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off (", specify_decimal(-1*dat$cutOff,2), ")")
    )
  )

  dat$bmr <- as.numeric(dat$bmr)


  # # add bmr annotation
  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = dat$bmr),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "BMR",
    legendgroup = "BMR",
    line = list(dash = "dash", width = 1.5, color = "black"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("BMR (", specify_decimal(dat$bmr,2), ")")
    )
  )

  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = dat$bmr*-1),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "BMR",
    legendgroup = "BMR",
    showlegend = FALSE,
    line = list(dash = "dash", width = 1.5, color = "black"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("BMR (", specify_decimal(-1*dat$bmr,2), ")")
    )
  )

  #make bmdl and bmdu numeric
  dat$bmdl <- as.numeric(dat$bmdl)
  dat$bmdu <- as.numeric(dat$bmdu)

  y_range <- seq(from = -1, to = 1, length.out = resolution)
  # add bmdl annotation
  fig <- fig %>% add_trace(
    data = tibble(x = dat$bmdl, y = y_range),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "BMDL",
    line = list(dash = "dash", width = 0.5, color = "black"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("BMDL (", specify_decimal(dat$bmdl,2), ")")
    )
  )


  # add bmdu annotation
  fig <- fig %>% add_trace(
    data = tibble(x = dat$bmd, y = y_range),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "BMD",
    line = list(dash = "dash", width = 1.5, color = "black"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("BMD (", specify_decimal(dat$bmd,2), ")")
    )
  )


  # add bmdu annotation
  fig <- fig %>% add_trace(
    data = tibble(x = dat$bmdu, y = y_range),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "BMDU",
    line = list(dash = "dash", width = 0.5, color = "black"),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("BMDU (", specify_decimal(dat$bmdu,2), ")")
    )
  )

  #create grey rectangle representing bmdl and bmdu error region
  bmdlu <- list(type = "rect",
       fillcolor = "lightgray", line = list(color = "lightgray"), opacity = 0.4,
       x0 = dat$bmdl, x1 = dat$bmdu, xref = "x",
       y0 = 0, y1 = 1, yref = "paper", layer = "below", linewidth = 0)

  if (models != "cnst") {
    #dat_lines <- vline(dat %>% pull(bmd) %>% as.numeric())
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y, shapes = list(bmdlu))
  } else {
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
  }

  # Check for low bmd value which may cause annotation overlap.
  low_bmd <- FALSE
  if (!is.na(dat %>% pull(bmd))) {
    if (!is.na(sort(unique(conc))[3])) {
      low_bmd <- (dat %>% pull(bmd)) < sort(unique(conc))[3]
    }
  }

  # add line for appropriate models (hitc=1)
  if (models != "cnst") {
    fig <- fig %>% add_annotations(
      yref = "paper",
      xref = "x",
      x = dat %>% pull(bmd) %>% as.numeric() %>% log10(),
      y = ifelse(low_bmd,0,1),
      text = paste0("Winning Model BMD (", specify_decimal(dat %>% pull(bmd) %>% as.numeric(),2), ")"),
      showarrow = F,
      textangle = 90,
      xanchor = ifelse(low_bmd,"right","left")
    )
  }

  m <- tibble(x = x_range,
              y = get(paste0("y_", dat$fitMethod)),
              model = dat$fitMethod,
              bmd = dat$bmd,
              top = dat$top,
              rmse = dat$rmse,
              top_over_cutoff = dat$topOverCutoff)

  # add line for winning model
  fig <- fig %>% add_trace(
    data = m,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    split = ~model,
    opacity = 1,
    line = list(width = 1.5, color = NULL),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", model,
      "</br> BMD: ", specify_decimal(bmd, 2),
      "</br> Concentration: ", specify_decimal(x,2),
      "</br> Response: ", specify_decimal(y, 2),
      "</br> RMSE: ", specify_decimal(rmse, 2),
      "</br> TOP: ", specify_decimal(top, 2),
      "</br> T/C: ", specify_decimal(top_over_cutoff, 2)
    )
  )



  # add annotations
  # Cell Type: string
  # Signature: string
  # Super Target: string
  # ACTIVE: numeric
  # Chemical Name (casn)
  # DTXSID
  # Sample ID
  # BMD
  fig <- fig %>% add_annotations(
    text = paste0(
      "Cell Type: ",dat %>% pull(.data$cellType), "<br>",
      "Signature: ",dat %>% pull(.data$signature), "<br>",
      "Super Target: ",dat %>% pull(.data$superTarget), "<br>",
      "Hit Call: ",dat %>% pull(.data$hitCall) %>% specify_decimal(2)," (ACTIVE)", "<br>",
      dat %>% pull(.data$chnm)," (",dat %>% pull(.data$casrn),")", "<br>",
      dat %>% pull(.data$dtxsid), "<br>",
      dat %>% pull(.data$sampleId), "<br>",
      "BMD: ",dat %>% pull(.data$bmd) %>% specify_decimal(2), "<br>"
      ),
    xref = "paper",
    x = 0.05,
    yref = "paper",
    y = .95,
    showarrow = FALSE,
    textposition = "bottom right",
    align = "left"
  )

  # return figure
  fig

}



#' Plotting for data related to tcplfit2 (HTPP)
#'
#' @param dat single row dataframe loaded from api for a single signature
#' @importFrom stringr str_split
#' @import dplyr
#' @import plotly
#' @import tcplfit2
#' @importFrom stats qt
#' @return plotly figure
#' @export
tcplfit2PlotHTPP <- function(dat){
  #variable binding
  bmd <- NULL
  conc <- as.numeric(stringr::str_split(dat %>% dplyr::pull(conc),"\\|")[[1]])
  resp <- as.numeric(stringr::str_split(dat %>% dplyr::pull(resp),"\\|")[[1]])
  l3_dat <- dplyr::tibble(conc = unlist(conc), resp = unlist(resp))

  resolution <- 100
  x_min_max <- range(conc)
  x_range <- exp(seq(from = log(x_min_max[1]), to = log(x_min_max[2]), length.out = resolution))

  models <- dat$fitMethod
  #specify actual column names so that order does not matter
  model_params <- c("a","er","tp","b","p","q","ga","la")
  for(a in model_params){
    dat <- dplyr::rename_with(dat,~gsub("^",paste0(models,"_"),.x),a)
  }



  #cast model params as numeric
  dat <- dat %>% mutate_at(vars(starts_with(models[1])),as.numeric)
  dat$bmd <- as.numeric(dat$bmd)
  dat$topOrig <- as.numeric(dat$topOrig)
  dat$topOverCutoff <- as.numeric(dat$topOverCutoff)
  dat$rmse <- as.numeric(dat$rmse)

  # calculate y values for each function
  if ("hill" %in% models) y_hill <- tcplfit2::hillfn(ps = c(dat$hill_tp,dat$hill_ga,dat$hill_p), x = x_range)
  #tp = ps[1], ga = ps[2], p = ps[3], la = ps[4], q = ps[5]
  if ("gnls" %in% models) y_gnls <- tcplfit2::gnls(ps = c(dat$gnls_tp,dat$gnls_ga,dat$gnls_p,dat$gnls_la,dat$gnls_q),x = x_range)
  #a = ps[1], b = ps[2]
  if ("exp2" %in% models) y_exp2 <- tcplfit2::exp2(ps = c(dat$exp2_a,dat$exp2_b), x = x_range)
  #a = ps[1], b = ps[2], p = ps[3]
  if ("exp3" %in% models) y_exp3 <- tcplfit2::exp3(ps = c(dat$exp3_a,dat$exp3_b,dat$exp3_p), x = x_range)
  #tp = ps[1], ga = ps[2]
  if ("exp4" %in% models) y_exp4 <- tcplfit2::exp4(ps = c(dat$exp4_tp,dat$exp4_ga), x = x_range)
  #tp = ps[1], ga = ps[2], p = ps[3]
  if ("exp5" %in% models) y_exp5 <- tcplfit2::exp5(ps = c(dat$exp5_tp,dat$exp5_ga,dat$exp5_p), x = x_range)
  #a = ps[1]
  if ("poly1" %in% models) y_poly1 <- tcplfit2::poly1(ps = c(dat$poly1_a), x = x_range)
  #a = ps[1], b = ps[2]
  if ("poly2" %in% models) y_poly2 <- tcplfit2::poly2(ps = c(dat$poly2_a,dat$poly2_b), x = x_range)
  #a = ps[1], p = ps[2]
  if ("pow" %in% models) y_pow <- tcplfit2::pow(ps = c(dat$pow_a,dat$pow_p), x = x_range)


  # function for truncating decimals
  specify_decimal <- function(x, k) {
    if (!is.numeric(x)) {
      return(NA)
    }
    y <- lapply(x, function(x) {
      if (!is.na(x)) {
        if (abs(x) <= .005) {
          return(formatC(x, format = "e", digits = k))
        }
      }
      trimws(format(round(x, k), nsmall = k))
    })
    unlist(y)
  }


  # add error bar calc to conc/resp data
  er <- dat %>% pull(paste0(models,"_er"))
  l3_dat$bar <- abs(exp(er)*qt(.025,4)) * 2


  # start creation of actual plot
  fig <- plotly::plot_ly(
    data = l3_dat,
    x = ~conc,
    y = ~resp,
    type = "scatter",
    mode = "markers",
    name = "response",
    #error_y = ~list(array = bar),
    hoverinfo = "text",
    text = ~ paste(
      # "</br> Assay Plate ID: ", apid,
      "</br> Concentration: ", specify_decimal(conc,2),
      "</br> Response: ", specify_decimal(resp,2)
    )
  )


  # Set Y range to a minimum of -5,5
  if(min(resp)>-5){
    min_y <- -5
  }else{
    min_y <- min(resp)
  }
  if(max(resp)<5){
    max_y <- 5
  }else{
    max_y <- max(resp)
  }
  yrange <- c(min_y=min_y,max_y=max_y)



  # formatting for y axis
  y <- list(
    title = "Response",
    range = yrange
    # set zeroline to false otherwise there would be a big horizontal line at y = 0
    #zeroline = FALSE
  )
  # formatting for x axis
  x <- list(
    title = "Concentration  (\u03BCM)",
    # set zeroline to false so there is no vertical line at x = 0
    type = "log",
    zeroline = FALSE,
    tickformat='.3'
  )

  dat$cutOff <- as.numeric(dat$cutOff) * sign(as.numeric(dat$topOrig))

  # # add cutoff annotation
  fig <- fig %>% plotly::add_trace(
    data = tibble(x = x_range, y = dat$cutOff),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "cutoff",
    line = list(dash = "solid", width = 1.5, color = NULL),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off (", specify_decimal(dat$cutOff,2), ")")
    )
  )

  dat$bmr <- as.numeric(dat$bmr) * sign(as.numeric(dat$topOrig))
  if(!is.na(dat$bmd)){
    # # add bmr annotation
    fig <- fig %>% plotly::add_trace(
      data = tibble(x = x_range, y = dat$bmr),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "BMR",
      line = list(dash = "dash", width = 1.5, color = "black"),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("BMR (", specify_decimal(dat$bmr,2), ")")
      )
    )
  }

  #make bmdl and bmdu numeric
  dat$bmdl <- as.numeric(dat$bmdl)
  if(!is.null(dat$bmdu)){
    dat$bmdu <- as.numeric(dat$bmdu)
  }

  y_range <- seq(from = yrange['min_y'], to = yrange['max_y'], length.out = resolution)
  if(!is.na(dat$bmd)){
    # add bmdl annotation
    fig <- fig %>% plotly::add_trace(
      data = tibble(x = dat$bmdl, y = y_range),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "BMDL",
      line = list(dash = "dash", width = 0.5, color = "black"),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("BMDL (", specify_decimal(dat$bmdl,2), ")")
      )
    )
  }

  if(!is.na(dat$bmd)){
    # add bmd annotation
    fig <- fig %>% plotly::add_trace(
      data = tibble(x = dat$bmd, y = y_range),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "BMD",
      line = list(dash = "dashdot", width = 1.5, color = "black"),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("BMD (", specify_decimal(dat$bmd,2), ")")
      )
    )
  }

  if(!is.na(dat$bmd)){
    # add bmdu annotation
    if(!is.null(dat$bmdu)){
      fig <- fig %>% plotly::add_trace(
        data = tibble(x = dat$bmdu, y = y_range),
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "lines",
        name = "BMDU",
        line = list(dash = "dash", width = 0.5, color = "black"),
        inherit = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>", paste0("BMDU (", specify_decimal(dat$bmdu,2), ")")
        )
      )
    }
  }

  #create grey rectangle representing bmdl and bmdu error region
  bmdlu <- list(type = "rect",
                fillcolor = "lightgray", line = list(color = "lightgray"), opacity = 0.4,
                x0 = dat$bmdl, x1 = dat$bmdu, xref = "x",
                y0 = 0, y1 = 1, yref = "paper", layer = "below", linewidth = 0)

  if ((models != "cnst") & (!is.na(dat$bmd))) {
    #dat_lines <- vline(dat %>% pull(bmd) %>% as.numeric())
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y, shapes = list(bmdlu))
  } else {
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
  }


  # add line for appropriate models (hitc=1)
  if (models != "cnst") {
    if(!is.na(dat$bmd)){
      fig <- fig %>% plotly::add_annotations(
        yref = "paper",
        xref = "x",
        x = dat %>% pull(bmd) %>% as.numeric() %>% log10(),
        y = 1,
        text = paste0("Winning Model BMD (", specify_decimal(dat %>% pull(bmd) %>% as.numeric(),2), ")"),
        showarrow = F,
        textangle = 90,
        xanchor = "left"
      )
    }
  }

  m <- tibble(x = x_range,
              y = get(paste0("y_", dat$fitMethod)),
              model = dat$fitMethod,
              bmd = dat$bmd,
              top = dat$topOrig,
              rmse = dat$rmse,
              top_over_cutoff = dat$topOverCutoff)

  # add line for winning model
  fig <- fig %>% plotly::add_trace(
    data = m,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    split = ~model,
    opacity = 1,
    line = list(width = 1.5, color = NULL),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", model,
      "</br> BMD: ", specify_decimal(bmd, 2),
      "</br> Concentration: ", specify_decimal(x,2),
      "</br> Response: ", specify_decimal(y, 2),
      "</br> RMSE: ", specify_decimal(rmse, 2),
      "</br> TOP: ", specify_decimal(top, 2),
      "</br> T/C: ", specify_decimal(top_over_cutoff, 2)
    )
  )



  # add annotations
  # 1. Dataset
  # 2. CellType
  # 3. Exposure Duration
  # 4. Cell Density
  # 5. Feature/Category
  # 6. SampleId
  # ACTIVE: numeric
  # Chemical Name (casn)
  # DTXSID
  # Sample ID
  # BMD
  fig <- fig %>% plotly::add_annotations(
    text = paste0(
      ifelse(is.na(dat$bmd),"INACTIVE",paste0("ACTIVE: ",dat %>% pull(.data$hitCall) %>% as.numeric() %>% specify_decimal(2))), "<br>",
      "Dataset: ",dat %>% pull(.data$dataset), "<br>",
      "Cell Type: ",dat %>% pull(.data$cellType), "<br>",
      "Exposure Duration: ",dat %>% pull(.data$exposureDuration), "<br>",
      "Cell Density: ",dat %>% pull(.data$seedingDensity), "<br>",
      dat %>% pull(.data$chnm)," (",dat %>% pull(.data$casrn),")", "<br>",
      dat %>% pull(.data$dtxsid), "<br>",
      dat %>% pull(.data$sampleId), "<br>",
      "BMD: ",dat %>% pull(.data$bmd) %>% specify_decimal(2), "<br>"
    ),
    xref = "paper",
    x = 0.05,
    yref = "paper",
    y = .95,
    showarrow = FALSE,
    textposition = "bottom right",
    align = "left"
  )

  # return figure
  fig

}

