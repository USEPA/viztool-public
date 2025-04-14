#' tcplPlotlyPlot a generic plotly plotting function that accepts JSON as input
#'
#' @param dat dataframe of necessary parameters for plotting
#'
#' @return generic plotly plot of given JSON parameters
#' @export
#'
#' @import dplyr
#' @import plotly
#' @import tidyr
#' @import stringr
#' @importFrom rlang .data
#' @importFrom stringr str_to_title
tcplPlotlyPlot <- function(dat) {
  #variable binding
  model_stats <- model <- param <- value <- ac50 <- hitc <- name <-  NULL

  l3_dat <- tibble(conc = 10^as.numeric(unlist(dat$conc)), resp = unlist(dat$resp))

  # extract range from level 3 data for creating plotting all the functions
  # increase resolution to get smoother curves
  resolution <- 100
  x_min_max <- range(l3_dat$conc)
  x_range <- exp(seq(from = log(x_min_max[1]), to = log(x_min_max[2]), length.out = resolution))

  #check if winning model = none
  if (!dat$modl == "none"){

    #check if winning model has negative top.  If so coff should be negative
    if(!is.null(dat$mc5Param[[1]]$hit_param_top) && !is.null(dat$coff) && !is.na(dat$mc5Param[[1]]$hit_param_top)){
      if(dat$mc5Param[[1]]$hit_param_top < 0){
        dat$coff <- dat$coff*-1
      }
    }

    #get models from columns that have an ac50 listed
    params <- dat %>% select(contains("mc4param"))  %>% tidyr::unnest_wider(col = everything())
    ac50s <- params %>% select(contains("ac50"),-contains("loss")) %>% tidyr::pivot_longer(cols = everything(),values_to = "ac50") %>% mutate(model = stringr::str_extract(name,"[[:alnum:]]+"))
    models <- ac50s %>% pull(model)
    dat <- cbind(dat,params)

    # dat$models <- NULL
    # dat$ac50 <- NULL
    # l4_dat <- as_tibble(dat[3:length(dat)])


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

    if (dat$fitc == 100) {
      # loec is stored as modl_acc
      x_loec <- rep(dat$modl_acc, resolution)
      l3_resp <- l3_dat %>%
        pull(.data$resp) %>%
        range()
      y_loec <- seq(from = l3_resp[1], to = l3_resp[2], length.out = resolution)
    }

    # for model type 0 (default) add constant model
    y_cnst <- x_range * 0
    ac50s <- ac50s %>% rbind(c(model = "cnst", ac50 = NA))
    models <- c(models, "cnst")


    model_stats <- dat %>%
      select(ends_with("aic"), ends_with("rme"), ends_with("_top"), ends_with("_p")) %>%
      tidyr::pivot_longer(everything(),
                          names_to = c("model", "param"),
                          names_pattern = "(.*)_(.*)"
      ) %>%
      tidyr::pivot_wider(names_from = param, values_from = value)
    ac50s$ac50 <- as.numeric(ac50s$ac50)

    # set background opacity
    op <- .2
    opacity <- tibble(model = models, opacity = op) %>% mutate(opacity = ifelse(.data$model == dat$modl, 1, opacity))
    line.fmt <- tibble(model = models, dash = "dash") %>% mutate(dash = ifelse(.data$model == dat$modl, "solid", .data$dash))

    # build data table for plotly
    m <- opacity %>%
      inner_join(line.fmt, by = "model") %>%
      inner_join(ac50s, by = "model") %>%
      rowwise() %>%
      mutate(x = ifelse(dat$fitc == 100,list(x_loec),list(x_range)), y = list(get(paste0("y_", .data$model)))) %>%
      tidyr::unnest(cols = c(x, y))

    # if we have model stats we want them included in the hoverover
    if (!is.null(model_stats)) {
      m <- m %>% inner_join(model_stats, by = "model")
    }
  }

  # function for truncating decimals
  specify_decimal <- function(x, k) {
    if (!is.numeric(x)) {
      return(NA)
    }
    trimws(format(round(x, k), nsmall = k))
  }

  # start creation of actual plot
  fig <- plot_ly(
    data = l3_dat,
    x = ~conc,
    y = ~resp,
    type = "scatter",
    mode = "markers",
    name = "response",
    hoverinfo = "text",
    text = ~ paste(
      # "</br> Assay Plate ID: ", apid,
      "</br> Concentration: ", specify_decimal(conc,2),
      "</br> Response: ", specify_decimal(resp,2)
    )
  )
  # formatting for y axis
  y <- list(
    title = stringr::str_to_title(gsub("_"," ",dat$normalized_data_type)),
    # set zeroline to false otherwise there would be a big horizontal line at y = 0
    zeroline = FALSE
  )
  # formatting for x axis
  if(is.null(dat$conc_unit)){
  dat$conc_unit <- "\u03BCM"
  }
  x <- list(
    title = paste0("Concentration ","(",dat$conc_unit,")"),
    # set zeroline to false so there is no vertical line at x = 0
    type = "log",
    zeroline = FALSE,
    dtick=1
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

  # # add cutoff annotation
  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = dat$coff),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "cutoff",
    line = list(dash = "dash", width = 1.5, color = NULL),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off (", specify_decimal(dat$coff,2), ")")
    )
  )

  # currently only support for model types 1 and 0 but need to expand or make this generic
  if (dat$fitc == 100) {
    # apply axis and lines to figure
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)

    # add the loec line if hitc == 1.
    fig <- fig %>% add_trace(
      data = tibble(x = x_loec, y = y_loec),
      x = ~x,
      y = ~y,
      name = "LOEC",
      type = "scatter",
      mode = "lines",
      line = list(dash = "solid", width = 1.5, color = NULL),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", "LOEC",
        "</br> Log Concentration: ", x
      )
    )
  } else {
    if (!dat$modl == "cnst" && !dat$modl == "none") {
      dat_lines <- vline(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric())
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y, shapes = dat_lines)
    } else {
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
    }


    # add ac50 line for appropriate models (hitc=1)
    if (!dat$modl == "cnst" && !dat$modl == "none") {
      fig <- fig %>% add_annotations(
        yref = "paper",
        xref = "x",
        x = ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric() %>% log10(),
        y = 1,
        text = paste0("Winning Model AC50 (", specify_decimal(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric(),2), ")"),
        showarrow = F,
        textangle = 90,
        xanchor = "left"
      )
    }

    if (!dat$modl == "none"){
      # add all non-winning models
      fig <- fig %>% add_trace(
        data = m %>% filter(.data$model != dat$modl),
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "lines",
        split = ~model,
        opacity = ~opacity,
        line = list(dash = ~dash, width = 1.5, color = NULL),
        inherit = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>", model,
          "</br> ac50: ", specify_decimal(ac50, 2),
          "</br> Concentration: ", specify_decimal(x,2),
          "</br> Response: ", specify_decimal(y, 2),
          "</br> AIC: ", specify_decimal(aic, 2),
          "</br> RME: ", specify_decimal(rme, 2),
          "</br> TOP: ", specify_decimal(top, 2),
          "</br> SLOPE: ", specify_decimal(p, 2)
        )
      )

      # add line for winning model
      fig <- fig %>% add_trace(
        data = m %>% filter(.data$model == dat$modl),
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "lines",
        split = ~model,
        opacity = ~opacity,
        line = list(dash = ~dash, width = 1.5, color = NULL),
        inherit = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>", model,
          "</br> ac50: ", specify_decimal(ac50, 2),
          "</br> Concentration: ", specify_decimal(x,2),
          "</br> Response: ", specify_decimal(y, 2),
          "</br> AIC: ", specify_decimal(aic, 2),
          "</br> RME: ", specify_decimal(rme, 2),
          "</br> TOP: ", specify_decimal(top, 2),
          "</br> SLOPE: ", specify_decimal(p, 2)
        )
      )
    }
    # get hitcall
    hitcall <- dat %>% pull(hitc)

    # add annotations
    fig <- fig %>% add_annotations(
      text = paste0(
        dat %>% pull(.data$aenm), "<br>",
        case_when(
          #updated binary hitcall designation to three decimal rounding
          #hitcall == 1 ~ "ACTIVE",
          #hitcall == 0 ~ "INACTIVE",
          #hitcall == -1 ~ "NO CALL",
          TRUE ~ paste0("HITC: ", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3))))
        ), "<br>",
        dat %>% pull(.data$chnm), " (", dat %>% pull(.data$casn), ")", "<br>",
        dat %>% pull(.data$dsstox_id), "<br>",
        dat %>% pull(.data$spid), "<br>",
        ifelse(!is.null(dat$flag), gsub("\\|\\|", "<br>", paste0("Flags: ", dat %>% pull(.data$flag))), "")
      ),
      xref = "paper",
      x = 0.05,
      yref = "paper",
      y = .95,
      showarrow = FALSE,
      textposition = "bottom right",
      align = "left"
    )
  }
  # return figure
  fig
}
