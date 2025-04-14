#' Summary plot for bioactivity (invitrodb) data
#'
#' @param dat dataframe with plotting parameters from output of tcplApiQuery
#'
#' @return Summary level plot
#' @export
#'
#' @import plotly
tcplPlotSummary <- function(dat = tcplPlot::tcplSummaryPlotExampleData) {

  m5 <- dat
  # remove inactives or na values from plotting
  m5 <- m5[!is.na(m5$scaledTop)]
  cytotox <- min(as.numeric(m5$cytotoxLowBndUm))
  cytotox_med <- min(as.numeric(m5$cytotoxMednUm))
  url_dat <- paste0("https://s3.amazonaws.com/dmap-ccte-ccd/current/bioassay_assay_description/AEID", m5$aeid, ".pdf")
  url_dat[!grepl(paste0("AEID",tcplPlot::available_assay_descriptions,collapse = ".pdf|"),url_dat)] <- ""
  fig <- plot_ly(
    data = m5,
    type = "scatter",
    mode = "markers",
    x = ~ac50,
    y = ~scaledTop,
    color = ~intendedTargetFamily,
    customdata = url_dat,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", "<b>AC50 (uM): </b>", specify_decimal(ac50, 3),
      #"</br>","<b>Scaled top: </b>", specify_decimal(scaled_top,3),
      "</br>", "<b>Assay Endpoint Name: </b>", endpointName,
      "</br>", "<b>Gene Symbol: </b>", geneSymbol,
      "</br>", "<b>Organism: </b>", organism,
      "</br>", "<b>Tissue: </b>", tissue,
      #"</br>", "<b>Assay Format Type: </b>", cellFormat,
      "</br>", "<b>Assay Format Type: </b>", assayFormatType,
      "</br>", "<b>Biological Process Target: </b>", biologicalProcessTarget,
      "</br>", "<b>Intended Target Family: </b>", intendedTargetFamily,
      ifelse(aeid %in% tcplPlot::available_assay_descriptions,"</br> <b>Assay Description Document Available (click to open)</b>","")
    )
  )

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

  ax <- list(
    title = "",
    type = "log",
    zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside",
    tickformat='.3'
  )

  ay <- list(
    title = "",
    type = ifelse(max(m5$scaledTop, na.rm = TRUE) > 100,"log","linear"),
    # zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside"
  )

  if(!is.na(cytotox) & !is.infinite(cytotox)){
    vline_lst <- list(vline(cytotox),vline(cytotox_med))
  }else{
    vline_lst <- list()
  }

  fig <- layout(fig,
                xaxis = ax,
                yaxis = ay,
                shapes = vline_lst,
                updatemenus = list(
                  list(
                    x = .1,
                    y = 1.1,
                    buttons = list(

                      list(method = "relayout",
                           args = list(list(yaxis = list(type = ifelse(max(m5$scaledTop, na.rm = TRUE) > 100,"log","linear"),
                                                         zeroline = FALSE,
                                                         showline = TRUE,
                                                         showgrid = FALSE,
                                                         ticks = "outside"))),
                           label = ifelse(max(m5$scaledTop, na.rm = TRUE) > 100,"log","linear")),

                      list(method = "relayout",
                           args = list(list(yaxis = list(type = ifelse(max(m5$scaledTop, na.rm = TRUE) > 100,"linear","log"),
                                                         zeroline = FALSE,
                                                         showline = TRUE,
                                                         showgrid = FALSE,
                                                         ticks = "outside"))),
                           label = ifelse(max(m5$scaledTop, na.rm = TRUE) > 100,"linear","log"))))
                ))


  if(!is.na(cytotox) & !is.infinite(cytotox)){
    fig <- fig %>% add_annotations(
      yref = "paper",
      xref = "x",
      x = log10(cytotox),
      y = 1,
      text = paste0("Cytotox Lower Bound in &#181;M (", specify_decimal(cytotox, 3), ")"),
      showarrow = F,
      textangle = 90,
      xanchor = "left"
    )

    if (cytotox != cytotox_med) {
      fig <- fig %>% add_annotations(
        yref = "paper",
        xref = "x",
        x = log10(cytotox_med),
        y = 1,
        text = paste0("Cytotox Median in &#181;M (", specify_decimal(cytotox_med, 3), ")"),
        showarrow = F,
        textangle = 90,
        xanchor = "left"
      )
    }
  }

  fig <- fig %>% layout(legend = list(
    x = -.15,
    y = 0,
    xanchor = "right",
    tracegroupgap = 0,
    valign = "top"
  ))


  fig <- fig %>% add_annotations(
    x = 0,
    y = 1,
    xref = "paper",
    yref = "paper",
    text = "Scaled Top",
    showarrow = F,
    textangle = 90
  )

  fig <- fig %>% add_annotations(
    x = 1,
    y = 0,
    xref = "paper",
    yref = "paper",
    text = "AC50 (&#181;M)",
    showarrow = F
  )

  onRender(
    fig, "
    function(el) {
      el.on('plotly_click', function(d) {
        var url = d.points[0].customdata;
        if (url.length > 1) {window.open(url);}
      });
    }
  ")
}


#' Summary Plot for HTTr data
#'
#' @param dat dataframe with all associated plotting data as columns
#'
#' @return Plot with combined checkbox functionality
#' @export
#'
#' @import plotly
#' @importFrom crosstalk filter_checkbox
#' @importFrom manipulateWidget  combineWidgets
#' @importFrom htmltools tagQuery p
tcplPlotSummaryHTTr <- function(dat) {
  m5 <- dat
  # change nas to character value so that plotly will recognize it in legend
  m5$targetClass[is.na(m5$targetClass)] <- "NA"
  m5$time <- as.numeric(m5$time)
  cytotox <- min(as.numeric(m5$cytotoxLowBndUm))
  cytotox_med <- min(as.numeric(m5$cytotoxMednUm))
  # set a constant x and y range based on min and max values
  # add buffer so that points are not quite on the edge
  buffer <- .05
  x_range <- c(min(log10(as.numeric(m5$bmd))) - buffer, max(log10(as.numeric(m5$bmd))) + buffer)
  y_range <- c(min(as.numeric(m5$top)) - buffer, max(as.numeric(m5$top)) + buffer)

  m5 <- highlight_key(m5)

  fig <- plot_ly(
    data = m5,
    type = "scatter",
    mode = "markers",
    x = ~bmd,
    y = ~top,
    color = ~targetClass,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", "<b>Signature (uM): </b>", signature,
      "</br>", "<b>Cell Type: </b>", cellType,
      "</br>", "<b>Target Class: </b>", targetClass,
      "</br>", "<b>Super Target: </b>", superTarget,
      "</br>", "<b>Target Level: </b>", superTargetLevel,
      "</br>", "<b>Hitcall: </b>", hitCall,
      "</br>", "<b>Top: </b>", top,
      "</br>", "<b>BMD: </b>", bmd,
      "</br>", "<b>T/C: </b>", topOverCutoff,
      "</br>", "<b>SID: </b>", sampleId
    )
  )

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

  ax <- list(
    title = "",
    type = "log",
    zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside",
    range = x_range
  )

  ay <- list(
    title = "",
    type = "linear",
    # zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside",
    range = y_range
  )

  cytotox_list <- ""
  if(!is.na(cytotox) & !is.infinite(cytotox)){
    cytotox_list <- list(vline(cytotox), vline(cytotox_med))
  }

  fig <- layout(fig,
    xaxis = ax,
    yaxis = ay,
    shapes = cytotox_list
  )


  if (!is.na(cytotox) & !is.infinite(cytotox)) {
    fig <- fig %>% add_annotations(
      yref = "paper",
      xref = "x",
      x = log10(cytotox),
      y = 1,
      text = paste0("Cytotox Lower Bound in &#181;M (", specify_decimal(cytotox, 3), ")"),
      showarrow = F,
      textangle = 90,
      xanchor = "left"
    )

    if (cytotox != cytotox_med) {
      fig <- fig %>% add_annotations(
        yref = "paper",
        xref = "x",
        x = log10(cytotox_med),
        y = 1,
        text = paste0("Cytotox Median in &#181;M (", specify_decimal(cytotox_med, 3), ")"),
        showarrow = F,
        textangle = 90,
        xanchor = "left"
      )
    }
  }

  fig <- fig %>% layout(legend = list(
    title=list(text='<b> Target Class </b>'),
    x = -.15,
    y = 0,
    xanchor = "right",
    tracegroupgap = 0,
    valign = "top"
  ))


  fig <- fig %>% add_annotations(
    x = 0,
    y = 1,
    xref = "paper",
    yref = "paper",
    text = "Top",
    showarrow = F,
    textangle = 90
  )

  fig <- fig %>% add_annotations(
    x = 1,
    y = 0,
    xref = "paper",
    yref = "paper",
    text = "BMD (&#181;M)",
    showarrow = F
  )

  p_3 <- htmltools::p("*Default Toggle Switches Set To No Filtering", style = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")

  ct <- crosstalk::filter_checkbox("variable",
    "Cell Type",
    m5,
    ~cellType,
    inline = TRUE
  )

  # add font styling to match plotly
  tagQ <- htmltools::tagQuery(ct)
  ct <- tagQ$
    addAttrs("style" = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")$
    allTags()

  ct2 <- crosstalk::filter_checkbox("variable",
    "Exposure Duration",
    m5,
    ~time,
    inline = TRUE
  )
  # add font styling to match plotly
  tagQ2 <- htmltools::tagQuery(ct2)
  ct2 <- tagQ2$
    addAttrs("style" = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")$
    allTags()

  ct <- combineWidgets(list = list(ct, ct2), ncol = 2)


  manipulateWidget::combineWidgets(ct, fig, rowsize = c(1, 9))
}



#' Summary Plot for HTPP data
#'
#' @param dat dataframe with all associated data required to plot
#' @param approach either category or feature to indicate which subset to filter
#'
#' @return Summary level plot of the HTPP data
#' @export
#'
#' @import plotly
#' @importFrom stats setNames
#' @importFrom htmlwidgets onRender
#' @importFrom crosstalk filter_checkbox
#' @importFrom manipulateWidget  combineWidgets
#' @importFrom htmltools tagQuery p
tcplPlotSummaryHTPP <- function(dat,approach = "category") {
  #variable binding
  bmd <- sampleId <- topOverCutoff <- topOrig <- categoryNameDescription <- NULL
  categoryName <- channel <- module <- compartment <- NULL

  #return 400 if approach method is not feature or category
  if(!approach %in% c("feature" , "category")){
    return(400)
  }
  #filter based on category/global or feature
  if(approach == "category"){
    filter_param <- c("category","global")
  }else{
    filter_param <- c("feature")
  }
  m5 <- dat
  m5 <- m5[!is.na(m5$bmd)]
  m5$bmd <- as.numeric(m5$bmd)

  #Find lowest bmc for the chemical and filter to only plot those sample_ids
  representative_sample <- m5 %>% filter(approach == "category") %>% slice_min(bmd) %>% pull(sampleId)

  #if there are no category hits we need to take the representative sample from feature level
  if(length(representative_sample)<1){
    representative_sample <- m5 %>% slice_min(bmd) %>% pull(sampleId)
  }

  # m5 <- m5 %>% filter(sampleId == representative_sample)

  #get values for vertical lines
  global_bmc <- m5 %>% filter(approach == "global") %>% pull(bmd)
  category_min_bmc <- m5 %>% filter(approach == "category") %>% slice_min(bmd) %>% pull(bmd)
  global <- TRUE
  #if no global bmc then set global to false and don't draw line
  if(length(global_bmc)<1){global <- FALSE}

  #if no category bmc then set cat_min to false and don't draw line
  cat_min <- TRUE
  if(length(category_min_bmc)<1){cat_min <- FALSE}

  # Get the ymax and set to 5 if less than 5
  ymax <- m5 %>%
    slice_max(topOverCutoff) %>%
    pull(topOverCutoff) %>% as.numeric()
  if (ymax < 5) {
    ymax <- 5
  }
  # set the y-axis range
  if (approach == "category") {
    yrange <- c(0, ymax)
  } else {
    yrange <- c(-ymax, ymax)
  }

  # get minimum global and category level hits
  min_bmc <- m5 |>
    filter(approach %in% c("global","category")) |>
    group_by(approach,sampleId, cellType, exposureDuration, seedingDensity) |>
    mutate(size = bmd == min(bmd, na.rm = TRUE)) |>
    ungroup() |>
    mutate(size = ifelse(categoryName == "global", TRUE, size)) |>
    mutate(size = size * 2 + 1) |>
    filter(size == 3)

  #filter data by approach
  m5 <- m5 %>% filter(approach %in% filter_param)

  #set default point size for m5
  m5$size <- 1
  #add min bmc data that has larger points
  m5 <- rbind(m5,min_bmc)
  #remove duplicates
  m5 <- m5 %>% group_by(across(c(-size))) |> summarise(size = max(size))

  #set topovercutoff and toporig to numeric
  m5$topOverCutoff <- as.numeric(m5$topOverCutoff)
  m5$topOrig <- as.numeric(m5$topOrig)

  #if doing feature level multiply the top over cutoff by sign of top
  if(approach == "feature"){
    m5 <- m5 %>% rowwise() %>% mutate(topOverCutoff = topOverCutoff*sign(topOrig))
  }


  ## ----Code from Johanna
  # legend("topleft", cex=cexLegend, y.intersp=1.5, x.intersp=2,
  #        bg="transparent", bty="n",
  #        c("Shape / Position", "DNA", "RNA", "ER", "AGP", "Mito",
  #          "Intensity", "Texture", "Morphology", "Symmetry", "Compactness", "Axial", "Radial", "Profile",
  #          "Cell / Cytoplasm", "Nuclei", "Ring", "Membrane"),
  #        col=c(adjustcolor(c("gray50","blue","green","green4","orange","deeppink3"), alpha.f=0.7),
  #              rep("black", times=8),
  #              rep("gray50", times=4) ),
  #        pch=c(rep(19, times=6),
  #              utf8ToInt("ITMSCARP"),
  #              19, 15, 18, 17),
  #        pt.cex=c(rep(cexPch, times=6),
  #                 rep(cexLegend, times=8),
  #                 cexPch*c(1,1,1.2,1)), pt.bg="white")
  # browser()
  #begin section for color coding
  #fill na values for category name desc
  m5 <- m5 |> mutate(categoryNameDescription = coalesce(categoryNameDescription,categoryName))

  #extract channel module and compartment fro categoryNameDesc
  m5 <- m5 |>  mutate(
    channel_ex = gsub("_\\w+", "", categoryNameDescription),
    module_ex = gsub("_[[:alpha:]]+$","",gsub("^[[:alpha:]]+_","",categoryNameDescription)),
    compartment_ex = gsub("\\w+_", "", categoryNameDescription)
  )

  #fill na channel module compartments and remove temporary columns
  m5 <- m5 |>  mutate(
    channel = coalesce(channel,channel_ex),
    module = coalesce(module,module_ex),
    compartment = coalesce(compartment,compartment_ex)
  ) |> select(-channel_ex,-module_ex,-compartment_ex)

  m5 <- m5 %>% mutate(
    color = case_when(
      channel == "Position" ~ "gray50",
      channel == "Shape" ~ "gray50",
      channel == "global" ~ "purple",
      channel == "DNA" ~ "blue",
      channel == "RNA" ~ "green",
      channel == "ER" ~ "green4",
      channel == "AGP" ~ "orange",
      channel == "Mito" ~ "deeppink3",
      TRUE ~ "gray50"
    ))
  # set each point's color
  pal <- c("gray50","gray50","purple","blue","green","green4","orange","deeppink3")
  pal <- setNames(pal, c("Position","Shape","global","DNA","RNA","ER","AGP","Mito"))

  m5 <- m5 %>% mutate(
    shape = case_when(
      compartment == "Cells" ~ "circle",
      compartment == "Cytoplasm" ~ "circle",
      compartment == "Nuclei" ~ "square",
      compartment == "Ring" ~ "diamond",
      compartment == "Membrane" ~ "triangle",
      compartment == "Shape" ~ "circle",
      compartment == "global" ~ "circle",
      TRUE ~ "circle"
    ))

  # set each point's shape
  sha <- c("circle","circle","square","diamond","triangle","circle","circle","circle")
  sha <- setNames(sha, c("Cells","Cytoplasm","Nuclei","Ring","Membrane","Shape","global","Position"))

  #set y range of cutoff band
  ifelse(approach == "category", ycoff <- c(0, 1), ycoff <- c(-1, 1))
  #set x range of cutoff band
  minConc <- m5 %>% pull(minConc) %>% unique() %>% as.numeric()
  maxConc <- m5 %>% pull(maxConc) %>% unique() %>% as.numeric()

  #feature level set the order of the f_ names
  if(approach == "feature"){
    newvec <- m5$categoryName
    lvl <- newvec[order(nchar(newvec), newvec)]
    m5$categoryName <- factor(m5$categoryName, levels = unique(lvl))
  }

  #set as shared data for checkboxes
  m5 <- highlight_key(m5)

  fig <- plot_ly(
    data = m5,
    type = "scatter",
    mode = "markers",
    x = ~as.numeric(bmd),
    y = ~as.numeric(topOverCutoff),
    name = ~categoryName,
    legendrank = ~as.numeric(categoryName),
    color = ~channel,
    size = ~size,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", "<b>Sample ID: </b>", sampleId,
      "</br>", "<b>Cell Type: </b>", cellType,
      "</br>", "<b>Exposure Duration: </b>", exposureDuration,
      "</br>", "<b>Seeding Density: </b>", seedingDensity,
      "</br>", "<b>Endpoint Name: </b>", categoryName,
      "</br>", "<b>Endpoint Description: </b>", featureNameHarmony,
      "</br>", "<b>Hitcall: </b>", hitCall,
      "</br>", "<b>BMD: </b>", bmd,
      "</br>", "<b>T/C: </b>", topOverCutoff
    ),
    colors = pal,
    symbol = ~compartment,
    symbols = sha
  )

  ax <- list(
    title = "",
    type = "log",
    zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside",
    tickformat='.3'
  )

  ay <- list(
    title = "",
    type = "linear",
    zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside",
    range = yrange
  )

  fig <- layout(fig,
                xaxis = ax,
                yaxis = ay
  )


  resolution <- 100
  y_range <- seq(from = yrange[1], to = yrange[2], length.out = resolution)
  # add global annotation
  # if(global){
  # fig <- fig %>% add_trace(
  #   data = tibble(x = global_bmc, y = y_range),
  #   x = ~x,
  #   y = ~y,
  #   type = "scatter",
  #   mode = "lines",
  #   name = "Global BMD",
  #   line = list(dash = "dash", width = 0.5, color = "black"),
  #   inherit = FALSE,
  #   hoverinfo = "text",
  #   text = ~ paste(
  #     "</br>", paste0("Global BMD (", specify_decimal(global_bmc,2), ")")
  #   )
  # )
  # }
  #
  # if(approach == "feature"){
  #   if(cat_min){
  #   fig <- fig %>% add_trace(
  #     data = tibble(x = category_min_bmc, y = y_range),
  #     x = ~x,
  #     y = ~y,
  #     type = "scatter",
  #     mode = "lines",
  #     name = "Minimum Category BMD",
  #     line = list(dash = "dash", width = 0.5, color = "black"),
  #     inherit = FALSE,
  #     hoverinfo = "text",
  #     text = ~ paste(
  #       "</br>", paste0("Minimum Category BMD (", specify_decimal(category_min_bmc,2), ")")
  #     )
  #   )
  #   }
  # }


  #create grey rectangle representing cutoff region
  coff_region <- list(type = "rect",
                fillcolor = "lightgray",
                line = list(color = "lightgray"),
                opacity = 0.4,
                x0 = minConc, x1 = maxConc, xref = "x",
                y0 = ycoff[1], y1 = ycoff[2], yref = "y",
                layer = "below",
                linewidth = 0)

  fig <- fig %>% layout(legend = list(
    x = -.15,
    y = 0,
    xanchor = "right",
    tracegroupgap = 0,
    valign = "top"
  ), shapes = list(coff_region))


  fig <- fig %>% add_annotations(
    x = 0,
    y = 1,
    xref = "paper",
    yref = "paper",
    text = "Efficacy (Top Over Cutoff)",
    showarrow = F,
    textangle = 90
  )

  fig <- fig %>% add_annotations(
    x = 1,
    y = 0,
    xref = "paper",
    yref = "paper",
    text = "BMD (&#181;M)",
    showarrow = F
  )




  p_3 <- htmltools::p("*Default Toggle Switches Set To No Filtering", style = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")

  ct <- crosstalk::filter_checkbox("variable",
                                   "Cell Type",
                                   m5,
                                   ~cellType,
                                   inline = TRUE
  )

  # add font styling to match plotly
  tagQ <- htmltools::tagQuery(ct)
  ct <- tagQ$
    addAttrs("style" = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")$
    allTags()

  ct2 <- crosstalk::filter_checkbox("variable",
                                    "Exposure Duration",
                                    m5,
                                    ~exposureDuration,
                                    inline = TRUE
  )
  # add font styling to match plotly
  tagQ2 <- htmltools::tagQuery(ct2)
  ct2 <- tagQ2$
    addAttrs("style" = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")$
    allTags()

  ct3 <- crosstalk::filter_checkbox("variable",
                                    "Seeding Density",
                                    m5,
                                    ~seedingDensity,
                                    inline = TRUE
  )
  # add font styling to match plotly
  tagQ3 <- htmltools::tagQuery(ct3)
  ct3 <- tagQ3$
    addAttrs("style" = "font-family: 'Open Sans', verdana, arial, sans-serif; font-size: 12px;")$
    allTags()

  ct <- combineWidgets(list = list(ct, ct2,ct3), ncol = 3)


  manipulateWidget::combineWidgets(ct, fig, rowsize = c(1, 9))
}



#' Function for switching to scientific notation in ccd
#'
#' @param x Number to be displayed
#' @param n Number of decimal places to round to
specify_decimal <- Vectorize(function(x, n=3) {
  if (!is.na(x)) {
    if (x >= 1000 | (x<=0.0005 & x != 0)) {
      # if x>=1000, convert value to scientific notation
      formatC(x, format = "e", digits = 1)
    } else { # else, round the value to 3 decimal places
      format(round(x, n), nsmall = 3)
    }
  } else {
    return(NA)
  }
}
)




#' Summary plot for bioactivity (invitrodb) data
#'
#' @param dat dataframe with plotting parameters from output of tcplApiQuery
#'
#' @return Summary level plot
#' @export
#'
#' @import plotly
#' @import dplyr
tcplPlotSummaryAEID <- function(dat) {

  m5 <- dat
  # remove inactives or na values from plotting
  m5 <- m5[!is.na(m5$top_over_cutoff)]
  m5 <- m5 |> dplyr::filter(!is.na(dtxsid))
  url_dat <- paste0("https://comptox.epa.gov/dashboard/chemical/invitrodb/", m5$dtxsid)

  #collapse gene info
  m5 <- m5 |> rowwise() |> mutate(geneSymbol = paste0(gene$geneSymbol, collapse = "|"))

  fig <- plot_ly(
    data = m5,
    type = "scatter",
    mode = "markers",
    x = ~ac50,
    y = ~top_over_cutoff,
    #color = ~intendedTargetFamily,
    customdata = url_dat,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", "<b>AC50 (uM): </b>", specify_decimal(ac50, 3),
      #"</br>","<b>Scaled top: </b>", specify_decimal(scaled_top,3),
      "</br>", "<b>Assay Endpoint Name: </b>", assayComponentEndpointName,
      "</br>", "<b>Gene Symbol: </b>", geneSymbol,
      "</br>", "<b>Organism: </b>", organism,
      "</br>", "<b>Tissue: </b>", tissue,
      #"</br>", "<b>Assay Format Type: </b>", cellFormat,
      "</br>", "<b>Assay Format Type: </b>", assayFormatType,
      "</br>", "<b>Biological Process Target: </b>", biologicalProcessTarget,
      "</br>", "<b>Intended Target Family: </b>", intendedTargetFamily,
      ifelse(aeid %in% tcplPlot::available_assay_descriptions,"</br> <b>Assay Description Document Available (click to open)</b>","")
    )
  )

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

  ax <- list(
    title = "",
    type = "log",
    zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside",
    tickformat='.3'
  )

  ay <- list(
    title = "",
    type = ifelse(max(m5$top_over_cutoff, na.rm = TRUE) > 100,"log","linear"),
    # zeroline = FALSE,
    showline = TRUE,
    showgrid = FALSE,
    ticks = "outside"
  )

  vline_lst <- list()

  fig <- layout(fig,
                xaxis = ax,
                yaxis = ay,
                shapes = vline_lst,
                updatemenus = list(
                  list(
                    x = .1,
                    y = 1.1,
                    buttons = list(

                      list(method = "relayout",
                           args = list(list(yaxis = list(type = ifelse(max(m5$top_over_cutoff, na.rm = TRUE) > 100,"log","linear"),
                                                         zeroline = FALSE,
                                                         showline = TRUE,
                                                         showgrid = FALSE,
                                                         ticks = "outside"))),
                           label = ifelse(max(m5$top_over_cutoff, na.rm = TRUE) > 100,"log","linear")),

                      list(method = "relayout",
                           args = list(list(yaxis = list(type = ifelse(max(m5$top_over_cutoff, na.rm = TRUE) > 100,"linear","log"),
                                                         zeroline = FALSE,
                                                         showline = TRUE,
                                                         showgrid = FALSE,
                                                         ticks = "outside"))),
                           label = ifelse(max(m5$top_over_cutoff, na.rm = TRUE) > 100,"linear","log"))))
                ))


  fig <- fig %>% layout(legend = list(
    x = -.15,
    y = 0,
    xanchor = "right",
    tracegroupgap = 0,
    valign = "top"
  ))


  fig <- fig %>% add_annotations(
    x = 0,
    y = 1,
    xref = "paper",
    yref = "paper",
    text = "Scaled Top",
    showarrow = F,
    textangle = 90
  )

  fig <- fig %>% add_annotations(
    x = 1,
    y = 0,
    xref = "paper",
    yref = "paper",
    text = "AC50 (&#181;M)",
    showarrow = F
  )

  onRender(
    fig, "
    function(el) {
      el.on('plotly_click', function(d) {
        var url = d.points[0].customdata;
        if (url.length > 1) {window.open(url);}
      });
    }
  ")
}

