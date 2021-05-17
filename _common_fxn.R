########################################################################
# Define common functions
#
# Create MH::03/02/2021
########################################################################


###############################
# Trimmer for long texts (eg. for use in labels, axis texts, ..)
###############################
trimmer <- function(x, break_limit) {
  sapply(strwrap(x, break_limit, simplify = FALSE), paste, collapse = "\n")
}

###############################
# Function to either save/read ggplot
# from file, or print from session created (if fig_create = TRUE)
###############################
fig_writeread_fromfile_fxn <- function(g1 = g1, # gglopt object
                                       fig_fromfile = TRUE,
                                       fig_create = FALSE,
                                       rel_fig_path = "./fig",
                                       fig_name = "test",
                                       out_device = "png",
                                       fig_width = 9,
                                       fig_height = 6) {
  if (fig_fromfile) {
    # out_device <- out_device
    file.out <- file.path(rel_fig_path, paste0(fig_name, ".", out_device))

    if (fig_create) {
      if (out_device == "pdf") {
        ggsave(g1, filename = file.out, device = cairo_pdf, width = fig_width, height = fig_height)
      } else {
        ggsave(g1, filename = file.out, device = out_device, width = fig_width, height = fig_height)
      }
    }

    knitr::include_graphics(file.out)
  } else {
    print(g1)
  }
}
###############################
# Treemap (with option to create fig, save as file, or only read already created file)
###############################

unido_treemap <- function(df,
                          area = df$mvacod,
                          fill = df$ADesc,
                          label = df$CDesc,
                          subgroup = df$ADesc,
                          colorpal = unido_color_palette[[n_lev]],
                          fig_create = TRUE,
                          fig_name = "test",
                          out_device = "png",
                          fig_fromfile = FALSE,
                          fig_width = 12,
                          fig_height = 8,
                          rel_fig_path = rel_fig_path) {
  if (F) {
    df_groups <- unique(country[grep("IND|156|EIEX|DEVOT|LDC$", country$ACode), c("ADesc", "ACode")])
    df_groups <- country[country$ACode %in% df_groups$ACode, ]
    # View(df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],])

    df <- merge(mvagdp[mvagdp$CompType == "M", ], df_groups, by.x = "acode", by.y = "Ccode", all = TRUE)
    # unique(df[ is.na(df$ADesc), c("acode", "desc")])
    rel_vars <- c("acode", "CDesc", "ADesc", "year", "mvacod")
    df1 <- df[, rel_vars][df$year %in% c(2020), ]
    df1$ADesc <- gsub("<.*>", "", df1$ADesc)
    n_lev <- length(unique(df$ADesc))
  }

  if (fig_create) {
    g1 <- ggplot(df, aes(area = area, fill = fill, label = label, subgroup = subgroup)) +
      geom_treemap(colour = "#009CDC") + # facet_wrap( ~ cc1) +
      geom_treemap_text(grow = T, reflow = T, colour = "black", alpha = 0.9) +
      geom_treemap_subgroup_border(colour = "#009CDC") +
      geom_treemap_subgroup_text(
        place = "centre",
        grow = T,
        alpha = 0.7,
        colour = "#009CDC", # "darkmagenta", #"black",
        fontface = "italic",
        min.size = 0,
        angle = 45
      ) +
      scale_fill_manual(values = colorpal) +
      theme(legend.position = "none")
  }

  fig_writeread_fromfile_fxn(
    g1,
    fig_fromfile = fig_fromfile,
    fig_create = fig_create,
    rel_fig_path = rel_fig_path,
    fig_name = fig_name,
    out_device = out_device,
    fig_width = fig_width,
    fig_height = fig_height
  )
}


###############################
# leaflet map 1 categoric variable
# (with option to create fig, save as file, or only read already created file)
###############################

unido_leaflet1 <- function(unido_shape = unido_shape,
                           categoric_var = unido_shape$ADesc,
                           categoric_var_title = "Development stage",
                           categoric_var_levels = factor(levels(unido_shape$ADesc),
                             levels =
                               c(
                                 "Industrialized Economies",
                                 "Emerging Industrial Economies <incl. China>",
                                 "Other Developing Economies",
                                 "Least Developed Countries"
                               )
                           ),
                           label = unido_shape$CDesc.y,
                           popup_text = paste("", unido_shape$CDesc.y, "<br/>", unido_shape$ADesc),
                           colorpal = unido_color_palette[[4]],
                           set_lat = 40,
                           set_zoom = 1.1,
                           fig_create = TRUE,
                           fig_name = "test",
                           out_device = "png",
                           fig_fromfile = FALSE,
                           fig_width = 650,
                           fig_height = 500) {
  if (F) {
    df_groups <- c("IND", "EIE", "DEVOT", "LDC")
    df <- country[country$ACode %in% df_groups, ]

    ###### WORLDMAP Overview
    unido_shape <- merge(unido_shapefile,
      df,
      by = "Ccode",
      all.x = TRUE
    )
  }
  if (fig_create) {
    # categoric_var <- as.factor(gsub(">", ")", gsub("<", "(", categoric_var)))
    categoric_var <- as.factor(categoric_var)
    cDev_palette <- colorFactor(colorpal, categoric_var)

    # unido_shape <- unido_shape[!(unido_shape$NAME_EN == "Antarctica"), ]
    # title <- tags$div(HTML(""))
    g1 <- unido_shape %>%
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 1
      )) %>%
      setMapWidgetStyle(list(background = "transparent")) %>%
      setView(lng = 0, lat = set_lat, zoom = set_zoom) %>%
      addPolygons(
        weight = 1,
        color = ~ cDev_palette(categoric_var),
        label = ~label,
        popup = ~popup_text
      ) %>%
      addLegend(
        pal = cDev_palette,
        values = factor(categoric_var_levels),
        # values = n.bins,
        position = "topright",
        title = categoric_var_title
      )
  }

  if (fig_fromfile) {
    # out_device <- out_device
    file.out <- file.path(rel_fig_path, paste0(fig_name, ".", out_device))

    if (fig_create) {
      invisible(g1 %>%
        htmltools::html_print() %>%
        webshot2::webshot(file = file.out, delay = 0.5, vwidth = fig_width, vheight = fig_height, zoom = 4))
      invisible(gc())
      Sys.sleep(2)
    }

    knitr::include_graphics(file.out)
  } else {
    browsable(
      tagList(list(
        tags$head(
          tags$style(
            ".leaflet .legend {width:300px; text-align: left;}",
            ".leaflet .legend i{float: left;}",
            ".leaflet .legend label{float:left; text-align: left;}"
          )
        ),
        g1
      ))
    )
  }
}

###############################
# leaflet map 2 categoric variable
# (with option to create fig, save as file, or only read already created file)
###############################

unido_leaflet2 <- function(unido_shape = unido_shape,
                           categoric_var = unido_shape$ADesc,
                           categoric_var_title = "Development stage",
                           categoric_var_levels = factor(levels(unido_shape$ADesc),
                             levels =
                               c(
                                 "Industrialized Economies",
                                 "Emerging Industrial Economies <incl. China>",
                                 "Other Developing Economies",
                                 "Least Developed Countries"
                               )
                           ),
                           categoric_var2 = unido_shape$mvapccat,
                           categoric_var2_title = "MVA per capita",
                           categoric_var2_levels = levels(unido_shape$mvapccat),
                           categoric_var2_opacity = 0.5,
                           categoric_var2_legend_pos = "topright",
                           label = unido_shape$CDesc.y,
                           popup_text = paste("", unido_shape$CDesc.y, "<br/>", unido_shape$ADesc),
                           colorpal = unido_color_palette[[4]],
                           colorpal2 = unido_color_palette[[4]],
                           fig_create = TRUE,
                           fig_name = "test",
                           out_device = "png",
                           fig_fromfile = FALSE,
                           fig_width = 650,
                           fig_height = 500) {
  if (F) {
    df_groups <- c("IND", "EIE", "DEVOT", "LDC")
    df_groups <- country[country$ACode %in% df_groups, ]

    # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]

    df <- merge(df_groups,
      mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% "2020", ],
      by.x = "Ccode",
      by.y = "acode", all.y = TRUE
    )

    ###### WORLDMAP Overview
    unido_shape <- merge(unido_shapefile,
      df,
      by = "Ccode",
      all.x = TRUE
    )
    unido_shape$mvapccat <- as.factor(cut(unido_shape$mvapc,
      c(0, 100, 1000, 3000, Inf),
      labels = c("0 to 99 USD", "100 to 999 USD", "1000 to 2999 USD", "3000 and more USD"),
      include.lowest = TRUE, right = FALSE
    ))
  }

  if (fig_create) {
    # categoric_var <- as.factor(gsub(">", ")", gsub("<", "(", categoric_var)))
    categoric_var <- as.factor(categoric_var)
    categoric_var2 <- as.factor(categoric_var2)
    cDev_palette <- colorFactor(colorpal, categoric_var)
    cDev_palette_circle <- colorFactor(colorpal2, categoric_var2)

    # title <- tags$div(HTML(""))
    g1 <- unido_shape %>%
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 1.1
      )) %>%
      setMapWidgetStyle(list(background = "transparent")) %>%
      setView(lng = 0, lat = 40, zoom = 1.1) %>%
      addPolygons(
        weight = 1,
        color = ~ cDev_palette(categoric_var),
        label = ~label,
        popup = ~popup_text
      ) %>%
      addLegend(
        pal = cDev_palette,
        values = factor(categoric_var_levels),
        # values = n.bins,
        position = "topright",
        title = categoric_var_title
      ) %>%
      addCircleMarkers(
        lng = ~center_x, lat = ~center_y,
        color = ~ cDev_palette_circle(categoric_var2),
        radius = 1,
        fillOpacity = categoric_var2_opacity,
        opacity = categoric_var2_opacity
      )
    if (categoric_var2_legend_pos != "none") {
      g1 <- g1 %>%
        addLegend(
          pal = cDev_palette_circle, opacity = 0.5,
          values = factor(categoric_var2_levels),
          # values = n.bins,
          position = categoric_var2_legend_pos,
          title = categoric_var2_title
        )
    }
  }

  if (fig_fromfile) {
    # out_device <- out_device
    file.out <- file.path(rel_fig_path, paste0(fig_name, ".", out_device))

    if (fig_create) {
      invisible(g1 %>%
        htmltools::html_print() %>%
        webshot2::webshot(file = file.out, delay = 0.5, vwidth = fig_width, vheight = fig_height, zoom = 4))
      invisible(gc())
      Sys.sleep(2)
    }

    knitr::include_graphics(file.out)
  } else {
    browsable(
      tagList(list(
        tags$head(
          tags$style(
            ".leaflet .legend {width:300px; text-align: left;}",
            ".leaflet .legend i{float: left;}",
            ".leaflet .legend label{float:left; text-align: left;}"
          )
        ),
        g1
      ))
    )
  }
}


###############################
# gganimate ranking plot
# (with option to create fig, save as file, or only read already created file)
# MH:14022021 animate works html and pdf if opened with adobe...
#   however not yet if opened with PDFViewer in webbrowser
###############################
unido_rankplot <- function(df,
                           byvartime = df$year,
                           byvargroup = df$ADesc,
                           rankvar = df$mvacod,
                           nbiggest = 20,
                           share_show = TRUE,
                           label_show_round = 1,
                           colorpal = rep(unido_color_palette[[4]], 7),
                           subtitle = "Top 10 Countries, Year : {current_frame}",
                           rel_fig_cap = "Leading manufacturers (Share of MVA at constant 2015 prices)",
                           fig_create = TRUE,
                           fig_name = "test",
                           islatexout = FALSE,
                           islatexout.fig.out = "figure*", # "figure", "marginfigure"
                           islatexout.nocaption = FALSE,
                           fig_fromfile = FALSE,
                           fig_width = 800,
                           fig_height = 500,
                           fig_frames = 50) {
  if (F) {
    df_groups <- country[country$Operator == "M", ]
    # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]

    df <- merge(mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% seq(1990, 2020, 5), ], df_groups, by.x = "acode", by.y = "Ccode", all = TRUE)
  }
  if (fig_create) {
    df_formatted <- df %>%
      mutate(byvartime = byvartime, byvargroup = byvargroup, rankvar = rankvar, share_show = share_show) %>%
      group_by(byvartime) %>%
      mutate(rankvar = ifelse(share_show, rankvar / sum(rankvar) * 100, rankvar)) %>%
      # The * 1 makes it possible to have non-integer ranks while sliding
      mutate(
        value = rankvar,
        # rank = floor(rank(-value)),
        rank = rank(-value),
        Value_rel = value / value[rank == 1],
        Value_lbl = paste0(" ", round(value, label_show_round))
      ) %>%
      group_by(byvargroup) %>%
      filter(rank <= nbiggest) %>%
      ungroup()

    g1_static <- ggplot(df_formatted, aes(rank,
      group = byvargroup,
      fill = as.factor(byvargroup), color = as.factor(byvargroup)
    )) +
      geom_tile(aes(
        y = value / 2,
        height = value,
        width = 0.9
      ), alpha = 0.8, color = NA) +
      geom_text(aes(y = 0, label = paste(byvargroup, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y = value, label = Value_lbl, hjust = 0)) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      scale_color_manual(values = colorpal) +
      scale_fill_manual(values = colorpal) +
      guides(color = FALSE, fill = FALSE) +
      theme_unido() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .1),
        panel.grid.minor.x = element_line(size = .1),
        # plot.background = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 6, "cm")
      )

    g1 <- g1_static + transition_manual(byvartime) +
      # transition_states(byvartime,
      # transition_length = 5,
      # state_length = 1,
      # wrap = TRUE
      # ) +
      # view_follow(fixed_x = TRUE) +
      labs(
        subtitle = subtitle,
        caption = rel_fig_cap
      )
  }

  if (fig_fromfile) {
    if (exists("islatexout")) {
      if (islatexout) out_device <- "png" else out_device <- "gif"
    } else {
      out_device <- "gif"
    }

    file.out.name <- fig_name
    file.out <- file.path(rel_fig_path, file.out.name, paste0(file.out.name, ".", out_device))
    if (!dir.exists(file.path(rel_fig_path, file.out.name))) dir.create(file.path(rel_fig_path, file.out.name))

    nframes <- fig_frames
    nwidth <- fig_width
    nheigth <- fig_height
    if (fig_create) {
      if (out_device %in% c("png", "svg")) {
        animate(g1,
          nframes = nframes, device = out_device, width = nwidth,
          height = nheigth,
          renderer = file_renderer(file.path(rel_fig_path, file.out.name), prefix = file.out.name, overwrite = TRUE)
        )
      } else {
        invisible(
          anim_save(file.out, animate(g1,
            nframes = nframes,
            width = nwidth,
            height = nheigth,
            duration = 10
          ))
        )
      }
    }

    if (islatexout) {
      if (!islatexout.nocaption) {
        cat("\\begin{", islatexout.fig.out, "}\n  \\animategraphics[method=icon, controls=step,width=\\linewidth]{12}{",
          file.path(rel_fig_path, file.out.name, file.out.name), "}{0001}{",
          sprintf("%04d", nframes), "}\n \\caption{", rel_fig_cap, ")} \\label{fig:",
          file.out.name, "}\n \\end{", islatexout.fig.out, "}\n",
          sep = ""
        )
      } else {
        cat("\\begin{", islatexout.fig.out, "}\n  \\animategraphics[method=icon, controls=step,width=\\linewidth]{12}{",
          file.path(rel_fig_path, file.out.name, file.out.name), "}{0001}{",
          sprintf("%04d", nframes), "}\n\\label{fig:",
          file.out.name, "}\n \\end{", islatexout.fig.out, "}\n",
          sep = ""
        )
      }
    } else {
      knitr::include_graphics(file.out)
    }
  } else {
    # animate(g1)
    ## !PROBLEM progress is activated in interactive() session
    ## thus, also save and load gif, as otherwise
    out_device <- "gif"
    file.out.name <- fig_name
    file.out <- file.path(rel_fig_path, file.out.name, paste0(file.out.name, ".", out_device))
    if (!dir.exists(file.path(rel_fig_path, file.out.name))) dir.create(file.path(rel_fig_path, file.out.name))
    nframes <- fig_frames
    nwidth <- fig_width
    nheigth <- fig_height
    invisible(
      anim_save(file.out, animate(g1,
        nframes = nframes,
        width = nwidth,
        height = nheigth,
        duration = 10
      ))
    )
    knitr::include_graphics(file.out)
  }
}



###############################
# Imageplot for ISIC categories (with option to create fig, save as file, or only read already created file)
###############################

unido_imageplot_isic <- function(df,
                                 byvartime = df$year,
                                 byvarisic = df$isic,
                                 byvargroup = df$ADesc,
                                 byvargroup2 = NULL,
                                 share = df$share,
                                 colorpal = c(unido_color_palette[[2]][2], "#FFFFFF"),
                                 legend_title = "Share per \nISIC and\nYear in %",
                                 x_lab_text = "Year",
                                 y_lab_text = "ISIC Rev. 4 divisions",
                                 axis.text.x.size = 8,
                                 axis.text.y.size = 8,
                                 fig_create = TRUE,
                                 fig_name = "test",
                                 out_device = "png",
                                 islatexout = FALSE,
                                 fig_fromfile = FALSE,
                                 fig_width = 9,
                                 fig_height = 6,...) {
  if (F) {
    df_groups <- unique(country[grep("IND|156|DEVX$", country$ACode), c("ADesc", "ACode")])
    # df_groups <- country[country$ACode %in% df_groups$ACode, ]
    # View(df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],])
    df <- merge(T50x51, df_groups, by.x = "acode", by.y = "ACode", all.y = TRUE)
    df <- df %>%
      group_by(year) %>%
      mutate(value_all = sum(value)) %>%
      ungroup() %>%
      mutate(share = round(value / value_all * 100, 1))
    # unique(df[ is.na(df$ADesc), c("acode", "desc")])
    rel_vars <- c("acode", "ADesc", "year", "isic", "share")
    rel_years <- seq(2000, 2019, 1)
    df1 <- df[, rel_vars][df$year %in% rel_years, ]
    # df1$ADesc <- gsub(" <.*>", "", df1$ADesc)
    # df1$ADesc = swr(df1$ADesc)
    df1$ADesc <- factor(df1$ADesc,
      levels =
        c(
          "Industrialized Economies",
          "China",
          "Developing and EIEs (excl.China)"
        )
    )
    n_lev <- length(unique(df$ADesc))
  }


  if (fig_create) {
    if (!is.null(byvargroup2)) {
      df_formatted <- df %>%
        mutate(byvartime = byvartime, byvargroup = byvargroup, byvargroup2 = byvargroup2, share = share)

      g1 <- ggplot(df_formatted, aes(x = byvartime, y = byvarisic), colour = "white") +
        geom_tile(aes(fill = share)) +
        scale_fill_gradient(low = colorpal[2], high = colorpal[1]) +
        facet_grid(byvargroup2 ~ byvargroup, scale = "free", labeller = label_wrap_gen(width = 16)) +
        scale_y_discrete(limits = rev) +
        theme(
          axis.text.x = element_text(size = axis.text.x.size, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = axis.text.y.size, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position = "right"
        ) +
        labs(x = x_lab_text, y = y_lab_text, fill = legend_title)
    } else {
      df_formatted <- df %>%
        mutate(byvartime = byvartime, byvargroup = byvargroup, share = share)

      g1 <- ggplot(df_formatted, aes(x = byvartime, y = byvarisic), colour = "white") +
        geom_tile(aes(fill = share)) +
        scale_fill_gradient(low = colorpal[2], high = colorpal[1]) +
        facet_grid(. ~ byvargroup, scale = "free", labeller = label_wrap_gen(width = 16)) +
        scale_y_discrete(limits = rev) +
        theme(
          axis.text.x = element_text(size = axis.text.x.size, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = axis.text.y.size, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position = "right"
        ) +
        labs(x = x_lab_text, y = y_lab_text, fill = legend_title)
    }
  }

  fig_writeread_fromfile_fxn(
    g1,
    fig_fromfile = fig_fromfile,
    fig_create = fig_create,
    rel_fig_path = rel_fig_path,
    fig_name = fig_name,
    out_device = out_device,
    fig_width = fig_width,
    fig_height = fig_height
  )
}


###############################
# Slopechart
###############################

unido_slopechart <- function(dataframe,
                             Times,
                             Measurement,
                             Grouping,
                             Data.label = NULL,
                             Title = "No title given",
                             SubTitle = "No subtitle given",
                             Caption = "No caption given",
                             XTextSize = 12,
                             YTextSize = 3,
                             TitleTextSize = 14,
                             SubTitleTextSize = 10,
                             CaptionTextSize = 8,
                             TitleJustify = "left",
                             SubTitleJustify = "left",
                             CaptionJustify = "right",
                             LineThickness = 1,
                             LineColor = "ByGroup",
                             DataTextSize = 2.5,
                             DataTextColor = "black",
                             DataLabelPadding = 0.05,
                             DataLabelLineSize = 0,
                             DataLabelFillColor = "white",
                             WiderLabels = FALSE,
                             ReverseYAxis = FALSE,
                             ReverseXAxis = FALSE,
                             RemoveMissing = TRUE,
                             ThemeChoice = "bw",

                             fig_create = TRUE,
                             fig_name = "test",
                             out_device = "png",
                             fig_fromfile = FALSE,
                             fig_width = 8,
                             fig_height = 5,
                             rel_fig_path = rel_fig_path) {
  if (F) {
    # https://www.r-bloggers.com/2018/06/creating-slopegraphs-with-r/
    # Install from CRAN
    # install.packages("CGPfunctions", repos = "http://cran.r-project.org")
    # Or the development version from GitHub
    # install.packages("devtools")
    # devtools::install_github("ibecav/CGPfunctions")
    # library(CGPfunctions)
    #############
    for (is_agg in c(F, T)) {
      if (is_agg) {
        rel_agg <- df_groups_common[grep("INDDEV1|INDDEV3|DEVXREG|INC$|INDREG", names(df_groups_common))]
      } else {
        rel_agg_g <- unique(country[country$Operator == "A", "ACode"][[1]]) # c("OECD", "ASEAN", "982W", "LDC")
        rel_agg <- lapply(
          rel_agg_g,
          function(x) country[country$Operator == "M" & country$ACode %in% country[country$ACode %in% c(x), ]$Ccode, "Ccode"][[1]]
        )
        names(rel_agg) <- rel_agg_g
      }

      if (is_agg == TRUE) rel_years_all <- list(seq(1990, 2020, 30), seq(1990, 2020, 15), seq(1990, 2020, 5)) else rel_years_all <- list(seq(1990, 2020, 30))

      for (j in c(1:length(rel_years_all))) {
        rel_years <- rel_years_all[[j]]

        WiderLabels <- TRUE
        fig_width <- 5
        fig_height <- 3

        if (length(rel_years) < 3) {
          WiderLabels <- FALSE
          fig_width <- 5
          fig_height <- 3
        }

        for (var in c("mvacod", "mvash", "mvapc")) {
          for (i in names(rel_agg)) {
            rel_fig_cap <- paste0(var, " development in ", i, "")
            if (var == "mvash") rel_subtitle <- "Share of MVA in GDP in per cent"
            if (var == "mvapc") rel_subtitle <- "in thsd. of USD (constant 2015 prices)"
            if (var %in% c("mvacod", "mvacud", "gdpcod", "gdpcud")) rel_subtitle <- "in thsd. of USD (constant 2015 prices)"
            # cat("\n### ", rel_fig_cap, "\n")
            if (is_agg) {
              df_groups <- data.frame(Ccode = c(rel_agg[[i]]))
              df <- merge(mvagdp[mvagdp$CompType == "A" & mvagdp$year %in% rel_years, ], df_groups, by.x = "acode", by.y = "Ccode", all.y = TRUE)
              df1 <- df # %>% filter(acode %in% rel_biggest)
              df1$year <- as.factor(df1$year)
              df1$value <- df1[, var]

              if (var == "mvacod") {
                df1$value <- round(df1$mvacod / 1000000, 1)
                rel_subtitle <- "in billions of USD (constant 2015 prices)"
              }
              df1$desc <- trimmer(df1$desc, 15)
            } else {
              if (var %in% c("mvacod", "mvacud", "gdpcod", "gdpcud")) next
              df_groups <- data.frame(Ccode = c(rel_agg[[i]]))
              # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]
              df <- merge(mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% rel_years, ], df_groups, by.x = "acode", by.y = "Ccode", all.y = TRUE)

              nbiggest <- 20

              rel_biggest <- df[df$year %in% rel_years[c(1, length(rel_years))], ] %>%
                group_by(year) %>%
                slice_max(mvacod, n = nbiggest) %>%
                select(acode) %>%
                .$acode
              df1 <- df %>% filter(acode %in% rel_biggest)
              df1$year <- as.factor(df1$year)
              df1$value <- df1[, var]

              if (length(rel_biggest) > 8) {
                fig_width <- 4
                fig_height <- 5
              }
            }

            unido_slopechart(
              dataframe = df1,
              Times = year,
              Measurement = value,
              Grouping = desc,
              Data.label = NULL,
              Title = NULL, # rel_fig_cap,
              SubTitle = NULL, # rel_subtitle,
              Caption = NULL,
              XTextSize = 8,
              YTextSize = 3,
              TitleTextSize = 11,
              SubTitleTextSize = 8,
              CaptionTextSize = 4,
              TitleJustify = "left",
              SubTitleJustify = "left",
              CaptionJustify = "right",
              LineThickness = 1,
              LineColor = unido_color_palette[[6]], # "ByGroup",
              DataTextSize = 2,
              DataTextColor = unido_color_palette[[1]], # "black",
              DataLabelPadding = 0.05,
              DataLabelLineSize = 0,
              DataLabelFillColor = "white",
              WiderLabels = WiderLabels,
              ReverseYAxis = FALSE,
              ReverseXAxis = FALSE,
              RemoveMissing = TRUE,
              ThemeChoice = "theme_unido",
              out_device = out_device_fig_prefer,
              fig_create = TRUE,
              fig_name = paste0("./misc/Slope_", var, "_", i, "_years_", length(rel_years)),
              fig_fromfile = TRUE,
              fig_width = fig_width,
              fig_height = fig_height
            )
          }
        }
      }
    }
  }
  if (fig_create) {

    ###############################
    if (ThemeChoice == "bw") {
      theme_set(theme_bw())
    }
    else if (ThemeChoice == "econ") {
      theme_set(ggthemes::theme_economist())
      if (DataLabelFillColor == "white") {
        DataLabelFillColor <- "#d5e4eb"
      }
    }
    else if (ThemeChoice == "wsj") {
      theme_set(ggthemes::theme_wsj())
      if (DataLabelFillColor == "white") {
        DataLabelFillColor <- "#f8f2e4"
      }
      TitleTextSize <- TitleTextSize - 1
      SubTitleTextSize <- SubTitleTextSize + 1
    }
    else if (ThemeChoice == "gdocs") {
      theme_set(ggthemes::theme_gdocs())
    }
    else if (ThemeChoice == "tufte") {
      theme_set(ggthemes::theme_tufte())
    }
    else if (ThemeChoice == "theme_unido") {
      theme_set(theme_unido())
      if (DataLabelFillColor == "white") {
        DataLabelFillColor <- "#d5e4eb"
      }
    } else {
      theme_set(theme_bw())
    }
    justifyme <- function(x) {
      if (x %in% c("l", "left", "L", "LEFT")) {
        return(0)
      }
      if (x %in% c("r", "right", "R", "RIGHT")) {
        return(1)
      }
      if (x %in% c("c", "center", "C", "CENTER")) {
        return(0.5)
      }
    }

    MySpecial <- list(
      scale_x_discrete(position = "top"),
      theme(legend.position = "none"),
      theme(panel.border = element_blank()),
      theme(panel.background = element_blank()),
      theme(axis.title.y = element_blank()),
      theme(axis.text.y = element_blank()),
      theme(panel.grid.major.y = element_blank()),
      theme(panel.grid.minor.y = element_blank()),
      theme(axis.title.x = element_blank()),
      theme(panel.grid.major.x = element_blank()),
      theme(axis.text.x.top = element_text(
        size = XTextSize,
        face = "bold",
        color = DataTextColor
      )),
      theme(axis.ticks = element_blank()),
      theme(plot.title = element_text(
        size = TitleTextSize, color = DataTextColor,
        face = "bold", hjust = justifyme(TitleJustify)
      )),
      theme(plot.subtitle = element_text(
        size = SubTitleTextSize, color = DataTextColor,
        hjust = justifyme(SubTitleJustify)
      )), theme(plot.caption = element_text(
        size = CaptionTextSize,
        hjust = justifyme(CaptionJustify)
      ))
    )
    if (length(match.call()) <= 4) {
      stop("Not enough arguments passed requires a dataframe, plus at least three variables")
    }
    argList <- as.list(match.call()[-1])
    if (!hasArg(dataframe)) {
      stop("You didn't specify a dataframe to use", call. = FALSE)
    }
    NTimes <- deparse(substitute(Times))
    NMeasurement <- deparse(substitute(Measurement))
    NGrouping <- deparse(substitute(Grouping))
    if (is.null(argList$Data.label)) {
      NData.label <- deparse(substitute(Measurement))
      Data.label <- argList$Measurement
    }
    else {
      NData.label <- deparse(substitute(Data.label))
    }
    Ndataframe <- argList$dataframe
    if (!is(dataframe, "data.frame")) {
      stop(paste0("'", Ndataframe, "' does not appear to be a data frame"))
    }
    if (!NTimes %in% names(dataframe)) {
      stop(paste0("'", NTimes, "' is not the name of a variable in the dataframe"),
        call. = FALSE
      )
    }
    if (anyNA(dataframe[[NTimes]])) {
      stop(paste0("'", NTimes, "' can not have missing data please remove those rows!"),
        call. = FALSE
      )
    }
    if (!NMeasurement %in% names(dataframe)) {
      stop(paste0("'", NMeasurement, "' is not the name of a variable in the dataframe"),
        call. = FALSE
      )
    }
    if (!NGrouping %in% names(dataframe)) {
      stop(paste0("'", NGrouping, "' is not the name of a variable in the dataframe"),
        call. = FALSE
      )
    }
    if (!NData.label %in% names(dataframe)) {
      stop(paste0("'", NData.label, "' is not the name of a variable in the dataframe"),
        call. = FALSE
      )
    }
    if (anyNA(dataframe[[NGrouping]])) {
      stop(paste0("'", NGrouping, "' can not have missing data please remove those rows!"),
        call. = FALSE
      )
    }
    if (!class(dataframe[[NMeasurement]]) %in% c(
      "integer",
      "numeric"
    )) {
      stop(paste0(
        "Sorry I need the measured variable '",
        NMeasurement, "' to be a number"
      ), call. = FALSE)
    }
    if (!"ordered" %in% class(dataframe[[NTimes]])) {
      if (!"character" %in% class(dataframe[[NTimes]])) {
        if ("factor" %in% class(dataframe[[NTimes]])) {
          message(paste0(
            "\nConverting '", NTimes,
            "' to an ordered factor\n"
          ))
          dataframe[[NTimes]] <- factor(dataframe[[NTimes]],
            ordered = TRUE
          )
        }
        else {
          stop(paste0(
            "Sorry I need the variable '",
            NTimes, "' to be of class character, factor or ordered"
          ),
          call. = FALSE
          )
        }
      }
    }
    Times <- enquo(Times)
    Measurement <- enquo(Measurement)
    Grouping <- enquo(Grouping)
    Data.label <- enquo(Data.label)
    if (ReverseXAxis) {
      dataframe[[NTimes]] <- forcats::fct_rev(dataframe[[NTimes]])
    }
    NumbOfLevels <- nlevels(factor(dataframe[[NTimes]]))
    if (WiderLabels) {
      MySpecial <- c(MySpecial, expand_limits(x = c(0, NumbOfLevels +
        1)))
    }
    if (ReverseYAxis) {
      MySpecial <- c(MySpecial, scale_y_reverse())
    }
    if (length(LineColor) > 1) {
      if (length(LineColor) < length(unique(dataframe[[NGrouping]]))) {
        message(paste0(
          "\nYou gave me ", length(LineColor),
          " colors I'm recycling colors because you have ",
          length(unique(dataframe[[NGrouping]])), " ",
          NGrouping, "s\n"
        ))
        LineColor <- rep(LineColor, length.out = length(unique(dataframe[[NGrouping]])))
      }
      LineGeom <- list(
        geom_line(aes_(color = Grouping), size = LineThickness),
        scale_color_manual(values = LineColor)
      )
    }
    else {
      if (LineColor == "ByGroup") {
        LineGeom <- list(geom_line(aes_(
          color = Grouping,
          alpha = 1
        ), size = LineThickness))
      }
      else {
        LineGeom <- list(geom_line(aes_(),
          size = LineThickness,
          color = LineColor
        ))
      }
    }
    if (anyNA(dataframe[[NMeasurement]])) {
      if (RemoveMissing) {
        dataframe <- dataframe %>%
          group_by(!!Grouping) %>%
          filter(!anyNA(!!Measurement)) %>%
          droplevels()
      }
      else {
        dataframe <- dataframe %>% filter(!is.na(!!Measurement))
      }
    }

    g1 <- dataframe %>% ggplot(aes_(
      group = Grouping, y = Measurement,
      x = Times
    )) +
      LineGeom +
      geom_text_repel(
        data = . %>%
          filter(!!Times == min(!!Times)), aes_(label = Grouping),
        hjust = "left", box.padding = 0.1, point.padding = 0.1,
        segment.color = "gray", segment.alpha = 0.6, fontface = "bold",
        color = DataTextColor,
        size = YTextSize, nudge_x = -1.95, direction = "y",
        force = 0.5, max.iter = 3000
      ) +
      geom_text_repel(
        data = . %>%
          filter(!!Times == max(!!Times)), aes_(label = Grouping),
        hjust = "right", box.padding = 0.1, point.padding = 0.1,
        segment.color = "gray", segment.alpha = 0.6, fontface = "bold",
        color = DataTextColor,
        size = YTextSize, nudge_x = 1.95, direction = "y",
        force = 0.5, max.iter = 3000
      ) +
      geom_label(aes_string(label = NData.label),
        size = DataTextSize, label.padding = unit(
          DataLabelPadding,
          "lines"
        ), label.size = DataLabelLineSize, color = DataTextColor,
        fill = DataLabelFillColor
      ) +
      MySpecial +
      labs(
        title = Title,
        subtitle = SubTitle, caption = Caption
      )
  }
  ###########################################################
 fig_writeread_fromfile_fxn(
    g1,
    fig_fromfile = fig_fromfile,
    fig_create = fig_create,
    rel_fig_path = rel_fig_path,
    fig_name = fig_name,
    out_device = out_device,
    fig_width = fig_width,
    fig_height = fig_height
  )
}


###############################
# slopechart combined with rankplot above
###############################
unido_slopechart1 <- function(df,
                              byvartime = df$year,
                              byvargroup = df$ADesc,
                              rankvar = df$mvacod,
                              nbiggest = 5,
                              share_show = TRUE,
                              label_show_round = 1,
                              colorpal = rep(unido_color_palette[[4]], 7),
                              subtitle = NULL,
                              caption = NULL,
                              rel_fig_cap = NULL,
                              WiderLabels = TRUE,
                              fig_create = TRUE,
                              fig_name = "test",
                              out_device = "png",
                              fig_fromfile = TRUE,
                              fig_width = 8,
                              fig_height = 5,
                              rel_fig_path = "./fig",
                              ...) {
  if (F) {
    df_groups <- country[country$Operator == "M", ]
    # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]

    df <- merge(mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% seq(1990, 2020, 5), ], df_groups, by.x = "acode", by.y = "Ccode", all = TRUE)
  }
  if (fig_create){
  df_formatted <- df %>%
    mutate(byvartime = byvartime, byvargroup = byvargroup, rankvar = rankvar, share_show = share_show) %>%
    group_by(byvartime) %>%
    mutate(rankvar = ifelse(share_show, rankvar / sum(rankvar) * 100, rankvar)) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(
      value = rankvar,
      # rank = floor(rank(-value)),
      rank = rank(-value),
      Value_rel = value / value[rank == 1],
      Value_lbl = paste0(" ", round(value, label_show_round))
    ) %>%
    group_by(byvargroup) %>%
    filter(rank <= nbiggest) %>%
    ungroup()
  df_formatted$byvartime <- as.factor(df_formatted$byvartime)
  df_formatted$value <- round(df_formatted$value, label_show_round)
}
  unido_slopechart(
    dataframe = df_formatted,
    Times = byvartime,
    Measurement = value,
    Grouping = byvargroup,
    Data.label = NULL,
    Title = rel_fig_cap,
    SubTitle = subtitle,
    Caption = caption,
    XTextSize = 11,
    YTextSize = 4,
    TitleTextSize = 11,
    SubTitleTextSize = 8,
    CaptionTextSize = 4,
    TitleJustify = "left",
    SubTitleJustify = "left",
    CaptionJustify = "right",
    LineThickness = 1.5,
    LineColor = colorpal, # "ByGroup",
    DataTextSize = 3,
    DataTextColor = unido_color_palette[[1]], # "black",
    DataLabelPadding = 0.05,
    DataLabelLineSize = 0,
    DataLabelFillColor = "white",
    WiderLabels = WiderLabels,
    ReverseYAxis = FALSE,
    ReverseXAxis = FALSE,
    RemoveMissing = TRUE,
    ThemeChoice = "theme_unido",
    fig_create = fig_create,
    fig_name = fig_name,
    out_device = out_device,
    fig_fromfile = fig_fromfile,
    fig_width = fig_width,
    fig_height = fig_height,
    rel_fig_path = rel_fig_path
  )
}

###############################
# boxplot
###############################

unido_boxplot <- function(df,
                          byvartime = df$year,
                          byvargroup = df$ADesc,
                          byvargroup2 = NULL,
                          value = df$mvash,
                          value_max = quantile(df[, var], c(0.9)),
                          value_min = quantile(df[, var], c(0.1)),
                          x_lab = "Years",
                          y_lab = "Share of MVA in GDP",
                          fig_create = TRUE,
                          fig_name = "test",
                          out_device = "png",
                          islatexout = FALSE,
                          fig_fromfile = FALSE,
                          fig_width = 9,
                          fig_height = 6,
                          rel_fig_path = rel_fig_path) {
  if (F) {
    rel_agg <- c("WOR", "IND", "DEV", "DEVX", "EIE", "EIEX", "LDC", "DEVOT")
    rel_years <- seq(1990, 2020, 10)

    for (var in c("mvash", "mvapc")) {
      for (i in rel_agg) {
        df_groups <- country[country$Operator == "M" & country$ACode %in% country[country$ACode %in% c(i), ]$Ccode, ]
        # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]
        df <- merge(mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% rel_years, ], df_groups, by.x = "acode", by.y = "Ccode", all.y = TRUE)
        if (var == "mvapc") {
          y_lab <- "MVA per capita"
          value_max <- 7000 # quantile(df[, var], c(1))
          value_min <- 0 # quantile(df[, var], c(0))
          if (i %in% c("LDC")) {
            value_max <- 400 # quantile(df[, var], c(0.95))
            value_min <- 0 # quantile(df[, var], c(0.05))
          }
        }
        if (var == "mvash") {
          y_lab <- "Share of MVA in GDP"
          value_max <- 30 # quantile(df[, var], c(1))
          value_min <- 0 # quantile(df[, var], c(0))
        }
        if (i == "WOR") {
          rel_g <- c("IND", "DEV")
          df$group2 <- country[country$Operator == "A" &
            country$ACode %in% rel_g, "ADesc"][[1]][match(
            df$acode,
            country[country$Operator == "A" &
              country$ACode %in% rel_g, "Ccode"][[1]]
          )]
          df$group2 <- factor(df$group2, levels = unique(df$group2)[c(2, 1)])
          value_max <- quantile(df[, var], c(0.99))
          value_min <- quantile(df[, var], c(0))
        }
        unido_boxplot(df,
          byvartime = df$year,
          byvargroup = df$ADesc,
          byvargroup2 = df$group2,
          value = df[, var],
          value_max = value_max,
          value_min = value_min,
          x_lab = NULL, # "Years",
          y_lab = y_lab,
          fig_create = TRUE,
          fig_name = paste0("./misc/boxplot_", var, "_", i),
          out_device = out_device_fig_prefer,
          islatexout = FALSE,
          fig_fromfile = T,
          fig_width = 9,
          fig_height = 6
        )
        cat("\n", var, "_", i, " done...", sep = "")
      }
    }
  }

  if (fig_create) {
    df_formatted <- df %>%
      mutate(byvartime = as.factor(byvartime), byvargroup = byvargroup, byvargroup2 = byvargroup2, value = value)

    maxval <- value_max
    minval <- value_min

    ddmin <- df_formatted %>%
      filter(value < minval) %>%
      group_by(byvartime) %>%
      summarise(outlier_txt = paste(byvargroup, collapse = "\n"))
    # if (nrow(ddmin) == 0) ddmin <- data.frame(byvartime = as.factor(unique(byvartime)), outlier_txt = "")

    ddmax <- df_formatted %>%
      filter(value > maxval) %>%
      group_by(byvartime) %>%
      summarise(outlier_txt = paste0(byvargroup, collapse = "\n"))
    # if (nrow(ddmax) == 0) ddmax <- data.frame(byvartime = as.factor(unique(byvartime)), outlier_txt = "")

    g1 <- df_formatted %>%
      ggplot(aes(x = byvartime, y = value, fill = byvargroup2)) +
      geom_violin(fill = unido_color_palette[[1]]) +
      geom_boxplot(
        width = 0.15, outlier.colour = "black", outlier.shape = 16,
        outlier.size = 1, notch = F
      ) +
      scale_fill_manual(values = unido_color_palette[[6]]) +
      scale_y_continuous(limits = c(minval, maxval)) # , labels = scales::percent) +

    if (is.null(byvargroup2)) {
      g1 <- g1 + geom_text_repel(aes(
        group = byvartime,
        label = ifelse(test = value > median(value) + 1.72 * IQR(value) | value < median(value) - 1.7 * IQR(value),
          yes = paste(byvargroup),
          no = ""
        )
      ),
      position = position_dodge(width = 0.75),
      hjust = -.2,
      size = 2.5
      )
      # coord_cartesian(ylim = c(-1, 1)) +
      # ggtitle(paste0("Boxplot: deviation between INDSTAT country VA and MVA for ISIC C")) +
      # coord_cartesian(ylim = c(-1, 1)) #+ scale_y_log10()
      if (nrow(ddmin) != 0) {
        g1 <- g1 + geom_text_repel(
          data = ddmin, aes(y = minval, label = outlier_txt),
          size = 2.5, vjust = 0.6, hjust = -0.15
        ) +
          geom_segment(
            data = ddmin, aes(
              y = minval * 0.96, yend = minval * 1.0,
              xend = factor(byvartime)
            ),
            arrow = arrow(length = unit(0.1, "cm"))
          )
      }
      if (nrow(ddmax) != 0) {
        g1 <- g1 + geom_text_repel(
          data = ddmax, aes(y = maxval, label = outlier_txt),
          size = 2.5, vjust = 0.6, hjust = -0.15
        ) +
          geom_segment(
            data = ddmax, aes(
              y = maxval * 0.96, yend = maxval * 1.0,
              xend = factor(byvartime)
            ),
            arrow = arrow(length = unit(0.1, "cm"))
          )
      }
    }
    if (!is.null(byvargroup2)) l.position <- "bottom" else l.position <- "none"
    g1 <- g1 + labs(y = y_lab, x = x_lab) +
      # geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen", size = 1.25) +
      theme_unido() +
      theme(
        axis.text.x = element_text(angle = 0, size = 12),
        axis.text.y = element_text(angle = 0, size = 12),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = l.position,
        plot.title = element_text(hjust = 0.5)
      )
  }

  fig_writeread_fromfile_fxn(
    g1,
    fig_fromfile = fig_fromfile,
    fig_create = fig_create,
    rel_fig_path = rel_fig_path,
    fig_name = fig_name,
    out_device = out_device,
    fig_width = fig_width,
    fig_height = fig_height
  )
}

############################################

###############################
# waffle
###############################
unido_waffle1 <- function(df1,
                          byvartime = df1$year,
                          timevarrange = c(1990, 2020),
                          parts = c("desc", "share"),
                          use_glyph = "industry",
                          glyph_size = 6,
                          glyph_rows = 5,
                          colorpal = unido_color_palette[[5]],
                          fig_create = TRUE,
                          fig_name = "test",
                          out_device = "png",
                          fig_fromfile = FALSE,
                          fig_width = 12,
                          fig_height = 8, ...) {
  if (F) {
    df_groups <- df_groups_common[grep("GROUP_CONTINENT$", names(df_groups_common))][[1]]
    rel_years <- c(1990, 2005, 2020)
    df <- mvagdp[mvagdp$CompType == "A" & mvagdp$year %in% rel_years & mvagdp$acode %in% df_groups, ]

    df1 <- df %>%
      group_by(year) %>%
      mutate(share = round(mvacod / sum(mvacod) * 100)) %>%
      ungroup()
    df1$desc <- gsub(" <World>", "", df1$desc)

    unido_waffle1(df1,
      byvartime = df1$year,
      timevarrange = rel_years,
      parts = c("desc", "share"),
      use_glyph = "industry",
      glyph_size = 7,
      glyph_rows = 4,
      colorpal = unido_color_palette[[5]],
      fig_create = TRUE,
      fig_name = "MVA_waffle_continents",
      out_device = out_device_fig_prefer,
      fig_fromfile = TRUE,
      fig_width = 900,
      fig_height = 600, ...
    )
  }


  if (fig_create) {
    df1 <- df1 %>%
      mutate(byvartime = byvartime)

    create_fig_com <- paste(
      "\nrequire(waffle)\nrequire(showtext)\nshowtext_auto()\n iron(",
      paste0("waffle(
          df1[df1$byvartime == ", timevarrange, ", c('", paste0(parts, collapse = "', '"), "')],
          colors = colorpal,
          use_glyph =  ", paste0("c('", paste0(use_glyph, collapse = "', '"), "')"), ",
          glyph_size = ", glyph_size, ",
          xlab = ", timevarrange, ",
          rows = ", glyph_rows, ",
          legend_pos = '", c(rep("none", length(timevarrange) - 1), "bottom"), "') + theme(
                                     legend.text = element_text(color=unido_visualID_color, face = 'bold'),
                                     axis.title = element_text(color=unido_visualID_color, face = 'bold')
                                     )",
        collapse = ", "
      ),
      ")\nshowtext_auto(FALSE)"
    )

    g1 <- eval(parse(text = create_fig_com))
  }

  if (fig_fromfile) {
    # out_device <- out_device
    file.out <- file.path(rel_fig_path, paste0(fig_name, ".", out_device))

    if (fig_create) {
      if (out_device == "pdf") {
        pdf(file.out, width = fig_width / 100, height = fig_height / 100)
        eval(parse(text = create_fig_com))
        invisible(dev.off())
      } else {
        # ggsave(last_plot(), filename = file.out, device = out_device, width = fig_width, height = fig_height)
        png(file.out, width = fig_width, height = fig_height)
        eval(parse(text = create_fig_com))
        invisible(dev.off())
      }
    }

    knitr::include_graphics(file.out)
  } else {
    print(g1)
  }
}

##########################

###############################
# echart4r
# (with option to create fig, save as file, or only read already created file)
###############################

unido_echart <- function(df1,
                         fig_create = TRUE,
                         fig_name = "test",
                         out_device = "png",
                         fig_fromfile = FALSE,
                         fig_width = 650,
                         fig_height = 500, ...) {
  if (F) {
    install.packages("echarts4r")
    library("echarts4r")

    # gender = data.frame(gender=c("Male", "Female"), value=c(65, 35),
    #                    path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13',
    #                             'path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z'))

    df_groups <- country[country$Operator == "M", ]
    # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]

    df <- merge(mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% c(1990, 2020), ], df_groups, by.x = "acode", by.y = "Ccode", all = TRUE)
    df$group <- ifelse(df$acode %in% c(156), df$desc, "World (excl. China)")
    df1 <- df %>%
      group_by(year, group) %>%
      summarise(value = sum(mvacod)) %>%
      ungroup() %>%
      group_by(year) %>%
      mutate(value = round(value / sum(value) * 100, 1))
    df1 <- df1[df1$group == "China", ]
    df1$group <- paste0(df1$group, " (", df1$year, ")")
    path_svg <- "path://M475.115 163.781L336 252.309v-68.28c0-18.916-20.931-30.399-36.885-20.248L160 252.309V56c0-13.255-10.745-24-24-24H24C10.745 32 0 42.745 0 56v400c0 13.255 10.745 24 24 24h464c13.255 0 24-10.745 24-24V184.029c0-18.917-20.931-30.399-36.885-20.248z"
    df1$path <- path_svg
    unido_echart(df1,
      fig_create = TRUE,
      fig_name = "CHINA_echart_mvacod_1990_2020",
      out_device = "png",
      fig_fromfile = TRUE,
      fig_width = 950,
      fig_height = 300
    )
  }
  if (fig_create) {
    g1 <- df1 %>%
      e_charts(group) %>%
      e_x_axis(
        splitLine = list(show = FALSE),
        axisTick = list(show = FALSE),
        axisLine = list(show = FALSE),
        axisLabel = list(show = FALSE)
      ) %>%
      e_y_axis(
        max = 100,
        splitLine = list(show = FALSE),
        axisTick = list(show = FALSE),
        axisLine = list(show = FALSE),
        axisLabel = list(show = FALSE)
      ) %>%
      e_color(color = c(unido_color_palette[[1]], "#eee")) %>%
      e_pictorial(value,
        symbol = path, z = 10, name = "realValue",
        symbolBoundingData = 100, symbolClip = TRUE
      ) %>%
      e_pictorial(value,
        symbol = path, name = "background",
        symbolBoundingData = 100
      ) %>%
      e_labels(
        position = "bottom", offset = c(0, 10),
        textStyle = list(
          fontSize = 20, fontFamily = "Arial",
          fontWeight = "bold",
          color = unido_color_palette[[1]]
        ),
        formatter = "{@[1]}% {@[0]}"
      ) %>%
      e_legend(show = FALSE) %>%
      e_theme("westeros")
  }

  if (fig_fromfile) {
    # out_device <- out_device
    file.out <- file.path(rel_fig_path, paste0(fig_name, ".", out_device))

    if (fig_create) {
      invisible(g1 %>%
        htmltools::html_print() %>%
        webshot2::webshot(file = file.out, delay = 0.5, vwidth = fig_width, vheight = fig_height, zoom = 4))
      invisible(gc())
      Sys.sleep(2)
    }

    knitr::include_graphics(file.out)
  } else {
    g1
  }
}

########################
###############################
# bubble chart
#
###############################

unido_bubble <- function(df1,
                         xvar = log(df1$mvapc),
                         yvar = df1$mvash,
                         sizevar = df1$pop,
                         fillvar = df1$continent,
                         labelvar = df1$label,
                         scale_size_range = c(0.1, 24),
                         xlab_text = "MVA per capita (log.)",
                         ylab_text = "MVA Share (MVA/GDP)",
                         sizevarlabel = "Population",
                         fillvarlabel = "Income Groups",
                         colorpal = unido_color_palette[[9]],
                         fig_create = TRUE,
                         fig_name = "test",
                         out_device = "png",
                         fig_fromfile = FALSE,
                         fig_width = 650,
                         fig_height = 500, ...) {
  if (F) {
    library(ggplot2)
    library(dplyr)
    library(hrbrthemes)
    library(viridis)

    rel_years <- c(2020)
    rel_agg <- df_groups_common[grep("GROUP_INC$", names(df_groups_common))][[1]]
    df_groups <- country[country$Operator == "A" & country$ACode %in% rel_agg, ]
    # df_groups[df_groups$Ccode %in% df_groups$Ccode[duplicated(df_groups$Ccode)],]
    df <- merge(mvagdp[mvagdp$CompType == "M" & mvagdp$year %in% rel_years, ], df_groups, by.x = "acode", by.y = "Ccode", all.y = TRUE)

    df$label <- ifelse(df$mvapc > 20000 | df$mvash > 40, df$CDesc, "")

    df1 <- df
    unido_bubble(df1,
      xvar = log(df1$mvapc),
      yvar = df1$mvash,
      sizevar = df1$pop / 1000,
      fillvar = df1$ADesc,
      labelvar = df1$label,
      scale_size_range = c(0.1, 24),
      xlab_text = "MVA per capita (log.)",
      ylab_text = "MVA Share (MVA/GDP)",
      sizevarlabel = "Population (M.)",
      fillvarlabel = "Income Groups",
      colorpal = unido_color_palette[[5]],
      fig_create = TRUE,
      fig_name = "test",
      out_device = "png",
      fig_fromfile = TRUE,
      fig_width = 9,
      fig_height = 5,
      rel_fig_path = rel_fig_path
    )
  }
  if (fig_create) {
    df_formatted <- df1 %>%
      mutate(xvar = xvar, yvar = yvar, sizevar = sizevar, fillvar = fillvar, labelvar = labelvar)
    g1 <- df_formatted %>%
      arrange(desc(sizevar)) %>%
      # mutate(labelvar = factor(labelvar, labelvar)) %>%
      ggplot(aes(x = xvar, y = yvar, size = sizevar, fill = fillvar)) +
      geom_point(alpha = 0.5, shape = 21, color = unido_color_palette[[1]]) +
      scale_size(range = scale_size_range, name = sizevarlabel) +
      scale_fill_manual(values = colorpal, name = fillvarlabel) +
      theme(legend.position = "none") +
      ylab(ylab_text) +
      xlab(xlab_text) +
      theme(legend.position = "right", axis.text.x = element_text(angle = 0)) +
      guides(fill = guide_legend(override.aes = list(size = 8))) +
      geom_text_repel(aes(label = labelvar), size = 4, color = unido_color_palette[[1]])
  }

  fig_writeread_fromfile_fxn(
    g1,
    fig_fromfile = fig_fromfile,
    fig_create = fig_create,
    rel_fig_path = rel_fig_path,
    fig_name = fig_name,
    out_device = out_device_fig_prefer,
    fig_width = fig_width,
    fig_height = fig_height
  )
}


###############################
# Read UNOCHA icon and convert color
#
###############################
read_UNOCHA_convert_color <- function(rel_file = "./fonts/UNOCHA/COVID-19.svg",
                                      rel_density = 1200,
                                      rel_color = unido_color_palette[[1]],
                                      rel_file_out = NULL) {
  fig_svg <- image_fill(
    image_transparent(magick::image_read(rel_file, density = rel_density), "white"),
    fuzz = 100,
    color = rel_color
  )

  # object.size(fig_svg)
  if (is.null(rel_file_out)) {
    plot(fig_svg)
  } else {
    image_write(fig_svg, path = rel_file_out)
  }
}

if (FALSE) {
  rel_file <- list.files(file.path("./fonts/UNOCHA"),
    pattern = "Analysis|Bacteria|Car|Computer|Country|Food|Fund|Innovation|Mobile-phone|Smartphone|Virus"
  )
  lapply(rel_file, function(x) {
    read_UNOCHA_convert_color(
      rel_file = file.path("./fonts/UNOCHA", x),
      rel_color = "maroon",
      rel_file_out = file.path(rel_img_path, paste0("_maroon_", x))
    )
  })
}
#
###############################
# Read Draw SVG
#
###############################
read_draw_svg <- function(rel_file = file.path(rel_img_path, "/COVID-19.svg"),
                          rel_density = 150,
                          rel_file_out = NULL) {
  fig_svg <- cowplot::ggdraw() +
    cowplot::draw_image(
      magick::image_read(rel_file, density = rel_density)
    )
  #   #object.size(fig_svg)
  #   if (is.null(rel_file_out)){
  fig_svg
  # plot(fig_svg)
  #   } else {
  #     ggsave(filename = rel_file_out, plot = fig_svg)
  #   }
}

###############################
# Read Draw SVG
#
###############################
readwrite_faicon_svg <- function(fa_icon = "chart-line",
                                 rel_file_out = NULL,
                                 rel_color = unido_color_palette[[1]]) {
  fig_svg <- cowplot::ggdraw() +
    cowplot::draw_image(
      magick::image_read(rsvg::rsvg(charToRaw(as.character(fa(fa_icon, fill = rel_color)))))
    )
  #   #object.size(fig_svg)
  if (is.null(rel_file_out)) {
    fig_svg
    # plot(fig_svg)
  } else {
    ggsave(filename = rel_file_out, plot = fig_svg)
  }
}
if (F) {
  lapply(c(
    "chart-line", "industry", "arrow-circle-up",
    "truck", "truck-loading", "ship",
    "shipping-fast", "balance-scale", "balance-scale-left",
    "balance-scale-right", "cog", "cogs",
    "hammer", "info", "info-circle"
  ), function(x) {
    readwrite_faicon_svg(
      fa_icon = x,
      rel_file_out = file.path(rel_img_path, paste0(x, ".svg"))
    )
  })
}
