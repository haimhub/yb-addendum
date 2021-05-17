########################################################################
# Define overall parameters, ... for use in whole book
#
# Create MH::03/02/2021
########################################################################

#
#rm(list = ls())
###############################
#libraries
###############################
source("./_common_libraries.R")
###############################
###############################
# set options
getOutputFormat <- function() {
  output <- rmarkdown:::parse_yaml_front_matter(
    readLines(knitr::current_input())
  )$output
  if (is.list(output)){
    return(names(output)[1])
  } else {
    return(output[1])
  }
}

islatexout <- knitr::is_latex_output()

#if (exists("show_kable_f", where = params)) show_kable_f <- params$show_kable_f else show_kable_f <- "html" 
if (exists("islatexout")) {
  if (islatexout) show_kable_f <- "latex" else show_kable_f <- "html"
} else show_kable_f <- "html"
   
options(scipen = 999, digits = 3, 
        width = 72, 
        formatR.indent = 2,
        knitr.kable.NA = "", 
        #kableExtra.auto_format = TRUE, 
        kable.force.latex = TRUE,
        knitr.include_graphics.ext = TRUE,
        knitr.graphics.auto_pdf = FALSE
        )

knitr::opts_chunk$set(
  comment = "#>",
  #collapse = TRUE,
  cache = FALSE,
  #width = 72,
  out.width = "100%",
  #fig.align = 'center',
  #fig.width = 6,
  #fig.height = 3.708,  # width * 1 / phi
  fig.show = "hold"
  )

#knitr::opts_chunk$set(fig.pos = 'htbp')
###############################
###############################
# relevant paths 
rel_img_path <- "./img"
rel_fig_path <- "./fig"
rel_data_path <- "./data"
rel_dataraw_path <- "./data-raw"

if (!dir.exists(rel_fig_path)) dir.create(rel_fig_path)

###############################
###############################
# Define relevant individual parameters and funciton
YB.version = "YB2021"
#Define if to include static images form from ./fig

for (i in c("02", "03", "04", "05","06","07", "99")) {
  if (exists(paste0("fig_create_", i), where = params)) {
    assign(paste0("fig_create_", i), params[[paste0("fig_create_", i)]])
  } else {
    assign(paste0("fig_create_", i), FALSE)
  }
  if (exists(paste0("fig_fromfile_", i), where = params)) {
    assign(paste0("fig_fromfile_", i), params[[paste0("fig_fromfile_", i)]])
  } else {
    assign(paste0("fig_fromfile_", i), FALSE)
  }
  
}
i <- ""
if (exists(paste0("out_device_fig_prefer", i), where = params)) {
  assign(paste0("out_device_fig_prefer", i), params[[paste0("out_device_fig_prefer", i)]])
} else {
  assign(paste0("out_device_fig_prefer", i), "png")
}

###############################
###############################
#Visual identity
source("./_common_visual_id.R")
###############################
###############################
#common functions
source("./_common_fxn.R")

#kable tables
tab_size <- 10
if (show_kable_f == "latex") {
  kable_unido <- function(data, caption = "Test", t_envir = "table", ...) {
    knitr::kable(data, 
                 format = "latex",
                 caption = caption, 
                 booktabs = TRUE, 
                 digits = 2, 
                 linesep = "" 
                 ,table.envir= t_envir
    ) %>% 
      kable_styling(latex_options = c("hold_position"), 
                    font_size = tab_size, 
                    full_width=FALSE)
  }
} else {
  kable_unido <- function(data, caption = "Test", ...) {
    knitr::kable(data, 
                 format = "html",
                 caption = caption, 
                 booktabs = TRUE, 
                 digits = 2, 
                 linesep = "") %>% 
      kable_styling(font_size = tab_size, 
                    full_width=TRUE)
  }
}

# store session information
s.information <- sessionInfo()
########################################################################

###############################
###############################
#load_basic_data SET variable in bring...R to 2!!
source("./_common_bringdata.R")
###############################
###############################

