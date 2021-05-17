########################################################################
# Define parameters for visual identiy
#
# Create MH::03/02/2021
########################################################################

################################
#VISUAL identity
#https://intranet.unido.org/intranet/images/5/50/UNIDO_VISUAL_MANUAL_2017.pdf
#The primary colour used for UNIDO publications should be blue.
#CMYK C89 M18 Y0 K0
#PANTONE 2192C
#RGB Ro G156 B220

unido_visualID_color <- rgb(red = 0, green = 156, blue = 220, max = 255)

#find complementary color
#colortools::complementary(unido_visualID_color, plot = T) or https://planetcalc.com/7661/
#colortools::splitComp(unido_visualID_color, plot = T)
#colortools::tetradic(unido_visualID_color, plot = T)
#colortools::square(unido_visualID_color, plot = T)

unido_visualID_suppl_color <-  "#DC4000" #"#ff6323"
#unido_visualID_suppl_color2 <-  c("#DC002E", "#DCAE00")
#unido_visualID_suppl_color3 <- c("#4000DC", "#DC4000", "#9CDC00")
###############################
#DEFINE colorpalette to use
###############################
# RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL, colorblindFriendly = TRUE)
# RColorBrewer::brewer.pal(11,"RdYlBu")
# RColorBrewer::display.brewer.pal(9,"RdYlBu")
# cDev_palette<-colorFactor(unido_color_palette[[7]], unido_shape$ADesc)
unido_color_ramp_disc <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                           "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#unido_color_ramp_disc <- NULL

unido_color_ramp <- colorRampPalette(c(unido_visualID_color,
                                       #unido_color_ramp_disc, 
                                       unido_visualID_suppl_color))  
#barplot(1:4,col = unido_color_ramp(4))

unido_color_palette <- lapply(c(1:9), function(x) {
  if (x == 1) unido_visualID_color 
  else if (x == 2) rev(unido_color_ramp(x))
  #else if (x == 3) rev(unido_color_ramp(x))
  #else if (x == 4) rev(unido_color_ramp(x))
  else unido_color_ramp_disc[1:x] 
  #else viridis_pal()(x) 
  #else rev(unido_color_ramp(x))
  #else rev(RColorBrewer::brewer.pal(x,"RdYlBu"))
  #else RColorBrewer::brewer.pal(x+1,"PuBu")[-1]
}
)

###############################
#create ggplot theme
###############################
#library(extrafont)
##ICONS
extrafont::font_import(path = "./fonts", pattern = "fa-", prompt = FALSE)
extrafont::loadfonts(device = "win")

extrafont::fonttable() %>%
  dplyr::as_tibble() %>%
  dplyr::filter(grepl("Awesom", FamilyName)) %>%
  select(FamilyName, FontName, fontfile)
font_add(family = "FontAwesome5Free-Solid", regular = "./fonts/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "./fonts/fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", regular = "./fonts/fa-brands-400.ttf")
#####

#extrafont::font_import(paths = "./meta-pro-cufonfonts/",prompt = F, pattern = "FFMetaProCond")
# error when using family  
#In grid.Call(C_textBounds, as.graphicsAnnot(x$label),  ... :
#Zeichensatzfamilie in der Windows Zeichensatzdatenbank nicht gefunden

theme_unido <- function(base_size = 18, 
                        base_family = 'sans',  #'Meta Pro Cond' # 'Times New Roman', 
                        textcolor = unido_visualID_color, 
                        linecolor = unido_visualID_color) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #text = element_text(#family = "Meta Pro Cond", 
      #                    colour=textcolor),
      plot.title = element_text(
                                size = rel(1), face = "bold", 
                                margin = margin(0,0,5,0), hjust = 0.5, colour = textcolor),
      plot.subtitle = element_text(size = rel(1), hjust = 0.5, face = "italic", colour = textcolor),
      plot.caption = element_text(size = rel(1), hjust = 0.5, face = "italic", colour = textcolor),
      
      # add border 1)
      panel.border = element_rect(colour = linecolor, fill = NA, linetype = 2),
      # color background 2)
      panel.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(colour = NA, fill=NA),
      strip.text = element_text(size = rel(0.7), face = "bold", colour=textcolor),
      # modify grid 3)
      panel.grid.major.x = element_line(colour = linecolor, linetype = 3, size = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y =  element_line(colour = linecolor, linetype = 3, size = 0.5),
      panel.grid.minor.y = element_blank(),
      # modify text, axis and colour 4) and 5)
      axis.text = element_text(colour = textcolor
                               , face = "bold"),
      axis.title = element_text(colour = textcolor),
      axis.ticks = element_line(colour = linecolor),
      axis.text.x = element_text(size = rel(0.6), 
                                 #angle = 45, 
                                 vjust = 1, 
                                 hjust = 1),
      axis.text.y = element_text(size = rel(0.7)),
      # legend at the bottom 6)
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.7), 
                                  face = "bold", 
                                  colour = textcolor),
      legend.text = element_text(size = rel(0.6), 
                                 face = "bold", 
                                 colour = textcolor),
      #legend.key = element_rect(fill = "transparent", colour = linecolor),
      legend.key=element_blank(),
      legend.key.size = unit(1.5, "lines")
    )
}

theme_set(theme_unido())
update_geom_defaults("line", list(size = 1.25))
update_geom_defaults("point", list(size = 2, colour = unido_color_palette[[1]]))
update_geom_defaults("bar", list(size = 0.05, fill = unido_color_palette[[1]], colour = "gray30"))
################################


