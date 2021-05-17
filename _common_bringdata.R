########################################################################
# bring data
#
# Create MH::03/02/2021
########################################################################
# rm(list = ls())

save_or_load <- 2 # 1 for save (only if new data-raw available), 2 for load (standard)
#########################################################

#####################
#paths set in _common_start.R 
#rel_data_path <- "./data"
#rel_dataraw_path <- "./data-raw"
#YB.version = "YB2021"
#####################
#other relevant data paths 
abs_data_path <- rel_dataraw_path  #"G:/STATISTICS/CS-YB2021/Data"
abs_data_path_qiip <- rel_dataraw_path  #"G:/STATISTICS/IIP/Y2020/3_OUTPUT/QIIP/Y2020_Q4/RESULT"
abs_data_path_UQD <- rel_dataraw_path  #"S:/UQD/LookUp"

myfunctions_path <- rel_dataraw_path
#####################

# Lookup
# Read Common LookUps
unload_files <- "LookUp"
if (save_or_load == 1) {
  LookUp <- data.frame(
    short = gsub(".sas7bdat", "", list.files(abs_data_path_UQD , pattern = ".sas7bdat")),
    file = list.files(abs_data_path_UQD , pattern = ".sas7bdat"),
    full = list.files(abs_data_path_UQD , pattern = ".sas7bdat", full.names = TRUE),
    stringsAsFactors = FALSE
  )
  LookUp <- LookUp[grep("cntrycode|cntryaggr|ctabcode|utabcode|isic4xcode", LookUp$short), ]
  for (i in 1:nrow(LookUp)) {
    assign(LookUp$short[i], read_sas(LookUp$full[i]))
  }
  utabcode$Desc <- gsub("[[:punct:]]", "", utabcode$Desc)
  utabcode$Desc <- gsub("Number", "Nr", utabcode$Desc)
  utabcode$Desc <- gsub("Value added", "VA", utabcode$Desc)
  save(list = LookUp$short, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}
####
unload_files <- "country"
if (save_or_load == 1) {
  country <- readxl::read_excel(path = file.path(abs_data_path, 
                                                 paste0("Country", gsub("YB", "", YB.version), ".xls")), 
                                                 sheet = paste0("Country", gsub("YB", "", YB.version))) 
  ##filter duplicates
  country <- country[!duplicated(country), ]
  save(country, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
  #country[country$ADesc == "Northern America", "ADesc"] <- "Northern America <Industrialized>"
}

#########################################################
# read ISIC by Tech intensity
unload_files <- "isictech"
if (save_or_load == 1) {
  isictech <- readxl::read_xlsx(path = file.path(rel_dataraw_path, "isictech.xlsx")) 
  save(isictech, file = paste0(rel_data_path, "/", unload_files, ".rda"))
  
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}

#########################################################
# read ISIC R4
unload_files <- "isicR4"
if (save_or_load == 1) {
  isicR4_all <- read_csv(file = file.path(rel_dataraw_path, "ISIC_Rev_4_english_structure.txt")) 
  isicR4_all$lvl <- nchar(isicR4_all$Code)
  isicR4 <- isicR4_all[ substr(isicR4_all$Code,1,2) %in% c("C", 10:33),]
  # Apply texts from original ISIC class to UNIDO used isic4xcode
  tmp <- isic4xcode[isic4xcode$COMPTYPE == "M",]
  tmp1 <- tmp[ grep(".{3}0$",tmp$ACode ),]
  tmp1$ACode <- gsub("0$", "", tmp1$ACode)
  tmp2 <- tmp[ grep("^11$|^17$",tmp$ACode ),]
  tmp2$ACode <- paste0(tmp2$ACode, "0")
  shorttext = rbind(
    tmp, tmp1, tmp2
    )
  isicR4$ShortDesc <- shorttext$Desc[ match(isicR4$Code, shorttext$ACode)]
  save(isicR4, isicR4_all, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}

#########################################################
# Read  T50x51 

unload_files <- "T50x51"
if (save_or_load == 1) {
  T50x51 <- readxl::read_xlsx(path = file.path(abs_data_path, "T50x51EXP2021.xlsx"), sheet = YB.version) 
  save(T50x51, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}

#########################################################
# Read MVAGP

unload_files <- "mvagdp"
if (save_or_load == 1) {
  mvagdp <- readxl::read_xlsx(path = file.path(abs_data_path, "MVAGDPYB2020ext.XLSX"), sheet = paste0(YB.version, "ext")) 
  save(mvagdp, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}

#########################################################
# Read QIIP

unload_files <- "qiip"
if (save_or_load == 1) {
  qiip <- haven::read_sas(data_file  = file.path(abs_data_path_qiip, "output_all.sas7bdat"))
  qiip <- qiip[ qiip$variable %in% c("GR_Y", "value") & 
                  #qiip$isic == "C" & 
                  qiip$ctable == "51",]
  save(qiip, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}
#########################################################
# read SDG9
unload_files <- "sdg9"
if (save_or_load == 1) {
  tmp <- list.files(path = rel_dataraw_path, pattern = "^9.*-2020-02-14.csv$")
  
  sdg9 <- bind_rows(lapply(tmp, function(x) read_csv(file = file.path(rel_dataraw_path, x),
                                                     col_types = cols(
                                                       Series.Code = col_character(),
                                                       Series.Name = col_character(),
                                                       Indicator.Reference.Number = col_character(),
                                                       Reference.Area.Code..M49. = col_character(),
                                                       Reference.Area.Type = col_character(),
                                                       Reference.Area.Name = col_character(),
                                                       Time.Period = col_character(),
                                                       Observation.Value = col_character(),
                                                       Unit.of.Measurement = col_character(),
                                                       Nature.of.Data = col_character(),
                                                       Footnotes = col_character(),
                                                       Source.Detail = col_character(),
                                                       Time.Detail = col_character()
                                                     )
                                                     )) )
  #sdg9$Observation.Value[ which(sdg9$Observation.Value == "..")] <- NA
  sdg9$Observation.Value <- as.numeric(sdg9$Observation.Value)
  sdg9$Reference.Area.Code..M49. <- sprintf("%03d", as.numeric(sdg9$Reference.Area.Code..M49.))
  save(sdg9, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}

#########################################################
# Export data by tech
unload_files <- "exporttech"
if (save_or_load == 1) {
  exporttech <- readxl::read_xlsx(path = file.path(rel_dataraw_path, "Export data by tech.xlsx"), 
                                  sheet = "Exports",
                                  range = "A5:AE2441") 
  exporttech <- pivot_longer(exporttech, grep("\\d{4}", names(exporttech), value = TRUE), names_to = "year")
  #table(which(is.na(exporttech$class)) == grep("Total", exporttech$country2))
  exporttech[grep("Total", exporttech$country2), "class"] <- "Total"
  
  exporttech.labels.det <- readxl::read_xlsx(path = file.path(rel_dataraw_path, "Export data by tech.xlsx"), 
                                             sheet = "tech_class CIP",
                                             range = "A1:D262") 
  exporttech.labels.SITCrev3 <- readxl::read_xlsx(path = file.path(rel_dataraw_path, "Export data by tech.xlsx"), 
                                                  sheet = "tech_class CIP",
                                                  range = "J2:K13") 
  colnames(exporttech.labels.SITCrev3) <- c("Code", "Desc")
  exporttech.labels.SITCrev3$Desc.ext <- c('Primary', 
                                           'Resource based 1: agro based', 
                                           'Resource based 2: other RB', 
                                           'Low technology 1: fashion cluster', 
                                           'Low technology 2: other LT', 
                                           'Medium Technology 1: automotive', 
                                           'Medium Technology 2: process', 
                                           'Medium Technology 3: engineering', 
                                           'High Technology 1: electrical and electronics', 
                                           'High Technology 2: other HT', 
                                           'Other transactions'
  )
  exporttech.labels.SITCrev3$Desc.ext.det <- c('primary', 
                                               'agro based', 
                                               'other resource based', 
                                               'fashion cluster', 
                                               'other low technology', 
                                               'automotive', 
                                               'process', 
                                               'engineering', 
                                               'electrical and electronics', 
                                               'other high technology', 
                                               'other transactions'
  )
  exporttech.labels.SITCrev3$Desc.tech <- c('Primary', 
                                            'Resource Based', 
                                            'Resource Based', 
                                            'Low Technology', 
                                            'Low Technology', 
                                            'Medium Technology', 
                                            'Medium Technology', 
                                            'Medium Technology', 
                                            'High Technology', 
                                            'High Technology', 
                                            'Other Transactions'
  )
  exporttech.labels.SITCrev3$Desc.abbr <- c('Primary', 
                                            'RB', 
                                            'RB', 
                                            'LT', 
                                            'LT', 
                                            'MT', 
                                            'MT', 
                                            'MT', 
                                            'HT', 
                                            'HT', 
                                            'Other'
  )
  save(exporttech, exporttech.labels.det, exporttech.labels.SITCrev3, file = paste0(rel_data_path, "/", unload_files, ".rda"))
  
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}
#########################################################
# CD data
if (F){
unload_files <- "Inst32"
if (save_or_load == 1) {
  source(paste0(myfunctions_path, "/INDSTAT_CD_read.R"))
  INDSTAT_CD_read(vers = unload_files, rel.year = rel.year)
  save(list = ls(pattern = unload_files), file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}


unload_files <- "Inst4x"
if (save_or_load == 1) {
  source(paste0(myfunctions_path, "/INDSTAT_CD_read.R"))
  INDSTAT_CD_read(vers = unload_files, rel.year = rel.year)
  save(list = ls(pattern = unload_files), file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}
}
#########################################################
#########################################################
# shapefile for maps
unload_files <- "shapefile"
if (save_or_load == 1) {
  source(paste0(myfunctions_path, "/dl_imp_esri_shapefile.R"))
  dl_imp_esri_shapefile( 
    returnfile = unload_files,
    dl = FALSE,
    dl_path = "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download",
    product_spec1 = "50m/cultural",
    product_spec2 = "ne_50m_admin_0_countries",
    exdir = paste0(rel_dataraw_path, "/", unload_files)
)  
save(list = unload_files, file = paste0(rel_data_path, "/", unload_files, ".rda"))
} else {
  load(paste0(rel_data_path, "/", unload_files, ".rda"))
}

# shapefile %>% st_drop_geometry() %>% distinct(., ISO_N3) %>% pull()
# shapefile %>% st_drop_geometry() %>% filter(ISO_N3 =="-99") %>% #select(.,c("ISO_A3", "NAME_EN", "UN_A3","WB_A3"))

# Norway ISO -99 change
shapefile[shapefile$NAME_EN == "Norway", ]$ISO_N3 <- "578"
shapefile[shapefile$NAME_EN == "Norway", ]$ISO_A3 <- "NOR"
shapefile[shapefile$NAME_EN == "Panama", ]$ISO_N3 <- "590"
shapefile[shapefile$NAME_EN == "France", ]$ISO_A3 <- "FRA"
##!!!!!!!!Tuvalu 798 is not on shapefile!!!!!!!!!!!!!!!!!!
#View(shapefile%>% st_drop_geometry())

#calculate centers
centers <- shapefile %>% st_centroid() %>% st_coordinates() %>% data.frame()
colnames(centers) <- c("center_x", "center_y")
shapefile$center_x <- centers$center_x
shapefile$center_y <- centers$center_y

country$ISO_N3 <- ifelse(country$Operator == "M", country$Ccode, NA)
unido_shapefile_all <- merge(
  shapefile[, c("ISO_A3","ISO_N3", "NAME_EN", "CONTINENT", "REGION_UN", "center_x", "center_y")], 
                         country[country$Operator == "M", c("ISO_N3", "Ccode", "CDesc")], 
                         by = "ISO_N3", 
                         all.x = TRUE)
unido_shapefile <- unido_shapefile_all[ !is.na(unido_shapefile_all$Ccode),]
country$ISO_N3 <- NULL

###
df_groups <- list(
  GROUP_WORLD = c("WOR"),
  GROUP_CONTINENT = c("937", "957W", "962W", "982W", "EURW"),
  GROUP_IND = c("IND"),
  GROUP_DEV = c("DEV"),
  GROUP_DEV1 = c("156", "DEVX"),
  GROUP_INDDEV1 = c("IND","DEV"),
  GROUP_INDDEV2 = c("IND", "156", "DEVX"),
  GROUP_INDDEV3 = c("IND", "EIE", "DEVOT", "LDC"),
  GROUP_INDDEV4 = c("IND", "156", "EIEX", "DEVOT", "LDC"),
  GROUP_INC = c("907", "909", "910", "911"),
  GROUP_INC1 = c("907", "156", "908", "910"),
  GROUP_LDC = c("LDC"),
  GROUP_EUROPE = c("EURW"),
  GROUP_ASIAPACIFIC = c("962W"),
  GROUP_AFRICA = c("982W"),
  GROUP_NAMERICA = c("937"),
  GROUP_LAMERICA = c("957W"),
  GROUP_OECD = c("OECD"),
  GROUP_BRICS = c("BRICS"),
  GROUP_ASEAN = c("ASEAN"),
  GROUP_SAARC = c("SAARC"),
  GROUP_SADC = c("SADC"),
  GROUP_MENA = c("MENA"),
  GROUP_MERCOS = c("MERCOS"),
  GROUP_ECOWAS = c("ECOWAS"),
  GROUP_WAEMU = c("WAEMU"),
  GROUP_CARICOM = c("967"),
  GROUP_LDCSA = c("LDCSA"),
  GROUP_INDREG = c("EURI", "ESEAI", "WAI", "937", "972"),
  GROUP_INDREG1 = c("EURI", "ESEAI", "937"),
  GROUP_INDREG2 = c("WAI", "972"),
  GROUP_DEVREG = c("982", "962", "EURD", "957"),
  GROUP_DEVXREG = c("982", "900", "EURD", "957"),
  GROUP_DEVXREG1 = c("982"),
  GROUP_DEVXREG2 = c("957"),
  GROUP_DEVXREG3 = c("900"),
  GROUP_DEVXREG4 = c("EURD")
  
)
#sAVE FOR USE THROUGHOUT DOC
df_groups_common <- df_groups
###

###################################
if (save_or_load == 1 & FALSE) {
###################################
  #source("./_common_visual_id.R")
# Create worlmap for possible use as title image and as margin figures
#  View(unique(country[country$Operator != "M", c("ACode", "ADesc")]))
  

unido_shape <- unido_shapefile_all
for (i in c(names(df_groups_common))) {
  df <- country[country$ACode %in% df_groups_common[[i]],]
  eval(parse(text = paste0("unido_shape$", i, "code <- df$ACode[ match(unido_shape$Ccode, df$Ccode)]")))
  eval(parse(text = paste0("unido_shape$", i, "<- df$ADesc[ match(unido_shape$Ccode, df$Ccode)]")))
}

#########
require(ggplot2)
require(cowplot)   # for theme_minimal_grid()
require(sf)        # for manipulation of simple features objects
require(lwgeom)

#################################
#Version 1 interrrupted goode homolosine
#################################
#goode projeciton

crs_goode <- "+proj=igh"
# projection outline in long-lat coordinates
lats <- c(
  90:-90, # right side down
  -90:0, 0:-90, # third cut bottom
  -90:0, 0:-90, # second cut bottom
  -90:0, 0:-90, # first cut bottom
  -90:90, # left side up
  90:0, 0:90, # cut top
  90 # close
)
longs <- c(
  rep(180, 181), # right side down
  rep(c(80.01, 79.99), each = 91), # third cut bottom
  rep(c(-19.99, -20.01), each = 91), # second cut bottom
  rep(c(-99.99, -100.01), each = 91), # first cut bottom
  rep(-180, 181), # left side up
  rep(c(-40.01, -39.99), each = 91), # cut top
  180 # close
)
goode_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )

# now we need to work in transformed coordinates, not in long-lat coordinates
goode_outline <- st_transform(goode_outline, crs = crs_goode)
# get the bounding box in transformed coordinates and expand by 10%
xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1
# turn into enclosing rectangle
goode_encl_rect <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = crs_goode)
# calculate the area outside the earth outline as the difference
# between the enclosing rectangle and the earth outline
goode_without <- st_difference(goode_encl_rect, goode_outline)
###################


#for (i in c(names(df_groups))) {
for (i in c("GROUP_WORLD")) {
  df <- unido_shape
  df$ADesc <- as.factor(st_drop_geometry(df)[,i])
  n_lev <- length(unique(st_drop_geometry(df)$ADesc))
  g1 <- ggplot(data = df, aes(fill = ADesc)) + 
    geom_sf(aes(fill = ADesc), color = unido_visualID_color, size = 0.5/.pt) +
    scale_fill_manual(values = unido_color_palette[[n_lev-1]], guide = "none") +
    geom_sf(data = goode_without, fill = "white", color = "NA") +
    geom_sf(data = goode_outline, fill = NA, color = unido_visualID_color, size = 0.5/.pt) +
    coord_sf(crs = crs_goode, xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE) +
    theme_minimal_grid() +
    theme(
      panel.background = element_rect(fill = "white", color = "white", size = 1),
      #panel.grid.major = element_line(color = "gray30", size = 0.25),
      panel.grid.major = element_line(color = unido_visualID_color, size = 0.25),
      legend.position = 'none',
      plot.margin = unit(c(-4, 0, -4, 0), "cm")
    )  
  rel_image1 <- file.path(rel_img_path, "_maroon_Analysis.svg")
  rel_image2 <- file.path(rel_img_path, "_gray_Analysis.svg")
  g1 <- g1 + geom_image(data = data.frame(
    #proj_x= -16000000, 
    #proj_y= -2000000, 
    proj_x= -10000000, 
    proj_y= 1000000, 
    image = "", 
    ADesc = "World"),
    aes(x = proj_x, y = proj_y, fill = ADesc),
    size = 0.3,
    image = rel_image2
  ) + geom_image(data = data.frame(
    proj_x= 14000000, 
    proj_y= -2000000, 
    #proj_x= 0000000, 
    #proj_y= 0000000, 
    image = "", 
    ADesc = "World"),
    aes(x = proj_x, y = proj_y, fill = ADesc),
    size = 0.3,
    image = rel_image2
  ) +  geom_image(data = data.frame(
    proj_x= 4000000, 
    proj_y= 2500000, 
    #proj_x= 0000000, 
    #proj_y= 0000000, 
    image = "", 
    ADesc = "World"),
    aes(x = proj_x, y = proj_y, fill = ADesc),
    size = 0.45,
    image = rel_image1
  ) + geom_image(data = data.frame(
    proj_x= -19000000, 
    proj_y= -1500000, 
    #proj_x= 0000000, 
    #proj_y= 0000000, 
    image = "", 
    ADesc = "World"),
    aes(x = proj_x, y = proj_y, fill = ADesc),
    size = 0.1,
    image = rel_image1
  ) +theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) 
   
  out_device <- "pdf"
  file.out <- file.path(file.path(getwd(), "img"), paste0(i, ".", out_device))
  
  if(out_device == "pdf") {
    ggsave(g1, filename = file.out, device = cairo_pdf,width = 4, height = 2)
  } else {
    ggsave(g1, filename = file.out, device = out_device,width = 4, height = 2)
  }
}

#################################
#Version 2 for margin figures
#################################

crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"
grat_wintri <- 
  st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  st_transform_proj(crs = crs_wintri)
# vectors of latitudes and longitudes that go once around the 
# globe in 1-degree steps
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  st_transform_proj(crs = crs_wintri) # transform to Winkel tripel

for (i in c(names(df_groups))) {
#for (i in c("GROUP_INDDEV1")) {
  df <- st_transform_proj(unido_shape, crs = crs_wintri)
  df$ADesc <- as.factor(st_drop_geometry(df)[,i])
  n_lev <- length(unique(st_drop_geometry(df)$ADesc))
  

g1 <- ggplot() + 
  
  geom_sf(data = wintri_outline, 
          fill = unido_visualID_color,  #NA, 
          color = NA) +
  geom_sf(data = grat_wintri, 
          color = "gray30", #  unido_visualID_color, 
          size = 0.25/.pt) + 
  
  geom_sf(
    data = df,
    aes(fill = ADesc), 
    color = "gray30",  #unido_visualID_color, 
    size = 0.5/.pt
  ) +
  scale_fill_manual(
    values = unido_color_palette[[n_lev]], #unido_color_palette[[n_lev-1]], 
    guide = "none") +
  geom_sf(data = wintri_outline, 
          fill = NA, 
          color = "gray30", #unido_visualID_color,
          size = 0.5/.pt) +
  coord_sf(datum = NULL) +
  theme_map() +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0, -0.5, 0, -0.5), "cm")
  )

out_device <- "png"
file.out <- file.path(file.path(getwd(), "img"), paste0(i, "wintri.", out_device))

if(out_device == "pdf") {
  ggsave(g1, filename = file.out, device = cairo_pdf,width = 3, height = 2)
} else {
  ggsave(g1, filename = file.out, device = out_device,width = 3, height = 2)
}
}
###################
rm(list = c("unido_shape", "df"))
}