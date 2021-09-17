##################################################################################
### Script description
##################################################################################

# Random sampling of spatial points per DeSO




##################################################################################
### Clean start
##################################################################################

rm(list = ls())
gc()


##################################################################################
### Libraries etc
##################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp, mapview)



# avoid scientific notation
options(scipen=999)


##################################################################################
### Functions
##################################################################################

`%notin%` <- Negate(`%in%`)


# download and unzip shapefiles from Github unless done previously
get_shapefile <- function(path){
  file_name = str_extract(url_shp, "[^/]+(?=\\.zip$)")
  file_name_full = paste0(file_name, ".zip")
  
  if(!file.exists(paste0(folder_shapefile, "/", file_name_full))){
    file_name = str_extract(url_shp, "[^/]+(?=\\.zip$)")
    file_name_full = paste0(file_name, ".zip")
    download.file(url_shp, destfile = paste0(folder_shapefile, "/", file_name_full))
    fname = unzip(paste0(folder_shapefile, "/", file_name_full), list=TRUE)$Name[1:5]
    unzip(paste0(folder_shapefile, "/", file_name_full), 
          exdir=folder_shapefile, 
          overwrite=TRUE)
  }
  file_name <<- file_name
}





##################################################################################
### Define paths and create data directories (if not already exist)
##################################################################################

# create local directories
# dir.create(file.path(paste0(kollbar_data, "shapefile")))
dir.create(file.path(paste0(getwd(), "/output")))
dir.create(file.path(paste0(getwd(), "/shapefile")))

# define local paths
folder_shapefile <- paste0(getwd(), "/shapefile")
folder_output <- paste0(getwd(), "/output")


# define path to Github folder containing geodata
folder_github = "https://github.com/bjornsh/gis_data/raw/main/" 





##################################################################################
### Load data
##################################################################################

# create df with dummy coordinates
seq_y <- seq(from=595600, to=702400, by=1)
seq_x <- seq(from=6592400, to=6726400, by=.01)
y <- sample(seq_y, size=100000, replace=TRUE)
x <- sample(seq_x, size=100000, replace=TRUE)
koordinat <- data.frame(x, y)


### DeSO
url_shp = paste0(folder_github, "scb/deso/Deso_inomUppsalaLan.zip")
get_shapefile(url_shp)
deso = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
               options = "ENCODING=WINDOWS-1252")



##################################################################################
### Datahantering
##################################################################################

# create SF from coordinates, intersect with DeSO and sample X rows per DeSO
sample_popsp = koordinat %>% 
  st_as_sf(
    coords = c("y", "x"),
    agr = "constant",
    crs = 3006,        # assign WGS84 as CRS
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_join(., deso) %>% 
  group_by(Deso) %>% 
  dplyr::slice_sample(n = 10) # number of points per DeSO


# check that it worked
sample_popsp %>% 
  as.data.frame() %>% 
  group_by(Deso) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  view()


# visualise
mapview(sample_popsp)


# save to file
st_write(sample_popsp, paste0(folder_output, "/sample_pop_deso.shp"))














