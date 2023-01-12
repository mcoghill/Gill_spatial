# FUNCTIONS FOR DOWNLOADING LIDAR DATA

# Get publicly available LiDAR for the Goudie area: see the link for function:
# https://github.com/bcgov/bcmaps/issues/99#issuecomment-1309240729

library(esri2sf) # remotes::install_github("yonghah/esri2sf")
library(sf)
library(mapview)
library(dplyr)
library(httr)
library(stars)
library(terra)
bclidar_get_layers <- function(){
  data.frame(
    name = c("extent",
             "dsm_2500",
             "dsm_10000",
             "laz_2500",
             "dem_2500",
             "dem_20000"),
    format = c("shp",
               "laz",
               "laz",
               "laz",
               "tif",
               "tif"),
    url = c("https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/LiDAR_BC_S3_Public/FeatureServer/0",
            "https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/LiDAR_BC_S3_Public/FeatureServer/1",
            "https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/LiDAR_BC_S3_Public/FeatureServer/2",
            "https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/LiDAR_BC_S3_Public/FeatureServer/3",
            "https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/LiDAR_BC_S3_Public/FeatureServer/4",
            "https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/LiDAR_BC_S3_Public/FeatureServer/5"))}
bclidar_search_aoi <- function(aoi){
  
  bclidar_layers <- bclidar_get_layers()
  
  do.call(bind_rows, lapply(1:nrow(bclidar_layers), function(i){
    name <- bclidar_layers[i,"name"]
    format <- bclidar_layers[i,"format"]
    url <- bclidar_layers[i,"url"]
    tiles <- esri2sf(url, bbox = st_bbox(aoi), )
    tilesize = nrow(tiles)
    data.frame(layer=name, 
               format=format, 
               n_tiles=tilesize)
  }))}
bclidar_tif_get_tiles <- function(aoi = aoi, 
                                  layer = "dem_20000"){
  
  bclidar_layers <- bclidar_get_layers()
  
  url <- bclidar_layers[bclidar_layers$name == layer,"url"]
  
  tiles <- esri2sf(url, bbox = st_bbox(aoi))
  
  if(nrow(tiles)==0){
    print("NO TILES")}else{
      print(paste("FOUND",nrow(tiles),"TILES"))
      tiles}
}
bclidar_tif_get_data <- function(tiles = my_tiles, 
                                 out_dir = tempdir(),
                                 overwrite = F,
                                 read_r = T,
                                 read_r_format = "terra"){
  
  if(is.null(tiles[1,]$filename)){
    stop("No filename in layer")}
  
  if(length(grep("tif",tiles[1,]$filename))==0){
    stop("This tile does not contain a tif.")}
  
  lapply(1:nrow(tiles), function(i){
    
    print(paste("Downloading", i, "of", nrow(tiles)))
    tile <- tiles[i,]
    file <- tile$filename
    url <- tile$s3Url
    
    if(length(list.files(out_dir, pattern = file))==1&overwrite==F){
      print("File exists, overwrite set to FALSE, skipping to next")}else{
        httr::GET(url, 
                  httr::write_disk(paste(out_dir,file,sep="/"), 
                                   overwrite=overwrite))
        if(i == nrow(tiles)){print("Downloads complete")}
      }
    
  })
  
  
  if(read_r == T){  
    vrt_name=paste0(out_dir,"/bclidar_download_",gsub(" ","_",gsub("-|:","",Sys.time())),".vrt")
    sf::gdal_utils(util = "buildvrt", 
                   source = paste0(out_dir,"/",tiles$file), 
                   destination = vrt_name)
    
    if(read_r_format == "stars"){
      out_img <- stars::read_stars(vrt_name, proxy = T)}
    
    if(read_r_format == "terra"){
      out_img <- terra::rast(vrt_name)}
    
    print("Process complete") 
    out_img
  }
}

##############
bclidar_laz_get_data <- function(tiles = my_tiles, 
                                 out_dir = tempdir(),
                                 overwrite = F){
  
  if(is.null(tiles[1,]$filename)){
    stop("No filename in layer")}
  
  if(length(grep("laz",tiles[1,]$filename))==0){
    stop("This tile does not contain a laz.")}
  
  lapply(1:nrow(tiles), function(i){
    
    print(paste("Downloading", i, "of", nrow(tiles)))
    tile <- tiles[i,]
    file <- tile$filename
    url <- tile$s3Url
    
    if(length(list.files(out_dir, pattern = file))==1&overwrite==F){
      print("File exists, overwrite set to FALSE, skipping to next")}else{
        httr::GET(url, 
                  httr::write_disk(paste(out_dir,file,sep="/"), 
                                   overwrite=overwrite))
        if(i == nrow(tiles)){print("Downloads complete")}
      }
    
  })
  return(file.path(out_dir, tiles$filename))
}

#####

library(tidyverse)
library(bcdata)
library(sf)
library(lidR)

# Goudie study area
goudie <- bcdc_query_geodata("dfb8b498-fa4b-4286-b3ec-58db88aca1cf") |> 
  bcdata::filter(CUT_BLOCK_ID %in% c("KM1209", "KM1210", "KM1212")) |> 
  collect()

goudie_box <- st_as_sfc(st_bbox(goudie))

# What products are available in this area?
bclidar_search_aoi(aoi = goudie_box)

# Download tile data
my_tiles <- bclidar_tif_get_tiles(aoi = goudie_box, layer = "laz_2500")

r <- bclidar_laz_get_data(tiles = my_tiles, 
                          out_dir = "D:/temp", 
                          overwrite = FALSE)

#####

# LiDAR processing
goudie_laz <- readLAScatalog(r)

# DO LIKE THIS
goudie_clip <- lapply(goudie$CUT_BLOCK_ID, function(x) {
  aoi <- goudie |> 
    dplyr::filter(CUT_BLOCK_ID == x) |> 
    st_transform(st_crs(goudie_laz))
  laz_files <- catalog_intersect(goudie_laz, aoi)
  
  # Clip to aoi and expand
  opt_chunk_size(laz_files) <- 0
  opt_chunk_buffer(laz_files) <- 0
  opt_output_files(laz_files) <- file.path("D:/temp", x)
  las_clip <- clip_roi(laz_files, aoi)
})

