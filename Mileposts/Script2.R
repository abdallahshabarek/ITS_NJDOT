
route <- "00000130__"

df <- sf::read_sf(paste0("C:/Users/Abdullah.Shabarek/Desktop/AMC_Coding/ritis/Milepost/NJ_Roads_shp/",route,"results2.gpkg"))
# df2 <- sf::st_transform(df, crs = sf::st_crs("+proj=longlat +datum=WGS84 +no_defs"))

df$MP <- 0
df$long <- 0
df$lat <- 0
for ( i in (1:nrow(df))){
  pt <- sf::st_cast(df$geom[i], "POINT")
  df$long[i] <- unlist(pt[1,])[1]
  df$lat[i] <- unlist(pt[1,])[2]
  if(df$fid[i] == 1){
    df$MP[i] <- 0
  } else {
    df$MP[i] <- df$MP[i-1]+0.1
  }
}
df$geom <- NULL
# sf::write_sf(df,"C:/Users/Abdullah.Shabarek/Desktop/AMC_Coding/ritis/Milepost/NJ_Roads_shp/00000001__results3.gpkg")
readr::write_csv(df, paste0("C:/Users/Abdullah.Shabarek/Desktop/AMC_Coding/ritis/Milepost/NJ_Roads_shp/",route,"results3.csv"))


