size <- vector("numeric",15)
res <- vector("numeric",15)
timing <- vector("numeric",15)
for(i in 0:14){
  timing[i+1] <- system.time({x <- get_elev_raster(data.frame(long = 1, lat = 1), 
                       prj = elevatr:::ll_geo, z = i)})[3]
  size[i+1] <- miscPackage::byter(x,"Mb")
  res[i+1] <- raster::res(x)[1]
}
xdf <- dplyr::tibble(zoom = 0:14, res, size, timing)
xdf
