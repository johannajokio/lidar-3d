## LiDar 3D map of central Kuopio, Finland

# to use terra development version: 
install.packages('terra', repos='https://rspatial.r-universe.dev')

libs <- c("sf", "rayshader", "magick")

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(
  any(installed_libraries == F) ){
  install.packages(
    libs[!installed_libraries]
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)
############################
# 1. Get data ----

#heights - TIF file  
lidar_rast <- terra::rast(
 "P5114D.tif"    
)  

#aerial image ie. orthophoto
 
ortho_rast <- terra::rast(
  "P5114D.jp2"
)

terra::plotRGB(ortho_rast)

# 2. CROP AREA ----

coords <- data.frame(
  long = 27.684162,
  lat = 62.891295
) %>% 
  sf::st_as_sf(
    coords = c(
      "long", "lat"
    ),
    crs = sf::st_crs(4326)
  ) %>% 
  sf::st_transform(
    crs = terra::crs(
      ortho_rast 
    )
  )

kpo_buffer <- terra::buffer(
  terra::vect(coords),
  width = 500
)


lidar_crop <- terra::crop(
  lidar_rast,
  kpo_buffer ,
  snap = "in",
  mask = T
)

terra::plot(lidar_crop)

ortho_crop <- terra::crop(
  ortho_rast,
  kpo_buffer,
  snap = "in",
  mask = T
)

terra::plot(ortho_crop)

##############################
# 3. RESAMPLE ---- # Scale it down because of high resolution

ortho_resampled <- terra::resample(
  x = ortho_crop,
  y = lidar_crop,
  method = "bilinear"
)

plot(ortho_resampled)

# 4. SAVE ORTHO AS IMAGE

terra::writeRaster(
  ortho_resampled,
  "kuopio.png",
  overwrite = T
)

img <- png::readPNG(
  "kuopio.png"
)
# 5. FILL MISSING VALUES

lidar_crop_predict <- terra::focal(
  lidar_crop,
  w = 9,
  fun = mean,
  na.policy = "only",
  na.rm = T
)

plot(lidar_crop_predict)

##empty values - assign to minimum value  
min(lidar_crop_predict)
lidar_crop_predict <- terra::ifel(
  is.na(lidar_crop_predict),
  1,
  lidar_crop_predict
)

plot(lidar_crop_predict)

# 6. RENDER
# convert to matrix
lidar_mat <- rayshader::raster_to_matrix(
  lidar_crop_predict
)


lidar_mat %>% 
  rayshader::height_shade() %>% 
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) %>% 
  rayshader::plot_3d(
    lidar_mat,
    solid = F,  # no border
    zscale = 32, # heights
    zoom = .6,
    phi = 45,  # angle
    theta = 30, #rotation around z-axis
    windowsize = 800 
  )

# I remove some objects so my environment doesn't explode
rm(av, coords, kpo_buffer, lidar_rast, ortho_rast, lidar_crop, ortho_crop, ortho_resampled)

# 7. RENDER OBJECT

rayshader::render_highquality(
  filename = "3d-kuopio.png", 
  preview = F,
  light = T,
  lightsize = 200,
  intensity_env = 1,
  rotate_env = 90,
  parallel = T,
  interactive = F,
  width = 800,  # made this smaller 
  height = 800  
)
  
