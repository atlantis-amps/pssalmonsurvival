#' @title Mapping Atlantis polygons
#' @description  Functions to plot shapefile
#' @details INPUT: 1) An Atlantis model polygon shape file
#' @details OUTPUT: 1) Map of Atlantis polygons
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @return model.map
#' @export
#' @param shape.file which shape file to map

make_map <- function(file.name, get_shapefile) {


  #dir.create("./shapefiles") # create directory for shapefile

  # download files from Github data repository using get_shapefile function

  #file.names <- c("CPG","dbf","prj","sbn","sbx","shp","shp.xml","shx") #specify file extensions to download

  #shapefile.name <- "bgm_Puget_Sound_89b_0p0001_WGS84."
  #lapply(file.names, get_shapefile, shapefile.name) # get shapefile, download as raw

  dsn.shp <- here::here("shapefiles", paste0(shapefile.name,"shp")) # get file location
  model.shape <- sf::st_read(dsn.shp) # read shapefile

 # shapefile.name <- "Puget_Sound_cities_WSG84."
 # lapply(file.names, get_shapefile, shapefile.name) # get shapefile, download as raw

  dsn.shp <- here::here("shapefiles", paste0(shapefile.name,"shp")) # get file location
  cities.shape <- sf::st_read(dsn.shp) %>%
    dplyr::filter(Name!="Bellevue")# read shapefile


  world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") # get world map

  min.long <- -121.9072
  max.long <- -124.0495
  min.lat <- 46.982
  max.lat <- 49.2412

  #map world and overlay model polygons
  model.map <-  ggplot2::ggplot(data = world) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = model.shape, ggplot2::aes(fill = BOTZ)) +
    ggplot2::geom_sf(data=cities.shape) +
    ggplot2::geom_text(data = cities.shape, ggplot2::aes(x = LONG, y = LAT, label = Name),
                       size = 2.5, col = "black", fontface = "plain", nudge_y = 0.05, nudge_x = 0.1) +
    ggplot2::scale_fill_continuous(trans = "sqrt", name = "Depth") +

    #    ggplot2::scale_fill_viridis_c(trans = "sqrt", alpha = .4, name = "Depth") +
    ggplot2::coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE) +
    ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = ggplot2::unit(0.1, "in"), pad_y = ggplot2::unit(0.1, "in"),
                                      style = ggspatial::north_arrow_fancy_orienteering()) +
    ggspatial::annotation_scale(location = "tr", width_hint = 0.2) +
    ggplot2::xlab("Lon")+
    ggplot2::ylab("Lat")+
    ggplot2::theme_bw()


  ggplot2::ggsave(file.name, model.map, width = 12, height = 12, units="cm", dpi = 400)

  #return map
  return(model.map)

}
