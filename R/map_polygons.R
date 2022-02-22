#' @title Mapping Atlantis polygons
#' @description  Functions to plot shapefile
#' @details INPUT: 1) An Atlantis model polygon shape file
#' @details OUTPUT: 1) Map of Atlantis polygons
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @return model.map
#' @export
#' @param shape.file which shape file to map


make_map <- function(shape.file, file.name, scale.factor, min.long, max.long, min.lat, max.lat) {

  model.shape <- rgdal::readOGR(shape.file) # read shapefile


  model.shape.df <- broom::tidy(model.shape) # Reformat shape for mapping purposes

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # get world map
  #class(world)

  world_points<- sf::st_centroid(world) # get the centroid of the world map
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry))) # get coordinates

  #plot shapefile over world map
  model.map <- ggplot2::ggplot(data = world) +
    sf::geom_sf() +
    ggspatial::annotation_north_arrow(location = "br", which_north = "true",
                                      pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                                      style = north_arrow_fancy_orienteering) +
    #geom_text(data= world_points,aes(x=X, y=Y, label=name),
    #          color = "darkgrey", check_overlap = FALSE) +
    ggplot2::coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
    ggplot2::geom_path(data = model.shape.df, aes(x = long, y = lat, group=group),
                       colour = "lightgrey") +
    ggspatial::annotation_scale(location = "tl", width_hint = 0.2) +
    ggplot2::xlab("Lon")+
    ggplot2::ylab("Lat")+
    ggplot2::theme_bw()+
    ggplot2::annotate(geom="text", x=-102, y=24, label="Puget Sound",
                      color="black")

  #save map
  ggplot2::load_all()
  load_all()
  ggsave(file.name, model.map, width = 16,height = 12, units="cm", dpi = 400)

  #return map
  return(model.map)

}
