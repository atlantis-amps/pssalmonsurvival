#' @title download shapefiles
#' @param shapefile.name, a string
#' @param eachfilename, a string
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002


get_shapefile <- function(eachfilename, shapefile.name){

  file.name <- paste0("https://github.com/hmorzaria/ampsdata/blob/main/shapefiles/",shapefile.name,eachfilename,"?raw=true")
  dest.file <- paste0("./shapefiles/",shapefile.name,eachfilename)
  print(file.name)
  print(dest.file)
  try(download.file(file.name, dest.file, method = "wget"))

  return("done")
}
