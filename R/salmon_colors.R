#' Vector of custom salmon colors
#'
#' @param ...
#'
#' @return salmon.colors
#' @export
#'
#' @examples


  # chi.fill.all <- paletteer::paletteer_dynamic("cartography::blue.pal",20)
  # chi.fill <- chi.fill.all[c(1,3,5,7,8,9,10,12,14,15,16,18)]
  #
  # #chum
  # chu.fill.all <- paletteer::paletteer_dynamic("cartography::orange.pal",5)
  # chu.fill <- chu.fill.all[c(1,3,4)]
  #
  # #coho
  # co.fill.all <- paletteer::paletteer_dynamic("cartography::green.pal",6)
  # co.fill <- co.fill.all[c(1,4,5,6)]
  #
  # #pink
  # pi.fill.all <- paletteer::paletteer_dynamic("cartography::purple.pal",3)
  # pi.fill <- pi.fill.all[c(2)]
  #
  # #other salmonids
  # os.fill.all <- paletteer::paletteer_dynamic("cartography::pink.pal",3)
  # os.fill <- os.fill.all[c(2)]


salmon_colors <- function(...){

  col.names <- c("Chinook Hatch SY" = "#DCF0F8FF","Chinook Hatch Y" = "#C4E0ECFF" ,"Chinook other SY"  = "#ADD0E1FF",
                                    "Chinook other Y" = "#96C0D6FF", "St. of Georgia salmonids" = "#8AB8D0FF", "Chinook Skagit SY" = "#7EB0CBFF",
                                    "Chinook Skagit Y"="#6EA6C3FF","Chinook Snohomish SY" = "#4F91B4FF","Chinook Duwamish SY" = "#307BA5FF",
                                    "Chinook Hood Canal SY" = "#236F9AFF","Chinook Nisqually SY"= "#1E6085FF", "Chinook Nisqually Y"= "#14405AFF",
                                    "Chum Fall SY"= "#FCE480FF", "Chum Hatch SY"= "#FD932EFF", "Chum Hood Canal SY"="#FE5C00FF",
                                    "Coho other Y" = "#BBDAADFF", "Coho Skagit Y" = "#468E3DFF", "Coho Hatch Y" = "#247524FF",
                                    "Coho Deep South Y" = "#16642AFF","Pink Salmon SY" ="#9A70ABFF", "Other salmonids"= "#D746BBFF")

  return(col.names)

  }





