# WARNING - Generated by {fusen} from dev/3-flat_global_map.Rmd: do not edit by hand

#' global_map Title

#' @param dataset a dataset, defaults to all_cities
#' @param varname a string corresponding to one of the variables in all_cities. Defaults to X2018
#' @return a leaflet map
#' @export
#'
#' @examples
#' global_map(all_cities,"X2018")
#' global_map(all_cities,"clim")
global_map <- function(dataset=all_cities,varname="X2018") {
  pal=fun_palette(dataset,varname)
  datamap=dataset %>%
    dplyr::mutate(colors=pal(dataset[[varname]]))
  map= leaflet::leaflet(datamap) %>%
       leaflet::addProviderTiles("OpenStreetMap.Mapnik") %>%
       leaflet::addCircleMarkers(fillColor=~colors,
                                 popup=~name,
                                 layerId=~name,
                                 stroke=~selection1,
                                 color="black",
                                 weight=2,
                                 fillOpacity=0.75,
                                 radius=6)
  return(map)
}
