#' Returns txt_page data for given citycode and river
#' @param thisCityCode the citycode
#' @param thisRiver the river
#' @param conn the connection to glourb database
#' @return a table
#' @export
get_txt_page=function(thisCityCode,
                      thisRiver,
                      conn){
  tib_page=glourbi::get_city_tib(name="txt_page",
                                 thisCityCode=thisCityCode,
                                 conn=conn) %>%
    dplyr::select(river_en,
                  query,
                  hl,
                  position,
                  title,
                  link,
                  domain,
                  trans_snippet,
                  text_en,
                  id,
                  lemmatext)

  if(length(thisRiver)==1){
    result=tib_page %>%
      dplyr::filter(river_en==thisRiver)
  }else{result=tib_page}
  return(result)
}
#' Returns txt_page data for given citycode and river
#' @param thisCityCode the citycode
#' @param thisRiver the river
#' @param conn the connection to glourb database
#' @return a table
#' @export
get_txt_segment=function(thisCityCode,
                         thisRiver,
                         conn){
  tib_segment=glourbi::get_city_tib(name="txt_segment",
                                    thisCityCode=thisCityCode,
                                    conn=conn)
  if(length(thisRiver)==1){
    result=tib_segment %>%
        dplyr::filter(river_en==thisRiver)
  }else{result=tib_segment}
  return(result)
}
