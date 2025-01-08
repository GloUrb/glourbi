library(tidyverse)
tibpol= sf::st_read(dsn="data-raw/data-gitignored/",
                    layer="StudyArea_reach_zone")
#' The splash_pol function writes the sf objects (tibpol) to shapefiles in directory "inst/{dir}"
splash_pol=function(tibpol,dir){
  tibpol=tibpol %>%
    mutate(filename=paste0("inst/",dir,"/",CityCode,".shp")) %>%
    select(-ID) %>%
    group_by(CityCode,filename) %>%
    tidyr::nest() %>%
    mutate(do=purrr::walk2(.x=data,.y=filename,~sf::st_write(.x,dsn=.y,overwrite=TRUE)))
  return(tibpol)
}

#' Create target directory and run splash_pol to get individual shapefiles for each city
dir.create("inst/per_city")
splash_pol(tibpol,dir="per_city")

#' Get list of cities (identifiers CityCode and name UrbanAggl) in selection 1 for GSW analyses
selection1_GSW=tibpol

#' Get list of cities (identifiers CityCode and name UrbanAggl) in selection 1 for Discourses analyses
conn=glourbi::connect_to_glourb()
txt_city_rivers=DBI::dbReadTable(conn,"txt_city_rivers")


## code to prepare `DATASET` dataset goes here
all_cities=readr::read_csv("data-raw/data-gitignored/GHS_all_complete_subset.csv")


all_cities=all_cities %>%
  mutate(Continent=case_when(is.na(Continent)~"AN",
                              TRUE~Continent)) %>%
  group_by(Urban.Aggl) %>%
  mutate(ntot=n()) %>%
  mutate(rank=1:n()) %>%
  mutate(rank=case_when(ntot==1~"",
                        TRUE~as.character(rank))) %>%
  mutate(name=paste0(Urban.Aggl,"-", rank,"-",Country.or)) %>%
  select(-Urban.Aggl,-ntot,-rank) %>%
  ungroup() %>%
  mutate(biom=paste0("b",sprintf("%02d",biom)),
         clco=paste0("c",sprintf("%02d",clco)),
         clim=paste0("c",sprintf("%02d",clim))) %>%
  mutate(biom=as.factor(biom),
         clco=as.factor(clco),
         clim=as.factor(clim)
         ) %>%
  na.omit() %>%
  mutate(selection1_GSW=case_when(ID %in% selection1_GSW$CityCode~ TRUE,
                           TRUE~ FALSE),
         selection1_Discourses=case_when(ID %in% txt_city_rivers$citycode~TRUE,
                                         TRUE~FALSE)) %>%
  mutate(Urban.Aggl=case_when(Urban.Aggl=="Santiago" & Country.or=="Chile"~"Santiago, Chile",
                              Urban.Aggl=="Santiago" & Country.or=="Dominican Republic"~"Santiago, D.R.",
                              TRUE~Urban.Aggl))

all_cities %>% filter(stringr::str_detect(Urban.Aggl, "Santiago"))

usethis::use_data(all_cities, overwrite = TRUE)

##
meta_all_cities=readr::read_delim("data-raw/data-gitignored/GHS_all_complete_subset_README.txt",
                                  delim = ";",
                                  escape_double = FALSE, trim_ws = TRUE)
usethis::use_data(meta_all_cities, overwrite=TRUE)


