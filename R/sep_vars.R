# WARNING - Generated by {fusen} from dev/2-flat_stat.Rmd: do not edit by hand

#' A function that separates variables in the dataset into 3 groups: identifying (vars_id), descriptive (vars_des), numeric (vars_num)
#' @param dataset a glourbi dataset with varnames comparable to all_cities
#' @return a list with elements vars_id, vars_des, vars_cat, vars_num
#'
#' @export
#' @examples
#' sep_vars(all_cities)
sep_vars <- function(dataset){
  dataset =dataset %>%
    na.omit()
  # Keep only variables used for calculations of PCA
  vars_id="name"
  vars_des=c("Urban.Aggl","ID","Latitude","Longitude",
             "Continent","Country.Co","City.Code","selection1","selection1_Discourses","selection1_GSW"
             )
  vars_cat=c("clco","biom","clim")
  if("cluster" %in% colnames(dataset)){vars_cat=c(vars_cat,"cluster")}
  vars_num=c("X2018","X1980","pop_growth","pop_mean","Area","plain_area","plain_perc",
             "disc","alti","prec","hdev")
  return(list(dataset=dataset,
              vars_id=vars_id,
              vars_des=vars_des,
              vars_cat=vars_cat,
              vars_num=vars_num))
}
