# WARNING - Generated by {fusen} from dev/2-flat_stat.Rmd: do not edit by hand

#' Plot the variables on factorial plan (i,j)
#' @param dataset a dataset on which PCA has been run
#' @param pca a PCA object, as returned by run_pca
#' @param type which type of element you wish to plot ("var" or "ind")
#' @param i factorial axis represented as x (defaults to "Dim.1")
#' @param j factorial axis represented as y (defaults to "Dim.2")
#' @param highlight_subset a subset of dataset which should be highlighted in the plot of individuals
#' @return a PCA plot of individuals or variables
#' @export
#' @examples
#' data(all_cities)
#' mypca=run_pca(all_cities,quali.sup="clco")
#' plot_pca(all_cities_part_desc,mypca, type="var")
#' plot_pca(all_cities,mypca, type="ind")
#'
#' all_cities_clust=run_hclust(all_cities, nclust=10)
#' mypca=run_pca(all_cities_clust,quali.sup="cluster")
#' plot_pca(all_cities_clust,mypca,type="var")
#' plot_pca(all_cities_clust,mypca,type="ind")
#'
#' mypca=run_pca(all_cities_clust,quali.sup="X2018")
#' plot_pca(all_cities_clust,mypca,type="var")
#' plot_pca(all_cities_clust,mypca,type="ind")
#' plot_pca(all_cities_clust,mypca,type="ind",highlight_subset=dataset %>% dplyr::filter(Urban.Aggl=="Lyon"))
plot_pca <- function(dataset,pca, type="var", i="Dim.1", j="Dim.2",highlight_subset=NULL){
  # Calculate dataset of coordinates on factorial plan
  pcadata=pca[[type]]$coord %>%
    as.data.frame() %>%
    tibble::rownames_to_column("name")%>%
    tibble::as_tibble()
  pcadata[[i]]=rescale(pcadata[[i]],1)
  pcadata[[j]]=rescale(pcadata[[j]],1)

  # Initialize plot
  plot=ggplot2::ggplot(data=pcadata,
                       ggplot2::aes(.data[[i]],y=.data[[j]]))+
    ggplot2::scale_x_continuous(limits=c(-1.2,1.2))+
    ggplot2::scale_y_continuous(limits=c(-1.2,1.2))+
    ggplot2::theme(legend.position="none")+
    ggplot2::geom_vline(xintercept=0, col="dark grey")+
    ggplot2::geom_hline(yintercept=0, col="dark grey")
    # Calculate color palette if needed
  if("quali.sup" %in% names(pca)){
    datacol=form_palette(dataset=dataset,varname=pca$quali.sup.name)
    plot=plot+
      ggplot2::scale_fill_manual(values=datacol$colors)+
      ggplot2::scale_color_manual(values=c("FALSE"="light grey","TRUE"="black"))
  }
  ### Type var
  if(type=="var"){
    plot=plot+
      ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=.data[[i]],yend=.data[[j]]),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.1,"cm")),
                            col="grey")+
      ggplot2::geom_text(ggplot2::aes(x=1.1*.data[[i]], y=1.1*.data[[j]], label=name))

  }
  # Type ind
  if(type=="ind"){
    if(!is.null(pca$quali.sup.name)){
        pcadata=pcadata %>%
          dplyr::mutate(group=pca$quali.sup.value,
                        selection1=as.character(dataset$selection1))
        plot=plot +
          ggplot2::geom_point(data=pcadata,
                              shape=21,size=4, alpha=0.5,
                              ggplot2::aes(x=.data[[i]],y=.data[[j]],
                                          fill=group,text=name, color=selection1))
    }else{
        plot=plot +
          ggplot2::geom_text(ggplot2::aes(x=.data[[i]], y=.data[[j]],
                                          text=name))
    }
    if(!is.null(highlight_subset)){
      highlight_pcadata=pcadata %>%
        dplyr::filter(name %in% highlight_subset$name)
      plot=plot+
        ggplot2::geom_point(data=highlight_pcadata,
                            shape=21,size=6,alpha=1,
                            ggplot2::aes(x=.data[[i]],y=.data[[j]],
                                         fill=group,text=name, color=selection1))
    }
  } # end type ind
  plotly=plotly::ggplotly(plot, tooltip="text")
  return(plotly)
}
