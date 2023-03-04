#' @title Check Graphs
#' @description Creates a forest plot and two variance plots to check data.
#' @param name_file Name of the file to use as input data. Default: 'RATIO'.
#' @param name_col_outcome Name of the column in the input data that contains the outcome variable. Default: 'Outcome'.
#' @param outcome_of_interest Outcome variable of interest. Default: 'soil carbon'.
#' @param intervention Name of the column in the input data that contains the intervention variable. Default: 'Sub_Cat_intervention'.
#' @param effect_size Name of the column in the input data that contains the effect size variable. Default: 'Effect size'.
#' @return A list containing the variance plots and the forest plot.
#' @details This function takes input data and creates a forest plot and two variance plots to check the data.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{facet_wrap}}, \code{\link[ggplot2]{geom_abline}}, \code{\link[ggplot2]{geom_crossbar}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{scale_size}}
#'  \code{\link[ggpubr]{ggpubr}}, \code{\link[ggpubr]{theme_pubr}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}
#' @export
#' @importFrom ggplot2 ggplot geom_point facet_wrap geom_hline geom_errorbar theme scale_size
#' @importFrom ggpubr theme_bw theme_pubr
#' @importFrom dplyr ungroup group_by arrange mutate filter
Check_graphs<- function(Name_File           = "RATIO",
                        Name_Col_Outcome    = "Outcome",
                        Outcome_of_interest =  "soil carbon",
                        Intervention        = "Sub_Cat_intervention",
                        Effect_size         = "Effect size"){

# Load Data
  ES_TMP  <-readxl::read_excel(here::here("data","derived-data","ALL_metrics.xlsx"))
  RATIO  <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx"))


  # Check Data

  ##### Plot 1 ####
  AA<-ES_TMP %>% dplyr::filter(Outcome == "soil carbon")
  AA$percent<-(exp(AA$`Effect size`)-1)*100

  histogram<-ggplot2::ggplot(ES_TMP %>% dplyr::filter(Land_use %in% c('cropland', 'forest land', 'grassland','na')),
                             ggplot2::aes(x = !!dplyr::sym(Effect_size))) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..),fill= "white", color="black",binwidth=0.1) +
    ggplot2::geom_density(binwidth=1)+
    ggplot2::scale_fill_viridis_c(name = "Effect_size", option = "C") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())+
    ggpubr::theme_pubr()+
    ggplot2::geom_vline(ggplot2::aes(xintercept=0), linetype=2)+
    ggplot2::facet_wrap(~Land_use, scales= "free")+
    ggplot2::xlim(-2,2)

  ##### Plot 2 ####

  Variance_All_metric<-ggplot2::ggplot(ggplot2::aes((exp(!!dplyr::sym(Effect_size))-1)*100,vi),data=AA)+
    ggplot2::geom_point()+
    ggpubr::theme_pubr()+
    ggplot2::facet_wrap(~metric,scales="free")
 # Variance_All_metric<-plotly::ggplotly(Variance_All_metric)


  ##### Plot 3 ####
  AA<- RATIO %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!dplyr::sym(Name_Col_Outcome)) %>%
    dplyr::arrange(!!dplyr::sym(Intervention), !!dplyr::sym(Name_Col_Outcome), !!dplyr::sym(Effect_size)) %>%
    dplyr::mutate(positionInCategory = 1:dplyr::n(),
                  order = dplyr::row_number()) %>%
    dplyr::filter(!!dplyr::sym(Name_Col_Outcome) == Outcome_of_interest)


  forestplot<-ggplot2::ggplot(AA)+
    ggplot2::geom_point(ggplot2::aes(reorder(order, !!dplyr::sym(Effect_size)), !!dplyr::sym(Effect_size), color = !!dplyr::sym(Intervention)))+
    ggplot2::geom_hline(ggplot2::aes(yintercept= 0), linetype=2, color="gray")+
    ggplot2::geom_errorbar(ggplot2::aes(reorder(order, !!dplyr::sym(Effect_size)), !!dplyr::sym(Effect_size),
                               ymin = lower_CI, ymax = upper_CI), width = 0.2, position = ggplot2::position_dodge(.9))+
    ggpubr::theme_pubr() +
    ggplot2::theme(legend.position='none')
  # dynamic plot
 # forestplot<-plotly::ggplotly(forestplot)

  ##### Plot 4 ####
  varianceplot<-ggplot2::ggplot(AA)+
    ggplot2::geom_point(ggplot2::aes(!!dplyr::sym(Effect_size), log(vi), color = !!dplyr::sym(Intervention), size = as.numeric(N_paired_data),fill=ID))+
    ggplot2::geom_hline(ggplot2::aes(yintercept= 0), linetype=2, color="gray")+
    ggpubr::theme_pubr()+
    ggplot2::theme(legend.position='none')+
    ggplot2::scale_size(range = c(0, 3))

  # dynamic plot
  varianceplot<-plotly::ggplotly(varianceplot)

  print(list(histogram,Variance_All_metric, forestplot,varianceplot))
  print("Four plots have been produced")
}
