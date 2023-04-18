#' Compare_BAY_FREQ: Compare les estimations de SOC entre méthodes bayésiennes et méthodes fréquentistes.
#'
#' Cette fonction permet de comparer les estimations de la concentration et de la quantité de carbone organique du sol pour différentes pratiques de gestion des sols, des changements d'utilisation des terres et des changements globaux.
#'
#' @param Freq Un dataframe contenant les résultats des méta-analyses fréquentistes
#' @param Bay Un dataframe contenant les résultats des méta-analyses Bayesiennes
#' @return Un graphique ggplot2 interactif avec les estimations de la concentration et de la quantité de carbone organique du sol.
#' @export
#'
#' @examples
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly ggplotly


Compare_Freq_Bay <- function(Freq, Bay) {

  # Sélection des variables nécessaires
  var_names <- c(names(Freq)[1:which(names(Freq)=="n_ES")-1], "estimate", "conf.low", "conf.high","AIC$answer")

  # Filtrage et traitement des données
  Freq <- Freq[, var_names] %>%
    dplyr::filter(Intervention %in% c("management", "global changes", "land use change")) %>%
    dplyr::group_by(Land_use, Intervention) %>%
    dplyr::arrange(Land_use, -estimate) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::rename_with(~ paste0(., "_Freq"), dplyr::contains(c("estim", "conf")))

  Bay <- Bay[, var_names] %>%
    dplyr::filter(Intervention %in% c("management", "global changes", "land use change")) %>%
    dplyr::group_by(Land_use, Intervention) %>%
    dplyr::arrange(Land_use, -estimate) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::rename_with(~ paste0(., "_Bay"), dplyr::contains(c("estim", "conf")))

  VAR<-names(Freq)[1:which(names(Freq)=="estimate_Freq")-1]
  JOINT_TAB <- dplyr::full_join(Freq, Bay, by = VAR)

  # # Calcul des couleurs
  # for (i in 1:length(JOINT_TAB$estimate_Freq)){
  #   JOINT_TAB$color[i] <- length(intersect(round(runif(9000, min = JOINT_TAB$conf.low_Freq[i], max = JOINT_TAB$conf.high_Bay[i]),2),
  #                                          round(runif(9000, min = JOINT_TAB$conf.low_Bay[i], max = JOINT_TAB$conf.high_Bay[i]),2)))
  #   JOINT_TAB$color[i] <- if (JOINT_TAB$color[i] == 0) {'#f5bd04'} else {'#D16103'}
  # }

  # Calcul des couleurs
  for (i in 1:length(JOINT_TAB$estimate_Freq)){
    JOINT_TAB$color[i] <- (JOINT_TAB$estimate_Freq[i]-JOINT_TAB$estimate_Bay[i])
   # JOINT_TAB$color[i] <- if (JOINT_TAB$color[i] == 0) {'#f5bd04'} else {'#D16103'}
  }

  # Création du graphique
  p <- ggplot2::ggplot(data = JOINT_TAB, ggplot2::aes(y = estimate_Bay, x = estimate_Freq,
                       text = paste("Land use :"       , Land_use,"\n",
                                    "Intervention :"   ,Intervention,"\n",
                                    "Sub_cat_intervention : " , Sub_Cat_intervention, "\n",
                                    "sub-category :"   , details_outcome,"\n",
                                    "estimate_Freq :"       ,round(estimate_Freq,1), "\n",
                                    "estimate_Bay :"       ,round(estimate_Bay,1), "\n")))+
    ggplot2::geom_point()+
    #ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low_Bay, ymax = conf.high_Bay), linetype = "dotted", color = "gray60") +
    #ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low_Freq, xmax = conf.high_Freq), linetype = "dotted", color = "gray60")+
    ggpubr::theme_pubr()+
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0))+
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0))+
    ggplot2::geom_point(aes(fill = as.numeric(color)), shape = 21, stroke = 0.2, size = 5)  +
    ggplot2::scale_fill_gradient2(low = scales::muted("red"),mid = "white",  high = scales::muted("blue"))

  p <- p + ggplot2::facet_wrap(~ Intervention)

  # Ajout d'un titre et d'une légende
  p <- p + ggplot2::ggtitle("SOC between linear and mixed model methods") +
    ggplot2::ylab("SOC - linear") + ggplot2::xlab("SOC- mixed model") +
    ggplot2::theme(legend.position="none")
   # ggplot2::scale_fill_manual(values = c("#D16103", "#f5bd04"), name = "Intersection\nStock/Concentration")

 # p<- p+ ggplot2::xlim(  min(JOINT_TAB$estimate_Freq)*1.2,  max(JOINT_TAB$estimate_Freq)*1.2)+
  #  ggplot2::ylim(  min(JOINT_TAB$estimate_Bay)*1.2,  max(JOINT_TAB$estimate_Bay)*1.2)

  p2 <- plotly::ggplotly(p,tooltip = "text")

  JOINT_TAB %<>% dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

  return(list(p,p2,JOINT_TAB))
}
