#' Compare_CONC_Stock: Compare les estimations de la concentration et de la quantité de carbone organique du sol
#'
#' Cette fonction permet de comparer les estimations de la concentration et de la quantité de carbone organique du sol pour différentes pratiques de gestion des sols, des changements d'utilisation des terres et des changements globaux.
#'
#' @param data Un dataframe contenant les données à utiliser pour la comparaison.
#' @return Un graphique ggplot2 interactif avec les estimations de la concentration et de la quantité de carbone organique du sol.
#' @export
#'
#' @examples
#' data <- BEST_fit_DO_LU_IN
#' Compare_CONC_Stock(data)
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly ggplotly

Compare_CONC_Stock <- function(data) {

  # Sélection des variables nécessaires
  var_names <- c("Land_use", "Intervention", "estimate", "conf.low", "conf.high", "details_outcome")
  vars <- data[, var_names]

  # Filtrage et traitement des données
  MEAN_stock <- vars %>%
    dplyr::filter(details_outcome == "SOC stock" & Intervention %in% c("management", "global changes", "land use change")) %>%
    dplyr::group_by(Land_use, Intervention) %>%
    dplyr::arrange(Land_use, -estimate) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::rename_with(~ paste0(., "_Stock"), dplyr::contains(c("estim", "conf"))) %>%
    dplyr::select(Land_use, Intervention, position, estimate_Stock, conf.low_Stock, conf.high_Stock)

  MEAN_conc <- vars %>%
    dplyr::filter(details_outcome == "SOC concentration" & Intervention %in% c("management", "global changes", "land use change")) %>%
    dplyr::group_by(Land_use, Intervention) %>%
    dplyr::arrange(Land_use, -estimate) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::rename_with(~ paste0(., "_Conc"), dplyr::contains(c("estim", "conf"))) %>%
    dplyr::select(Land_use, Intervention, position, estimate_Conc, conf.low_Conc, conf.high_Conc)

  JOINT_TAB <- dplyr::full_join(MEAN_stock, MEAN_conc, by = c("Land_use", "Intervention", "position"))

  # Calcul des couleurs
  for (i in 1:length(JOINT_TAB$estimate_Stock)){
    JOINT_TAB$color[i] <- length(intersect(round(runif(9000, min = JOINT_TAB$conf.low_Conc[i], max = JOINT_TAB$conf.high_Stock[i]),2),
                                           round(runif(9000, min = JOINT_TAB$conf.low_Stock[i], max = JOINT_TAB$conf.high_Stock[i]),2)))
    JOINT_TAB$color[i] <- if (JOINT_TAB$color[i] == 0) {'#f5bd04'} else {'#D16103'}
  }

     # Création du graphique
     p <- ggplot2::ggplot(data = JOINT_TAB, ggplot2::aes(y = estimate_Stock, x = estimate_Conc)) +
       ggplot2::geom_point()+
       ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low_Stock, ymax = conf.high_Stock), linetype = "dotted", color = "gray60") +
       ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low_Conc, xmax = conf.high_Conc), linetype = "dotted", color = "gray60")+
       ggpubr::theme_pubr()+
       ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1))+
       ggplot2::geom_hline(ggplot2::aes(yintercept = 0))+
       ggplot2::geom_vline(ggplot2::aes(xintercept = 0))+
       ggplot2::geom_point(fill = JOINT_TAB$color, shape = 21, stroke = 0.2, size = 5)

     p <- p + ggplot2::facet_wrap(~ Intervention)

     # Ajout d'un titre et d'une légende
     p <- p + ggplot2::ggtitle("Comparaison des stocks et concentrations de carbone organique du sol") +
       ggplot2::xlab("Concentration (g/kg)") + ggplot2::ylab("Stock (t/ha)") +
       ggplot2::scale_fill_manual(values = c("#D16103", "#f5bd04"), name = "Intersection\nStock/Concentration")

     return(p)
}
