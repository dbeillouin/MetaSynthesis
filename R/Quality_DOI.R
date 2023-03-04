#' @title Integrate the quality score into the variance
#' @description BAsed on Doiu et al., 2008.https://www.jstor.org/stable/20486499
#' @param SCORE Quality score of the meta-analysis (numeric)
#' @param vi Variance of the effect-size
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname QUALITY_DOI
#' @export
QUALITY_DOI<- function(SCORE, vi){
  ## Quality folowing Doi et al., 2018
  Qi   <- SCORE/max(SCORE)
  taui <-(((1-Qi)/vi)/(length(SCORE)-1))

  Qprimj <- NULL
  Qprimj<- dplyr::case_when(Qi<1~ (sum(Qi)*taui)/
              (sum(taui)*(length(SCORE)-1))+
              Qi,TRUE ~Qi)


  tauprim<-NULL
  for(j in 1: length(SCORE)){
    tauprim[j]<-
      (sum(taui)*length(SCORE)*
         (Qprimj[j]/ (sum(Qprimj))) -
         taui[j])
  }

  Qv<-Qi/vi
  W<-Qv+tauprim
  return(W)
}
