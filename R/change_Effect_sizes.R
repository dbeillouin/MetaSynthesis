

#' @title Convert Z score to correlation coefficient
#' @description This function converts a Z score to a correlation coefficient using the formula r = (exp(2Z)-1)/(exp(2Z)+1)
#' @param Z a numeric value representing a Z score
#' @return a numeric value representing the corresponding correlation coefficient
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' FUN_Z_to_r(2)
#' }
#' }
#' @rdname FUN_Z_to_r
#' @export 
FUN_Z_to_r<-function(Z){
  r=(exp(2*Z)-1)/(exp(2*Z)+1)
  return(r)}

#' @title Convert correlation coefficient to standardized mean difference (Cohen's d)
#' @description This function converts a correlation coefficient to Cohen's d using the formula d = 2r/(sqrt(1-r^2))
#' @param r a numeric value representing a correlation coefficient
#' @return a numeric value representing the corresponding standardized mean difference (Cohen's d)
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' FUN_r_to_d(0.5)
#' }
#' }
#' @rdname FUN_r_to_d
#' @export
FUN_r_to_d<-function(r){
  d= 2*r/(sqrt(1-r*r))
  return(d)}

#' @title Calculate variance from mean and confidence interval
#' @description This function calculates the variance given a mean and its corresponding confidence interval using the formula Var = ((CI-mean)/1.96)^2 see http://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm
#' @param mean a numeric value representing the mean
#' @param CI a numeric value representing the confidence interval
#' @return a numeric value representing the variance
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' FUN_var_from_CI(10, 2)
#' }
#' }
#' @rdname FUN_var_from_CI
#' @export
FUN_var_from_CI<-function(mean, CI){
Var= (abs(CI-mean)/1.96)^2
return(Var)}


#' @title Convert variance of correlation coefficient to variance of standardized mean difference (Cohen's d)
#' @description This function converts the variance of a correlation coefficient to the variance of Cohen's d using the formula Var_d = 4Var_r/((1-r^2)^3)
#' @param Var_r a numeric value representing the variance of a correlation coefficient
#' @param r a numeric value representing a correlation coefficient
#' @return a numeric value representing the variance of Cohen's d
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname FUN_var_r_to_d
#' @export 
FUN_var_r_to_d<-function(Var_r, r){
  Var_d= 4*Var_r/((1- r*r)^3)
  return(Var_d)
}

#' @title Calculate confidence interval from variance
#' @description This function calculates the confidence interval given the variance using the formula CI = sqrt(Var)/21.96
#' @param Var a numeric value representing the variance
#' @return a numeric value representing the confidence interval
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' FUN_CI_from_var(4)
#' }
#' }
#' @rdname FUN_CI_from_var
#' @export 
FUN_CI_from_var<-function(Var){
  CI= sqrt(Var)/2*1.96
  return(CI)
}

#' Convertir d en Hedges' g
#'
#' Convertit la taille de l'effet d (Cohen's d) en taille de l'effet Hedges' g en utilisant le facteur de correction J.
#' J est une approximation couramment utilisée pour convertir d en g.
#' La formule exacte de J a été décrite par Hedges (1981).
#'
#' @title Convertir d en Hedges' g
#' @param d La taille de l'effet d à convertir
#' @param df Les degrés de liberté associés à d
#' @return La taille de l'effet Hedges' g correspondante
#' @examples
#' \dontrun{
#' if(interactive()){
#' #EXAMPLE1
#' }
#' }
#' @references Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators. Journal of Educational Statistics, 6(2), 107-128.
#' @keywords Hedges' g, Cohen's d, taille de l'effet
#' @export
FUN_d_to_g <- function(d, df){
  J <- 1 - 3/(4*df - 1)
  g <- J * d
  return(g)
}


#' @title Calculate variance from confidence interval
#' @description This function calculates the variance from a given confidence interval.
#' @param CI A numeric vector representing the confidence interval.
#' @return The variance calculated from the confidence interval.
#' @details The formula used to calculate the variance is Var = (CI / 1.96 * 2) ^ 2, where 1.96 is the z-score for a 95% confidence interval.
#' @examples
#' \dontrun{
#' if(interactive()){
#' #EXAMPLE1
#' CI <- c(2.5, 5.5)
#' FUN_Var_from_CI(CI)
#' }
#' }
#' @rdname FUN_Var_from_CI
#' @export
FUN_Var_from_CI <- function(CI) {
  Var <- (CI / 1.96 * 2) ^ 2
  return(Var)
}

#' @title Convertir d'un odds ratio en Cohen's d
#' @description Cette fonction permet de convertir un odds ratio en Cohen's d.
#' @param LOR Le odds ratio à convertir.
#' @return La valeur de Cohen's d.
#' @details La formule utilisée pour convertir un odds ratio en Cohen's d est : d = LOR * sqrt(3) / pi.
#' @examples
#' \dontrun{
#' if(interactive()){
#' # Exemple d'utilisation :
#' FUN_LOR_to_d(1.5)
#' # Le résultat attendu est : 0.2461187
#' }
#' }
#' @rdname FUN_LOR_to_d
#' @export
FUN_LOR_to_d <- function(LOR){
  d <- LOR * sqrt(3) / pi
  return(d)
}

#' @title Convert variance of log odds ratio to variance of Cohen's d
#' @description This function converts the variance of log odds ratio (Var_LOR) to the variance of Cohen's d (Var_d).
#' @param Var_LOR Variance of log odds ratio
#' @return Variance of Cohen's d
#' @details This function is used in meta-analyses when combining studies that report log odds ratios and studies that report Cohen's d.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   # EXAMPLE 1: Convert variance of log odds ratio to variance of Cohen's d
#'   Var_LOR <- 0.01
#'   Var_d <- FUN_var_LOR_to_d(Var_LOR)
#'   print(Var_d)
#' }
#' }
#' @rdname FUN_var_LOR_to_d
#' @export 
FUN_var_LOR_to_d<-function(Var_LOR){
  Var_d <- Var_LOR * 3 / ((pi)^2)
  return(Var_d)
}
