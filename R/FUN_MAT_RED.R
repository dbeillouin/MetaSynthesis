
#' @title Calculate the pseudo-variance-covariance matrix based on variance and redundancy
#' @description Based on DOI and al., 2008
#' @param Global_Redundancy_matrix Global matrix of redudancy of primary studies between meta-analyses calculated with the Global_Redundancy_matrix function, Default: 'Matrice_red.csv'
#' @param Selected_ID Selected ID of the meta-analyses (E.g. for one type of intervention)
#' @param Variance Variance of the effect-sizes of the selected meta-analyses.
#' @return a pseudo variance co-variance matrix to be used in the final meta-analytical model
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{fread}}
#'  \code{\link[here]{here}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[base]{subset}}, \code{\link[base]{merge}}, \code{\link[base]{replace}}, \code{\link[base]{length}}, \code{\link[base]{split}}
#'  \code{\link[tidyr]{gather}}
#' @rdname FUN_MAT_RED
#' @export
#' @importFrom data.table fread
#' @importFrom here here
#' @importFrom dplyr filter select left_join arrange
#' @importFrom base subset merge replace length split
#' @importFrom tidyr gather
FUN_MAT_RED <-function(
  Matrix_name    = "Global_Redundancy_matrix.xlsx",
  Selected_rows_ID,
  Variance){

  # Calculate the unique ID
  unique_ID <- unique (Selected_rows_ID)

    # load the redundancy matrix calculated by the Global_Redundancy_matrix function
    MAT<- readxl::read_excel(here::here("data","derived-data",Matrix_name))

  rownames(MAT)   <-tolower(rownames(MAT))
  colnames(MAT)   <-tolower(colnames(MAT))

  # subset the MAT with selected ID
  sub_MAT              <-  MAT %>% dplyr::filter(ma %in% tolower(unique(unique_ID)))
  sub_MAT              <-  sub_MAT %>% dplyr::select( tolower(unique(unique_ID)))
  sub_MAT              <-  base::subset(sub_MAT, select=order(colnames(sub_MAT)))

  # calculate the number on the diagonal
  NB<-data.frame(MA=colnames(sub_MAT),
                 NB=diag(data.matrix(sub_MAT)))

  sub_MAT             <- data.frame(data.matrix(sub_MAT)* 2)
  sub_MAT$ID          <- colnames(sub_MAT)
  MAT_spread          <- tidyr::gather(sub_MAT, "MA2", "RED", -ID)
  names(MAT_spread)[1]<- "MA1"
  MAT_spread$MA1<-  substring(MAT_spread$MA1, 2)
  MAT_spread$MA2<-  substring(MAT_spread$MA2, 2)


  # create a dataframe with all combination of selected ID
  COMBI           <- data.frame(MA1=rep(tolower(Selected_rows_ID), length(Selected_rows_ID)),
                                MA2=rep(tolower(Selected_rows_ID), each=length(Selected_rows_ID)))
  COMBI$Order    <-1: nrow(COMBI)

  # Merge the data frame
  MAT_RED             <- dplyr::left_join(COMBI,MAT_spread,by=c("MA1", "MA2"))
  MAT_RED             <- base::merge(MAT_RED, NB, by.x = c("MA1"), by.y = c("MA"))
  MAT_RED             <- base::merge(MAT_RED, NB, by.x = c("MA2"), by.y = c("MA"))
  MAT_RED$SUM         <- MAT_RED$NB.x+MAT_RED$NB.y
  MAT_RED$REDONDANCY  <- MAT_RED$RED/MAT_RED$SUM
  MAT_RED             <- MAT_RED  %>% dplyr::arrange(Order)
  MAT_RED             <- MAT_RED %>% dplyr::select(-NB.x, -NB.y, -RED, -SUM, -Order)

  MAT_RED<-MAT_RED %>% base::replace(is.na(.), 0)


  # When the ES belong to the same MA, we change the redundancy to 0
  for (j in 1: length(MAT_RED$MA1)){
    if(MAT_RED$MA1[j]==MAT_RED$MA2[j]){MAT_RED$REDONDANCY[j]<-0}
  }

  # Go back to matrix format
  LONG                    <- base::length(Selected_rows_ID)
  d                       <- base::split(MAT_RED,rep(1:LONG,each=LONG))
  MAT_RED_m               <- data.frame(matrix(ncol=LONG, nrow=LONG))
  for (j in 1: LONG){
    MAT_RED_m[j,]         <- d[[j]]$RED
  }

  # #Matrix of the vi
  # MAT_vi                  <-data.matrix(Variance)
  # MAT_vi                  <-sqrt(MAT_vi %*% t(MAT_vi))
  #
  # # We muliply the MAT_vi and the MAT_RED_m matrix
  # MAT_RED_FINALE          <-MAT_vi *MAT_RED_m

  #We add the variance on the diagonal
 # diag(MAT_RED_m)        <- Variance
  Pseudo_Variance        <- scales::rescale(Variance/max(Variance), to=c(0.1,0.95))
  diag(MAT_RED_m)        <- Pseudo_Variance


  # We have calculated the apparent correlation. We transform the correlation into covariance. Covariance= COR(x,y) *var(X)*var(Y)
  MAT_RED_m = t(t(MAT_RED_m * sqrt( Pseudo_Variance )) * sqrt( Pseudo_Variance ))

  #We add the variance on the diagonal (again)
  diag(MAT_RED_m)        <-  Pseudo_Variance

  MAT_RED_FINALE          <-data.frame(MAT_RED_m)
  return(MAT_RED_FINALE)
}
