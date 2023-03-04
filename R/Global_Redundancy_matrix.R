
# Global_Redundancy_matrix(file_name= "Data_Base_C_Sol_2023-02-18.xlsx",
#                          sheet_name= "Primary_studies",
#                          ID= "ID",DOI = "DOI")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file_name Names of the file (Excel file)
#' @param sheet_name Name of the sheet where the primary studies are.
#' @param ID Names of the column for the ID of the meta-analyses
#' @param DOI Names of the column for the identifier of the primary studies
#' @param FACTOR Factor for the Meta-analyses with no available Primary studies.Between 0 and 1. 0-> MA with no studies = no redundance with the others, 1-> MA with no studies= 100% redundant with the others
#' @return return a matrix of redudancy between all Meta-analyses (in count, not percent)
#' @return return a matrix of redudancy between all Meta-analyses (in count, not percent)
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[here]{here}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{filter}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{coord_flip}}, \code{\link[ggplot2]{labs}}
#'  \code{\link[data.table]{dcast.data.table}}, \code{\link[data.table]{fwrite}}
#' @rdname Global_Redundancy_matrix
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom here here
#' @importFrom dplyr mutate distinct group_by tally filter
#' @importFrom ggplot2 ggplot geom_col coord_flip labs
#' @importFrom data.table dcast fwrite
Global_Redundancy_matrix<-function(
                  File_name = "Data_Base_C_Sol_2023-02-18.xlsx",
                  sheet_name = "Primary_studies",
                  ID= "ID",
                  DOI = "DOI",
                  FACTOR = 1){

  # Load Data
  PS<-  readxl::read_xlsx(here::here("data","raw-data",File_name),
                          sheet = sheet_name) %>%
    dplyr::mutate(ID = tolower(gsub(",", ".", ID)),
                  ID = gsub(" ", "_", ID, fixed = TRUE))

  # On ne conserve que les Primary studies  unique par meta-analyse
  PS<- PS %>% dplyr::distinct(ID, DOI)

  # verif: on fait le point sur le nombre d'études
  COUNT<-PS %>% dplyr::group_by(ID) %>% dplyr::tally()
  ggplot2::ggplot(COUNT, ggplot2::aes(x = reorder(ID,n), y = n, group = ID)) +
    ggplot2::geom_col(position="dodge")+
    ggplot2::coord_flip()+
    ggplot2::labs(y="Number of primary studies", x="Meta-analysis")

  # Redundancy Matrix
  w <- data.table::dcast(data.table::melt(PS), ID~DOI)
  x <- as.matrix(w[,-1])
  x[is.na(x)] <- 0
  x <- apply(x, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
  v <- x %*% t(x)                                   #the magic matrix
  dimnames(v) <- list(w[, 1], w[,1])                #name the dimensions
  Nb<- diag(v)
  NAMES<-rownames(v)
  TAB<-data.frame(v)
  colnames(TAB)<-NAMES


  TAB$Nb<-Nb

  #  colnames(TAB)[1]<-"NA"
  # TAB<- TAB %>%
  #  mutate_at(vars(starts_with("MA")), function (x) x/TAB$Nb*100)

  #TAB<-TAB %>% select(-Nb)

  TAB$MA<-  NAMES
  # identify all Meta-analyses with no available studies
  MA_noPS<-COUNT %>% dplyr::filter(n==1)
  MA_noPS<-MA_noPS$ID
  for (i in 1 : length(NAMES)){
    if (NAMES[i] %in% MA_noPS){ TAB[,i]<-round(Nb*2*FACTOR)}else{TAB[,i]<-TAB[,i]}
    if (NAMES[i] %in% MA_noPS){ TAB[i,1:(length(NAMES)-2)]<-round(Nb*2*FACTOR)}else{TAB[i,]<-TAB[i,]}

  }


    writexl::write_xlsx(TAB,
                      path = here::here("data","derived-data",
                                        "Global_Redundancy_matrix.xlsx"))

}
