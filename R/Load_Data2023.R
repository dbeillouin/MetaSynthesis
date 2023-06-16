#' @title Load data from Excel file
#' @description This function loads data from an Excel file and returns three data frames: primary studies, retained meta-analyses, and effect sizes.
#' @param File_name Name of the Excel file to load data from. Default: 'Data_Base_C_Sol_2023-02-18.xlsx'
#' @param sheet_PS Name of the sheet in the Excel file containing primary studies data. Default: 'Primary_studies'
#' @param sheet_desMA Name of the sheet in the Excel file containing retained meta-analyses data. Default: 'retained_meta-analyses'
#' @param sheet_ES Name of the sheet in the Excel file containing effect sizes data. Default: 'Effect-sizes'
#' @return A list containing three data frames: primary studies, retained meta-analyses, and effect sizes.
#' @details The function reads data from an Excel file, formats it, and returns three data frames: primary studies, retained meta-analyses, and effect sizes.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[here]{here}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate_all}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[forcats]{fct_recode}}
#' @importFrom readxl read_excel
#' @importFrom here here
#' @importFrom dplyr mutate mutate_at group_by
#' @importFrom forcats fct_recode
#' @importFrom magrittr  %>%
#' @export
Load_Data<- function(File_name= "Data_Base_C_Sol_2023-15-05.xlsx",
                     sheet_PS = "Primary_studies",
                     sheet_desMA = "retained_meta-analyses",
                     sheet_ES = "Effect-sizes"){

#  Load Primary studies
IND_STU  <-       readxl::read_excel(here::here("data","raw-data",File_name), sheet=sheet_PS) %>%
  dplyr::mutate(ID = tolower(gsub(",", ".", ID)),
                ID = gsub(" ", "_", ID, fixed = TRUE))

# Load Description of the meta-analyses
Des_MA <-  readxl::read_excel(here::here("data","raw-data",File_name), sheet=sheet_desMA) %>%
  dplyr::mutate(ID = tolower(gsub(",", ".", ID)),
         ID = gsub(" ", "_", ID, fixed = TRUE))

# Load Quality of the meta-analyses
QUAL <- Des_MA %>%
  dplyr::select("ID",starts_with("Quality")) %>%
  dplyr::mutate_at(.vars = dplyr::vars(contains("Quality"),
                          -dplyr::contains("Quality_Lit_Nb_database")),
            .funs = dplyr::funs(ifelse(grepl("YES|Yes|yes", .), 1, 0))) %>%
  dplyr::mutate(Quality_Lit_Nb_database = as.numeric(Quality_Lit_Nb_database),
                Quality_Lit_Nb_database= dplyr::case_when(Quality_Lit_Nb_database >1  ~ 1,TRUE ~0))

# mean score of the meta-analyses
QUAL <-QUAL %>% dplyr::group_by(ID) %>%
  dplyr::mutate(SCORE = sum(dplyr::c_across(where(is.numeric))))

# Load Effect-sizes
ES<- readxl::read_excel(here::here("data","raw-data",File_name), sheet=sheet_ES)   %>%
       dplyr::mutate (`Effect size`  = as.numeric(gsub(",", ".", `Effect size`)),
          lower_CI       = as.numeric(gsub(",", ".", lower_CI)),
          upper_CI       = as.numeric(gsub(",", ".", upper_CI)),
          ES_SE          = as.numeric(gsub(",", ".", ES_SE)),
          ID             = tolower(gsub(",", ".", ID)),
          ID = gsub(" ", "_", ID, fixed = TRUE),
          Sub_Cat_intervention = trimws(tolower(Sub_Cat_intervention)),
          Land_use       = tolower(Land_use),
          Intervention   = tolower(Intervention),
          Outcome        = trimws(tolower(Outcome)),
          Sub_cat_outcome= tolower(Sub_cat_outcome))

#### II/ Format the data ######

# Convert Relative différence -> Ratio
ES <- ES%>% dplyr::mutate(`Effect size`          =
                     ifelse(metric %in% c("Percent change"),
                            `Effect size`/100+1,
                            `Effect size`),
                   lower_CI =
                     ifelse(metric %in% c("Percent change"),
                            lower_CI/100+1,
                            lower_CI),
                   upper_CI =
                     ifelse(metric %in% c("Percent change"),
                            upper_CI/100+1,
                            upper_CI),
                   ES_SE       =
                     ifelse(metric %in% c("Percent change"),
                            ES_SE+1,
                            ES_SE)     ) %>%
  dplyr::mutate(metric = forcats::fct_recode(metric, "Ratio" = "Percent change"))


# Convert Ratio -> log (Ratio)
ES<-ES %>% dplyr::mutate(`Effect size`          =
                    ifelse((metric %in% c("Ratio") & Log_scale == "NO"),
                           log(`Effect size`),
                           `Effect size`),
                  lower_CI =
                    ifelse((metric %in% c("Ratio")& Log_scale == "NO"),
                           log(lower_CI),
                           lower_CI),
                  upper_CI =
                    ifelse((metric %in% c("Ratio")& Log_scale == "NO"),
                           log(upper_CI),
                           upper_CI) )

# Convert from Fisher to cohen's
ES  <- ES  %>%
  dplyr::mutate(lower_CI=
           ifelse(metric=="Fisher's Z",
                  `Effect size` - FUN_CI_from_var(
                    Var= FUN_var_r_to_d(Var_r =
                                          FUN_var_from_CI(mean = `Effect size`,CI=lower_CI),
                                        r = `Effect size` )),
                  lower_CI))

ES <- ES %>%
  dplyr::mutate(upper_CI=
           ifelse(metric=="Fisher's Z",
                  `Effect size`+FUN_CI_from_var(
                    Var= FUN_var_r_to_d(Var_r =
                                          FUN_var_from_CI(mean=`Effect size`,CI=upper_CI),
                                        r     = `Effect size` )),
                  upper_CI))

ES <- ES %>%
  dplyr::mutate(`Effect size`=
           ifelse(metric=="Fisher's Z" ,
                  FUN_r_to_d(FUN_Z_to_r(`Effect size`)),
                  `Effect size`))
ES<- ES%>% dplyr::mutate(metric = forcats::fct_recode(metric,
                                      "Cohen's d" = "Fisher's Z"))


# mean of the CI
## The two confidence intervals are averaged and the average value for the bounds is assigned
ES$CI_moy   <- (abs(ES$`Effect size`- ES$lower_CI)+ abs(ES$upper_CI - ES$`Effect size`))/2
ES$lower_CI <- ES$`Effect size` - ES$CI_moy
ES$upper_CI <- ES$`Effect size` + ES$CI_moy


## change the order of the ES if necessary
for (i in 1: length(ES$Inverse_ES)){
  if(ES$Inverse_ES[i]=='YES'){
    ES$lower_CI[i]<- -ES$lower_CI[i]}}
for (i in 1: length(ES$Inverse_ES)){
  if(ES$Inverse_ES[i]=='YES'){
    ES$`Effect size`[i]<- -ES$`Effect size`[i]}}
for (i in 1: length(ES$Inverse_ES)){
  if(ES$Inverse_ES[i]=='YES'){
    ES$upper_CI[i]<- -ES$upper_CI[i]}}

# supress useless files
rm(TMP); rm(RR)


### III/ Select the data and prepare the file for analysis####

# Select main  effect-sizes
ES_TMP<-ES                          %>%
  dplyr::filter(Main_effect=="KEEP")       %>%  # We analyse only "main" effect sizes (not sub-scenarios)
  dplyr::select(ID,Land_use,Intervention,Sub_Cat_intervention, details,
         metric,Outcome,Sub_cat_outcome,details_outcome, Homogenized_response,N_paired_data,
         lower_CI, `Effect size`, upper_CI, depth2, group_depth)%>% # select some columns
  dplyr::mutate(vi= FUN_var_from_CI(mean=`Effect size`, CI=upper_CI))



# Final table for analyses
# Create a unique ID per row
ES_TMP<-ES_TMP %>%
  dplyr::mutate(NUM= as.character(1: length(ID)))

# Suppress lines with no variance
ES_TMP <- ES_TMP %>% dplyr::filter(!is.na(vi))
RATIO  <- ES_TMP %>% dplyr::filter(metric=="Ratio")%>% dplyr::filter(!is.na(vi))

# merge Final table and quality score
RATIO<- merge(RATIO, QUAL %>% dplyr::select(c('ID', 'SCORE')))
ES_TMP<- merge(ES_TMP, QUAL %>% dplyr::select(c('ID', 'SCORE')))

COHEN<- ES_TMP %>% dplyr::filter(metric=="Cohen's d")%>% dplyr::filter(!is.na(vi))
COHEN<- COHEN %>% dplyr::filter(!is.na(vi))

LIST_ID<-unique(c(COHEN$ID, RATIO$ID))

# supress useless files
rm(TMP, SCORE)
writexl::write_xlsx(RATIO,
                    path = here::here("data","derived-data",
                                      "RATIO.xlsx"))
writexl::write_xlsx(ES_TMP,
                    path = here::here("data","derived-data",
                                      "ALL_metrics.xlsx"))
}
