#' Meta-Analysis on SOC project
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Beillouin Damien \email{beillouin@cirad.Fr}
#'
#' @date 2023/02/20



## Install Dependencies (listed in DESCRIPTION) ----

if (!("remotes" %in% installed.packages()[ , "Package"]))
  install.packages("remotes")

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Create sub-folders ----

dir.create(here::here("data", "derived-data"), showWarnings = FALSE)
dir.create(here::here("outputs"), showWarnings = FALSE)


## Run Project ----

# Load Data
Load_Data(File_name= "Data_Base_C_Sol_2023-02-18.xlsx",
            sheet_PS = "Primary_studies",
            sheet_desMA = "retained_meta-analyses",
            sheet_ES = "Effect-sizes")

# Check Data
Check_graphs(Name_File           = "RATIO",
             Name_Col_Outcome    = "Outcome",
             Outcome_of_interest =  "soil carbon",
             Intervention        = "Sub_Cat_intervention",
             Effect_size         = "Effect size")

# Frequentist Analysis

RATIO  <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx")) %>%
  dplyr::filter(Sub_cat_outcome %in% c("Bulk soil" ),
                !details_outcome =='SOC dynamics')

### Per details_outcome, Land_use, Intervention
Grouping_var         =c("details_outcome", "Land_use", "Intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_DO_LU_IN<-BEST_fit
ALL_fit_DO_LU_IN<-FINAL_freq

### Per details_outcome, Land_use, Intervention, Sub_cat_intervention
Grouping_var         =c("details_outcome", "Land_use", "Intervention","Sub_Cat_intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_DO_LU_IN_SubIN<-BEST_fit
ALL_fit_DO_LU_IN_SubIN<-FINAL_freq

### Per  Land_use, Intervention, Sub_cat_intervention
Grouping_var         =c("Land_use", "Intervention","Sub_Cat_intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_DO2_LU_IN_SubIN<-BEST_fit
ALL_fit_DO2_O_LU_IN_SubIN<-FINAL_freq
DATA_unES_DO2_LU_IN_SubIN <-DATABASE_un


### Per  Land_use, Intervention, Sub_cat_intervention
Grouping_var         =c("Intervention","Sub_Cat_intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_IN_SubIN<-BEST_fit
ALL_fit_IN_SubIN<-FINAL_freq
DATA_unES_IN_SubIN <-DATABASE_un


### Per  Land_use, Intervention, Sub_cat_intervention, details
Grouping_var         =c("Land_use", "Intervention","Sub_Cat_intervention","details")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_ALL_O_LU_IN_SubIN<-BEST_fit
ALL_fit_ALL_O_LU_IN_SubIN<-FINAL_freq

#### Merge all Dataframe
BEST_fit2<- dplyr::bind_rows(dplyr::bind_rows(BEST_fit_DO_LU_IN,BEST_fit_DO_LU_IN_SubIN),BEST_fit_ALL_O_LU_IN_SubIN)
# We also add the effect when only 1 MA was present
BEST_fit2<-dplyr::bind_rows(BEST_fit,DATABASE_un)

writexl::write_xlsx(BEST_fit2,
                    path = here::here("data", "derived-data",
                                      "BEST_fit.xlsx"))
