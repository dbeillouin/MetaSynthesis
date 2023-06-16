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
Load_Data(File_name= "Data_Base_C_Sol_2023-15-05.xlsx",
            sheet_PS = "Primary_studies",
            sheet_desMA = "retained_meta-analyses",
            sheet_ES = "Effect-sizes")

# Check Data
Check_graphs(Name_File           = "RATIO",
             Name_Col_Outcome    = "Outcome",
             Outcome_of_interest =  "soil carbon",
             Intervention        = "Sub_Cat_intervention",
             Effect_size         = "Effect size")


File_name= "Data_Base_C_Sol_2023-15-05.xlsx"

RATIO <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx")) %>%
  dplyr::filter(Sub_cat_outcome %in% c("bulk soil" ),
                details_outcome %in% c("SOC stock", "SOC stock / SOC concentration", "SOC concentration"))



### Figure 1 : #######
Grouping_var         =c("Land_use", "Intervention","Sub_Cat_intervention","details")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_ALL_O_LU_IN_SubIN<-BEST_fit
ALL_fit_ALL_O_LU_IN_SubIN<-FINAL_freq
DATA_unES_DO2_LU_IN_SubIN <-DATABASE_un

## Run script Figure_1.R


####### Figure 2 #########
Grouping_var         =c("Land_use", "Intervention","Sub_Cat_intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_DO2_LU_IN_SubIN<-BEST_fit
ALL_fit_DO2_O_LU_IN_SubIN<-FINAL_freq
DATA_unES_DO2_LU_IN_SubIN <-DATABASE_un
# Run script Figure 2.r

# use the data: BEST_fit_ALL_O_LU_IN_SubIN, BEST_fit_DO2_LU_IN_SubIN
# to make the interactive table associated to Figure 2


## Figure 4.
### Per  Land_use, Sub_cat_intervention
Grouping_var         =c("Intervention","Sub_Cat_intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_IN_SubIN<-BEST_fit
ALL_fit_IN_SubIN<-FINAL_freq
DATA_unES_IN_SubIN <-DATABASE_un





###### Per details_outcome, Land_use, Intervention ########
Grouping_var         =c("details_outcome", "Land_use", "Intervention")
#Grouping_var         =c("Land_use", "Intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_Conc_Stock<-BEST_fit ; ALL_fit_Conc_Stock<-FINAL_freq

# comparison STOCK and Concentration values
p <- Compare_CONC_Stock(BEST_fit_Conc_Stock)
plotly::ggplotly(p)

source(here::here("analysis", "Bayesian_models.R"))
BEST_fit_Conc_Stock_BAY<-BEST_fit; ALL_fit_Conc_Stock_BAY<-FINAL_Bay

# comparison Frequentist and Bayesian methods
p<-Compare_Freq_Bay(BEST_fit_Conc_Stock, BEST_fit_Conc_Stock_BAY)
plotly::ggplotly(p[[1]])
reactable::reactable(p[[2]])


### Per details_outcome, Land_use, Intervention, Sub_cat_intervention
Grouping_var         =c("details_outcome", "Land_use", "Intervention","Sub_Cat_intervention")

source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_Sub_cat_int<-BEST_fit; ALL_fit_Sub_cat_int<-FINAL_freq

# source(here::here("analysis", "Bayesian_models.R"))
# BEST_fit_Sub_cat_int_BAY<-BEST_fit; ALL_fit_Sub_cat_int_BAY<-FINAL_Bay

# comparison Frequentist and Bayesian methods
p<-Compare_Freq_Bay(BEST_fit_Sub_cat_int, BEST_fit_Sub_cat_int_BAY)
p[[1]]
reactable::reactable(p[[2]])

# comparison linear and best_freq
Linear <- ALL_fit_Sub_cat_int %>% dplyr::select(names(ALL_fit_Sub_cat_int)[1:which(names(ALL_fit_Sub_cat_int)=="n_ES")-1],
                                                contains('Lin')) %>%
  dplyr::mutate("AIC$answer" = 'Lin')
names(Linear)[6:15]<-gsub("\\_.*","",names(Linear)[6:15])

p<-Compare_Freq_Bay(BEST_fit_Sub_cat_int, Linear)
p[[1]]
reactable::reactable(p[[2]])

# comparison linear and best_freq
Trim <- ALL_fit_Sub_cat_int %>% dplyr::select(names(ALL_fit_Sub_cat_int)[1:which(names(ALL_fit_Sub_cat_int)=="n_ES")-1],
                                                contains('Lin')) %>%
  dplyr::mutate("AIC$answer" = 'Lin')
names(Linear)[6:15]<-gsub("\\_.*","",names(Linear)[6:15])

p<-Compare_Freq_Bay(BEST_fit_Sub_cat_int, Linear)
p[[1]]
reactable::reactable(p[[2]])

## Analysis of publication bias
ggplot(DATABASE_Linear %>%
         dplyr::filter(!is.na(diffTF_lin)),
       aes(x=cut(diffTF_lin, breaks=seq(-30,30,1) ))) +
  geom_bar(binwidth=1)+
  ggpubr::theme_pubr()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Percent difference between linear and trim and fill method")


Data<-DATABASE_plu$data[[18]]
names(Data)[9]<- "ES"
mod_Bay<- brm(ES|se(vi, sigma=TRUE) ~ 1+ (1|ID),    data =  Data,    prior = priors,  chains = chains,    warmup = warmup,  iter = iter,    cores= workers,    future = TRUE,    thin = thin,  inits = 0)

mod_Freq<-metafor::rma.mv(ES,
                vi,data=Data,
                random=~1|ID,
                method="REML",
                control=list(optimizer="optim",
                             optmethod="BFGS"))


## Supplementary
### Analysis for all type of outcome (bulk, fractions) and all type of metrics (ratio and others)
Grouping_var         =c("details_outcome", "Land_use", "Intervention","Sub_Cat_intervention", "metric")

# RATIO <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx")) %>%
#   dplyr::filter(Sub_cat_outcome %in% c("bulk soil" ),
#                 details_outcome %in% c("SOC stock", "SOC stock / SOC concentration", "SOC concentration"))


RATIO  <-readxl::read_excel(here::here("data","derived-data","ALL_metrics.xlsx")) %>%
  dplyr::filter(!(Sub_cat_outcome %in% c("bulk soil" ) &
                 details_outcome %in% c("SOC stock", "SOC stock / SOC concentration", "SOC concentration") &
                   metric %in% c("Ratio"))) %>%
  dplyr::filter(Outcome == "soil carbon")


source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_ALL<-BEST_fit; ALL_fit_ALL<-FINAL_freq


library(reactable)
TAB<-BEST_fit %>% dplyr::select(-c(fit,Weights,AIC))
TAB$estimate<-round(TAB$estimate,2)
TAB$conf.low<-round(TAB$conf.low,2)
TAB$conf.high<-round(TAB$conf.high,2)
TAB$p.value<-round(TAB$p.value,3)
TAB$statistic<-round(TAB$statistic,3)
  TAB$std.error<-round(TAB$std.error,2)
reactable(TAB)


GROUP <- dplyr::group_by(TAB, details_outcome) %>%
  dplyr::summarize(Number = dplyr::n())

library(dplyr)
reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TAB, details_outcome == GROUP$details_outcome[index]) %>% select(-details_outcome)
    tbl <- reactable(sales, outlined = TRUE, highlight = TRUE, fullWidth = FALSE,
                     columns = list(
                       estimate = colDef(
                         cell = function(value) {
                           if (value >= 0) paste0("+", value) else value
                         },
                         style = function(value) {
                           color <- if (value > 0) {
                             "#008000"
                           } else if (value < 0) {
                             "#e00000"
                           }
                           list(fontWeight = 600, color = color)
                         }
                       )
                     ))
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)
#write.table(TAB %>% select(-data),'Suppl_All_effects.csv')


## Supplementary
### Analysis of soil depth
Grouping_var         =c("details_outcome", "Land_use", "Intervention","Sub_Cat_intervention", "group_depth")

RATIO  <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx")) %>%
  dplyr::filter(Sub_cat_outcome %in% c("bulk soil" ),
                    details_outcome %in% c("SOC stock", "SOC stock / SOC concentration", "SOC concentration"),
                    metric %in% c("Ratio"))


source(here::here("analysis", "Frequentist_models.R"))
BEST_fit_ALL<-BEST_fit; ALL_fit_ALL<-FINAL_freq


library(reactable)
TAB<-BEST_fit %>% dplyr::select(-c(fit,Weights,AIC)) %>%
  filter(! grepl(';', Sub_Cat_intervention))
TAB$estimate<-round(TAB$estimate,2)
TAB$conf.low<-round(TAB$conf.low,2)
TAB$conf.high<-round(TAB$conf.high,2)
TAB$p.value<-round(TAB$p.value,3)
TAB$statistic<-round(TAB$statistic,3)
TAB$std.error<-round(TAB$std.error,2)

GROUP <- dplyr::group_by(TAB, Sub_Cat_intervention) %>%
  dplyr::summarize(Number = dplyr::n())

#write.table(TAB %>% select(-data),'Suppl_soil_depth.csv')

library(dplyr)
reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TAB, Sub_Cat_intervention == GROUP$Sub_Cat_intervention[index]) %>% select(-Sub_Cat_intervention)
    tbl <- reactable(sales, outlined = TRUE, highlight = TRUE, fullWidth = FALSE,
                     columns = list(
                       estimate = colDef(
                         cell = function(value) {
                           if (value >= 0) paste0("+", value) else value
                         },
                         style = function(value) {
                           color <- if (value > 0) {
                             "#008000"
                           } else if (value < 0) {
                             "#e00000"
                           }
                           list(fontWeight = 600, color = color)
                         }
                       )
                     ))
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)


### Supp

TAB<-dplyr::bind_rows(BEST_fit_ALL_O_LU_IN_SubIN,DATABASE_un)%>%
  dplyr::mutate(Intervention = tolower(Intervention)) %>%
  dplyr::filter(Intervention %in% c("land use change",
                                    "management",
                                    "global changes")) %>%
  dplyr::mutate(Intervention= plyr::revalue(Intervention,c("management"= "land management")))%>%
  dplyr::mutate(Intervention = factor(Intervention,
                                      levels=rev(c("land management",
                                                   "land use change",
                                                   "global changes")))) %>%
  dplyr::mutate(estimate = (exp(estimate)-1)*100) %>%
  filter(grepl(';', Sub_Cat_intervention)|
           Sub_Cat_intervention =="organic farming")

TAB<-TAB %>% dplyr::select(-c(fit,Weights,AIC,data))
GROUP <- dplyr::group_by(TAB, `Sub_Cat_intervention`) %>%
  dplyr::summarize(Number = dplyr::n())


TAB$estimate<- as.numeric(as.character(TAB$estimate))
TAB$conf.low<-round(((TAB$conf.low)),1)
TAB$conf.high<-round(((TAB$conf.high)),1)
TAB$estimate<-round(((TAB$estimate)),1)
TAB$statistic<-round(TAB$statistic,2)
TAB$std.error<-round(((TAB$std.error)),1)
TAB$p.value<-round(((TAB$p.value)),3)

#write.table(TAB,'Combi_practices.csv')


TAB$details[is.na(TAB$details)] <- "Global effect"
library(dplyr)
reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TAB, `Sub_Cat_intervention` == GROUP$`Sub_Cat_intervention`[index]) %>% select(-`Sub_Cat_intervention`)
    tbl <- reactable(sales, outlined = TRUE, highlight = TRUE, fullWidth = FALSE,
                     columns = list(
                       estimate = colDef(
                         cell = function(value) {
                           if (value >= 0) paste0("+", value) else value
                         },
                         style = function(value) {
                           color <- if (value > 0) {
                             "#008000"
                           } else if (value < 0) {
                             "#e00000"
                           }
                           list(fontWeight = 600, color = color)
                         }
                       )
                     ),
                     defaultPageSize = 25,
                     rowStyle = function(index) {
                       if (sales[index, "details"]== "Global effect") {
                         list(background = "rgba(0, 0, 0, 0.05)")
                       }
                     }
    )
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer"),
)

