# Load Packages
library(ggExtra)
library(ggstar)
library(ggplot2)
library(dplyr)
library(reactable)

#  Load Primary studies
File_name= "Data_Base_C_Sol_2023-03-04.xlsx"
sheet_PS = "Primary_studies"
LAND_use<- "cropland"

Color= "#ff9d02"
LIM= c(-10,140)

#color for Cropland: ##ff9d02/ LIM= c(-10,140)
#color for grassland: #f9f871/ LIM= c(-50,75)
#color for forestland: #006a00/ LIM= c(-65,45)

IND_STU  <-       readxl::read_excel(here::here("data","raw-data",File_name), sheet=sheet_PS) %>%
  dplyr::mutate(ID = tolower(gsub(",", ".", ID)),
                ID = gsub(" ", "_", ID, fixed = TRUE))


BEST_fit2<-dplyr::bind_rows(BEST_fit_DO2_LU_IN_SubIN,BEST_fit_ALL_O_LU_IN_SubIN)

MEAN<-BEST_fit2 %>% filter(Intervention =='management') %>%
  filter(!grepl(';', Sub_Cat_intervention)) %>%
  filter(!Sub_Cat_intervention %in% c("organic farming",'other')) %>%
  group_by(Land_use) %>%
  arrange(Land_use,-estimate) %>%  ungroup() %>%
  mutate(position = 1:nrow(.)) %>%
  mutate(Sub_Cat_intervention= firstup(Sub_Cat_intervention))

TAB_R_PUb<-MEAN %>% dplyr::select(-c(data, position, Weights, fit))

TAB_R_PUb$estimate<- as.numeric(as.character(TAB_R_PUb$estimate))
TAB_R_PUb$conf.low<-round((exp(TAB_R_PUb$conf.low)-1)*100,1)
TAB_R_PUb$conf.high<-round((exp(TAB_R_PUb$conf.high)-1)*100,1)
TAB_R_PUb$estimate<-round((exp(TAB_R_PUb$estimate)-1)*100,1)
TAB_R_PUb$statistic<-round(TAB_R_PUb$statistic,2)
TAB_R_PUb$std.error<-round((exp(TAB_R_PUb$std.error)-1)*100,1)
TAB_R_PUb$p.value<-round((exp(TAB_R_PUb$p.value)-1)*100,3)


TAB<-TAB_R_PUb %>% dplyr::select(-c(AIC))
names(TAB)[13]<-"model"
TAB<-TAB %>% dplyr::select(-c('Intervention'))
TAB<-TAB%>% relocate("details", .after = "Sub_Cat_intervention")

GROUP <- dplyr::group_by(TAB, `Land_use`) %>%
  dplyr::summarize(Number = dplyr::n())


TAB<-TAB%>% arrange(`Land_use`,`Sub_Cat_intervention`,`details`)

TAB %<>% mutate(Sub_Cat_intervention = forcats::fct_recode(Sub_Cat_intervention,
                                             "Biochar" ="Amendments pyrogenic",
                                             "Amendments"= "Amendments non-pyrogenic",
                                             "Replace Fert. by Amend." =  "Amendments non-pyrogenic substitution",
                                             "Tillage reduction" = "Tillage",
                                             "Residues" = "Residues management",
                                             "Perennial crops/orchards" = "Perenial crops/orchards",
                                             "Fertilization" = "Mineral fertilization"))




reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TAB, `Land_use` == GROUP$`Land_use`[index]) %>% select(-`Land_use`)
    tbl <- reactable(sales,  groupBy = "Sub_Cat_intervention",outlined = TRUE, highlight = TRUE, fullWidth = TRUE,
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
                     rowStyle = function(index) {
                       if (is.na(sales[index, "details"])) {
                         list(background = "rgba(0, 0, 0, 0.05)")
                       }
                     }
                     )
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)


## Database with indidivual effects
INd_effects <-   RATIO %>% mutate(Sub_Cat_intervention= firstup(Sub_Cat_intervention)) %>%
                       filter(Intervention =='management') %>%
                       filter(!grepl(';', Sub_Cat_intervention)) %>%
                      filter(!details_outcome =='SOC dynamics') %>%
                       filter(!Sub_Cat_intervention %in% c("Organic farming",'Other'))
INd_effects$estimate<-round((exp(INd_effects$`Effect size`)-1)*100,1)
#names(INd_effects)[13]<-'estimate'

## Calculation of redudancy

TAB_Land_use<- MEAN %>% dplyr::filter(Land_use %in%  LAND_use)
list<-TAB_Land_use$Sub_Cat_intervention

for ( i in 1: length(list)){
  TAB<-RATIO %>% filter(Land_use ==LAND_use) %>%
    mutate(Sub_Cat_intervention =tolower(Sub_Cat_intervention)) %>%
    dplyr::filter(Sub_Cat_intervention == tolower(list[i]))
  ID_select<-unique(TAB$ID)
  IND_select<- IND_STU %>% filter(ID %in% ID_select )

  TAB_Land_use$Prop[i] <- (1- table(table(IND_select$DOI))[1]/ sum( table(table(IND_select$DOI))))*100

  TAB2<-data.frame(table(IND_select$ID)) %>%
    dplyr::filter(Freq<2)

  TAB_Land_use$NB_0[i] <- length(TAB2$Var1)
}

## We simplify some names
# TAB_Land_use %<>% mutate(Sub_Cat_intervention = forcats::fct_recode(Sub_Cat_intervention,
#                                              "Biochar" ="Amendments pyrogenic",
#                                              "Amendments"= "Amendments non-pyrogenic",
#                                              "Replace Fert. by Amend." =  "Amendments non-pyrogenic substitution",
#                                              "Tillage reduction" = "Tillage",
#                                              "Residues" = "Residues management",
#                                              "Perennial crops/orchards" = "Perenial crops/orchards",
#                                              "Fertilization" = "Mineral fertilization"))
#
# INd_effects %<>% mutate(Sub_Cat_intervention = forcats::fct_recode(Sub_Cat_intervention,
#                                                             "Biochar" ="Amendments pyrogenic",
#                                                             "Amendments"= "Amendments non-pyrogenic",
#                                                             "Replace Fert. by Amend." =  "Amendments non-pyrogenic substitution",
#                                                             "Tillage reduction" = "Tillage",
#                                                             "Residues" = "Residues management",
#                                                             "Perennial crops/orchards" = "Perenial crops/orchards",
#                                                             "Fertilization" = "Mineral fertilization"))


# For the plots, we select only main calculated effect-sizes.
TAB_Land_use %<>% filter(is.na(details))


TAB_Land_use$estimate<- as.numeric(as.character(TAB_Land_use$estimate))
TAB_Land_use$conf.low<-round((exp(TAB_Land_use$conf.low)-1)*100,1)
TAB_Land_use$conf.high<-round((exp(TAB_Land_use$conf.high)-1)*100,1)
TAB_Land_use$estimate<-round((exp(TAB_Land_use$estimate)-1)*100,1)
TAB_Land_use$std.error<-round((exp(TAB_Land_use$std.error)-1)*100,1)
TAB_Land_use$p.value<-round((exp(TAB_Land_use$p.value)-1)*100,3)

TAB2 <- TAB_Land_use %>% filter(Land_use== LAND_use)
INd_effects<-INd_effects %>% filter(Land_use== LAND_use)


p<-ggplot2::ggplot(TAB2,aes(estimate,reorder(Sub_Cat_intervention, estimate)))+
  ggstar::geom_star(starshape=12, size=4,starstroke=1.1)+
   ggplot2::geom_point(data=INd_effects, aes(group=Sub_Cat_intervention ,
                                     size=as.numeric(N_paired_data)),
                       shape=21,alpha=0.8,fill=Color,
                       position = position_dodge(width = 0.6))+
  ggstar::geom_star(starshape=12, size=4,starstroke=1.1)+
  ggplot2::geom_errorbar(aes(xmin=conf.low,xmax= conf.high),color="black",
                size=0.2,width = 0.01)+
  ggplot2::geom_vline(aes(xintercept = 0),linetype=2, color="gray")+
  ggpubr::theme_pubr()+
 # ggrepel::geom_text_repel(aes(label=stringr::str_wrap(Sub_Cat_intervention, 10)))+
  #ggrepel::geom_text_repel(aes(label=Sub_Cat_intervention))+
  xlim(LIM)+
  theme(legend.position="none")+
  labs(y="",x ="SOC change (percent)")


p1 <- ggMarginal(p, type="histogram", fill= Color, alpha=0.2)


TAB<-TAB2 %>%
  group_by(Sub_Cat_intervention) %>%
  summarise(total = sum(n_data, na.rm=TRUE),
            est    = mean(estimate))


coeff<-0.02
p2<-ggplot(TAB,aes(y = reorder(Sub_Cat_intervention,est), x = total)) +
  geom_bar(stat = "identity", fill=Color, color="black",alpha=0.2)+
  ggpubr::theme_pubr()+
  labs(y="")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
    geom_line(data=TAB_Land_use,
               aes(reorder(Sub_Cat_intervention,estimate),
                                      x = as.numeric(Prop)/coeff), group=1, color="blue")+
  scale_x_continuous(

    # Features of the first axis
    name = "# primary studies (bar)",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="% common \n primary studies (line)",
     breaks = seq(0,100,50)),
    breaks=seq(0, 5000, 2500)
  )+
  theme(legend.position="none",
        plot.margin=unit(c(1.3,1,0.4,-2), "cm"),
        axis.title=element_text(size=9),
        axis.text=element_text(size=9))

cowplot::plot_grid(p1,p2,rel_widths = c(10,1), align='h')



