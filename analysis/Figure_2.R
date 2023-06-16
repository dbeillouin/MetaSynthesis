# Load Packages
library(stringr)


## Load table for online table
TAB_LUC<-dplyr::bind_rows(BEST_fit_ALL_O_LU_IN_SubIN, BEST_fit_DO2_LU_IN_SubIN)


TAB_LUC<-TAB_LUC %>% dplyr::filter(Intervention =='land use change') %>%
  tidyr::separate(Sub_Cat_intervention, c("first", "second"), sep= '->') %>%
  dplyr::filter(!first  =="various land uses ") %>%
  dplyr::filter(!second  ==" various land uses")

TAB_LUC$estimate<- as.numeric(as.character(TAB_LUC$estimate))
TAB_LUC$conf.low<-round((exp(TAB_LUC$conf.low)-1)*100,1)
TAB_LUC$conf.high<-round((exp(TAB_LUC$conf.high)-1)*100,1)
TAB_LUC$estimate<-round((exp(TAB_LUC$estimate)-1)*100,1)
TAB_LUC$statistic<-round(TAB_LUC$statistic,2)
TAB_LUC$std.error<-round((exp(TAB_LUC$std.error)-1)*100,1)
TAB_LUC$p.value<-round((exp(TAB_LUC$p.value)-1)*100,3)

library(reactable)
TAB<-TAB_LUC %>% dplyr::select(-c(fit,Weights,AIC))
TAB<-TAB %>% dplyr::select(-c('Land_use','Intervention','data'))
names(TAB)[1]<-"initial land use"
names(TAB)[2]<-"final land use"
names(TAB)[13]<-"model"
reactable(TAB)

TAB$details[is.na(TAB$details)] <- "Global effect"

GROUP <- dplyr::group_by(TAB, `initial land use`) %>%
  dplyr::summarize(Number = dplyr::n())



library(dplyr)
reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TAB, `initial land use` == GROUP$`initial land use`[index]) %>% select(-`initial land use`)
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



# create individual plot for Figure 2
Cropland <-data.frame(Nb=c(1560,711,368,359,1593),
                      label=c(1,2,3,4,5))
count.data <- Cropland %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)
sum(Cropland$Nb)

ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label))) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 180)+
  #geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#fdbb84","#e5f5f9","#0093ff","#fee8c8","#ffffe5")) +
  theme_void()+
  xlim(0.5, 2.5)



Forest <-data.frame(Nb=c(1650,1186,626,711),
                      label=c(1,2,3,4))
count.data <- Forest %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)
sum(Forest$Nb)

ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label))) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 150)+
 # geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#fdbb84","#fff7bc","#99d8c9","#e5f5f9")) +
  theme_void()+
  xlim(0.5, 2.5)



grassland <-data.frame(Nb=c(1593,359,626,1186),
                    label=c(1,2,3,4))
count.data <- grassland %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)
sum(grassland$Nb)

ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label)) )+
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 2)+
 # geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#ffffe5","#fee8c8","#99d8c9","#fff7bc")) +
  theme_void()+
  xlim(0.5, 2.5)

wetlands <-data.frame(Nb=c(368),
                       label=c(1))
count.data <- wetlands %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)


ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label))) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 150)+
#  geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#0093ff")) +
  theme_void()+
  xlim(0.5, 2.5)

