# Load Packages
library(stringr)


## Load table for online table
TAB_LUC<-dplyr::bind_rows(BEST_fit_ALL_O_LU_IN_SubIN, BEST_fit_DO2_LU_IN_SubIN)

TAB_LUC<-TAB_LUC %>% filter(Intervention =='land use change') %>%
  tidyr::separate(Sub_Cat_intervention, c("first", "second"), sep= '->') %>%
  filter(!first  =="various land uses ") %>%
  filter(!second  ==" various land uses")

TAB_LUC$estimate<- as.numeric(as.character(TAB_LUC$estimate))
TAB_LUC$conf.low<-round((exp(TAB_LUC$conf.low)-1)*100,1)
TAB_LUC$conf.high<-round((exp(TAB_LUC$conf.high)-1)*100,1)
TAB_LUC$estimate<-round((exp(TAB_LUC$estimate)-1)*100,1)
TAB_LUC$std.error<-round((exp(TAB_LUC$std.error)-1)*100,1)
TAB_LUC$p.value<-round((exp(TAB_LUC$p.value)-1)*100,3)

library(reactable)
TAB<-TAB_LUC %>% dplyr::select(-c(fit,Weights,AIC))
names(TAB)[3]<-"initial land use"
names(TAB)[4]<-"final land use"
reactable(TAB)


# create individual plot for Figure 2
Cropland <-data.frame(Nb=c(839,1230,368,359,1603),
                      label=c(1,2,3,4,5))
count.data <- Cropland %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)
sum(Cropland$Nb)

ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label))) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 180)+
  #geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#ffa100","#ac8b67","#fff7eb","#dfe0df","#b9a89b")) +
  theme_void()+
  xlim(0.5, 2.5)



Forest <-data.frame(Nb=c(839,685,1009,1230),
                      label=c(1,2,3,4))
count.data <- Forest %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)
sum(Forest$Nb)

ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label))) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 150)+
 # geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#006700","#7b9971","#f0fdec","#fffbce")) +
  theme_void()+
  xlim(0.5, 2.5)



grassland <-data.frame(Nb=c(1603,359,1009,685),
                    label=c(1,2,3,4))
count.data <- grassland %>%
  arrange(desc(label)) %>%
  mutate(FF = cumsum(Nb) - 0.5*Nb)
sum(grassland$Nb)

ggplot(count.data, aes(x = 2, y = Nb, fill = as.character(label)) )+
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 2)+
#  geom_text(aes(y = FF, label = Nb), color = "black")+
  scale_fill_manual(values = c("#00ff00","#7b9971","#f5fbf2","#21351b")) +
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

