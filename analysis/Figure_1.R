##package
library(ggplot2)
library(ggforce)
library(ggpubr)
library(plotly)

###### Load Data


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
    dplyr::mutate(estimate = (exp(estimate)-1)*100)  %>%
    tidyr::separate(Sub_Cat_intervention, c("first", "second"),sep='->') %>%
    dplyr::mutate(Land_use = factor(ifelse(Land_use == "land_use change", first, Land_use)))%>%
    dplyr::mutate(Land_use =as.character(trimws(trimws(Land_use))))

  TAB$Land_use<- dplyr::recode_factor(TAB$Land_use,"other land/ various land uses" = "other land",
                                                       "various land uses"            = "other land",
                                                       "shrublands"                   ="other land")


# Calculate the total number of land use (i.e include land-use change)
MGT_CC <- TAB %>%
  dplyr::group_by(Land_use) %>%
  dplyr::summarize(n1 = sum(n_data, na.rm=TRUE))%>%
  dplyr::filter(!Land_use  =="land_use change")

LUC<-TAB %>%
  dplyr::filter(Intervention  =="land use change") %>%
  dplyr::mutate(second =as.character(trimws(trimws(second)))) %>%
  dplyr::group_by(second) %>%
  dplyr::summarize(n2 = sum(n_data, na.rm=TRUE))
names(LUC)[1]<-"Land_use"


ALL<-dplyr::left_join(MGT_CC,LUC) %>%
dplyr::mutate("total" = rowSums((.[,2:3]), na.rm = TRUE))

# function to draw imbricated circles
circle <- function(center, radius, group) {
  th <- seq(0, 2*pi, len=200)
  data.frame(group=group,
             x=center[1] + radius*cos(th),
             y=center[2] + radius*sin(th))
}

# Create a named vector for your values
obs <- c(Cropland=67, Forest_land=46, Grassland=31, Other_land=18, Wetlands= 4)

# this reverse sorts them (so the stacked layered circles work)
# and makes it a list
obs <- as.list(rev(sort(obs)))

# need the radii
rads <- lapply(obs, "/", 2)

# need the max
x <- max(sapply(rads, "["))

# build a data frame of created circles
do.call(rbind.data.frame, lapply(1:length(rads), function(i) {
  circle(c(x, rads[[i]]), rads[[i]], names(rads[i]))
})) -> TAB_Land

# make the plot for the Land_use
plot_Land <- ggplot(TAB_Land)+
  geom_polygon(aes(x=x, y=y, group=group, fill=group),
                        color="black")+
  scale_fill_manual(values = c("orange","darkgreen", "green" ,'gray',"blue"))+
  coord_equal()+
  theme_pubr()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+
  theme(legend.position="none")

#### Make the plot for the Intervention

# Calculate the total number of intervention (i.e include land-use change)
INT <- TAB %>%
  dplyr::group_by(Intervention) %>%
  dplyr::summarize(n1 = sum(n_data,na.rm=TRUE))

# Create a named vector for your values
obs <- c(Cropland=75.8, Forest_land=20.3, Grassland=3.8)
### WARNING. The name are not the good ones, but the function does not work otherwise...

# this reverse sorts them (so the stacked layered circles work)
# and makes it a list
obs <- as.list(rev(sort(obs)))

# need the radii
rads <- lapply(obs, "/", 2)

# need the max
x <- max(sapply(rads, "["))

# build a data frame of created circles
do.call(rbind.data.frame, lapply(1:length(rads), function(i) {
  circle(c(x, rads[[i]]), rads[[i]], names(rads[i]))
})) -> TAB_int

# make the plot for the Land_use
Plot_INT <- ggplot(TAB_int)+
  geom_polygon(aes(x=x, y=y, group=group, fill=group),
               color="black")+
  scale_fill_manual(values = c("#7fc97f","#377eb8", "#e7298a"))+
  coord_equal()+
  theme_pubr()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+
  theme(legend.position="none")


TAB$Intervention<- firstup(as.character(TAB$Intervention))
p<-ggplot(TAB,
       aes(x = Intervention, y = estimate)) +
  xlab(NULL) +
  geom_hline(aes(yintercept=0), linetype=2, color='gray80')+
  geom_sina(aes(fill = Land_use,
                group = Intervention, size= 3*n_data),
                shape=21,color='black',alpha = 0.6,scale=FALSE)+
  scale_fill_manual(values = c("gray", "darkgreen", "orange" ,'green',"blue","yellow",'black'))+
  ylim(c(-80,120))+
  geom_violin(scale = "width",draw_quantiles = c(0.25, 0.5, 0.75), alpha=0.2) +
  coord_flip()+
  theme_pubr() +
   theme(legend.position="none")+
  labs(y="SOC change (percent)")+
  theme(legend.position="none",
        strip.background =element_rect(color="white", fill= NA),
        strip.text.x = element_text(angle = 0, hjust = 0, size=15),
        panel.border = element_rect(fill = NA, colour = "black",
                                    linewidth = rel(1)), panel.grid = element_line(colour = "black"))



 plot<-cowplot::ggdraw() +
  cowplot::draw_plot(p) +
  cowplot::draw_plot(plot_Land, x = 0.83, y = 0.2, width = 0.2, height = 0.2)+
   cowplot::draw_plot(Plot_INT, x = 0.83, y = 0.7, width = 0.2, height = 0.2)
plot

p<-ggplot(TAB,
          aes(x = Intervention, y = estimate, group=Intervention,
              text = paste("Land use :"       , Land_use,"\n",
                           "Intervention :"   ,Intervention,"\n",
                           "sub-category :"   , details,"\n",
                           "n_data :"         ,n_data,"\n",
                           "n_meta-analyse :" ,n_MA,"\n",
                           "estimate :"       ,round(estimate,1), "\n",
                           "conf.low :"       ,round(conf.low,1), "\n",
                           "conf.high :"      ,round(conf.high,1)))) +
  xlab(NULL) +
  geom_hline(aes(yintercept=0), linetype=2, color='gray80')+
  geom_sina(aes(fill = Land_use,
                group = Intervention, size= 3*n_data),
            shape=21,color='black',alpha = 0.6,scale=FALSE)+
  scale_fill_manual(values = c("gray", "darkgreen", "orange" ,'green',"blue","yellow"))+
  ylim(c(-80,120))+
  geom_violin(scale = "width",draw_quantiles = c(0.25, 0.5, 0.75), alpha=0.2) +
  coord_flip()+
  theme_pubr() +
  theme(legend.position="none")+
  labs(y="SOC change (percent)")+
  theme(legend.position="none",
        strip.background =element_rect(color="white", fill= NA),
        strip.text.x = element_text(angle = 0, hjust = 0, size=15),
        panel.border = element_rect(fill = NA, colour = "black",
                                    linewidth = rel(1)), panel.grid = element_line(colour = "black"))



 ggplotly(p,tooltip = "text")

TAB %>% group_by(Intervention)%>%
  filter(!details %in% c("fire : Wildfire","fire : wildfire", "snow cover")) %>%
  dplyr::summarize(q1 = quantile(estimate, 0.25),
            q3 = quantile(estimate, 0.75),
                          min = min (estimate),
                          max= max(estimate))

