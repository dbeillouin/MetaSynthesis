# Packages:
library(ggpubr)
library(ggstar)

# Load Data
TAB_FIG3<-dplyr::bind_rows(BEST_fit_IN_SubIN,BEST_fit_DO2_LU_IN_SubIN) %>%
            #dplyr::bind_rows(.,BEST_fit_IN_SubIN) %>%
            dplyr::filter(Intervention =="global changes") %>%
          dplyr::filter(!Sub_Cat_intervention =="fire")

TAB_FIG3<-TAB_FIG3 %>%
  mutate(Land_use = factor(Land_use, levels=rev(c("ALL",
                                                  "cropland",
                                                  "forest land",
                                                  "grassland",
                                                  "wetlands",
                                                  "other land",
                                                  "various land uses"))))

TAB_FIG3<-TAB_FIG3 %>%
  mutate(Sub_Cat_intervention = factor(Sub_Cat_intervention, levels=rev(c("ALL",
                                                                          "combination of climatic factors",
                                                                          "climate change : co2 enrichment",
                                                                          "climate change : warming",
                                                                          "climate change : drought",
                                                                          "climate change : rainfall increase/irrgation",
                                                                          "climate change : drought/rainfall reduction",
                                                                          "snow cover decrease",
                                                                          "fire",
                                                                          'wild fire'))))
TAB_FIG3$CAT<-TAB_FIG3$Sub_Cat_intervention

TAB_FIG3 <- TAB_FIG3 %>% mutate(CAT = forcats::fct_recode(CAT,
                                                 "Direct effects" = "combination of climatic factors",
                                                 "Direct effects"= "climate change : co2 enrichment",
                                                 "Direct effects" =  "climate change : warming",
                                                 "Direct effects" =  "climate change : drought",
                                                 "Direct effects" = "climate change : rainfall increase/irrgation",
                                                 "Direct effects" = "climate change : drought/rainfall reduction",
                                                 "Indirect effects" = "snow cover decrease",
                                                 "Indirect effects" = "wild fire",
                                                 "Indirect effects" = "fire"))

TAB_FIG3<-TAB_FIG3 %>%
  mutate(CAT = factor(CAT, levels=c("Direct effects",
                                    "Indirect effects")))

## correct some namming
TAB<- TAB %>% dplyr::mutate(Intervention= plyr::revalue(Intervention,c("climate change : rainfall increase/irrgation"= "climate change : rainfall increase")))


TAB_FIG3$estimate<- ((exp(TAB_FIG3$estimate)-1)*100)
TAB_FIG3$conf.low<- ((exp(TAB_FIG3$conf.low)-1)*100)
TAB_FIG3$conf.high<- ((exp(TAB_FIG3$conf.high)-1)*100)
#TAB_FIG3<- TAB_FIG3 %>% filter(!Sub_Cat_intervention %in% c('wild fire'))


### Figure for the circles

# Calculate the total number of climate change
TAB_CC <- TAB_FIG3 %>%
  dplyr::filter(!is.na(Land_use)) %>%
  dplyr::group_by(Sub_Cat_intervention) %>%
  dplyr::summarize(n1 = sum(n_data, na.rm=TRUE))
TAB_CC$prop<-TAB_CC$n1/sum(TAB_CC$n1)*100

# function to draw imbricated circles
circle <- function(center, radius, group) {
  th <- seq(0, 2*pi, len=200)
  data.frame(group=group,
             x=center[1] + radius*cos(th),
             y=center[2] + radius*sin(th))
}

# Create a named vector for your values
obs <- c(A_Warming=37.5,B_Fire=20.3, C_CO2=14.3, D_drought=13.8, E_combi=13.6, F_snow= 0.5)

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
plot_CC <- ggplot(TAB_Land)+
  geom_polygon(aes(x=x, y=y, group=group, fill=group),
               color="black")+
  scale_fill_manual(values = rev(c("#8c510a","#d8b365", "#f6e8c3" ,'#c7eae5',"#5ab4ac","#01665e")))+
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

### plot land-use
# Calculate the total number of climate change
TAB_LU <- TAB_FIG3 %>%
  dplyr::filter(!is.na(Land_use)) %>%
  dplyr::group_by(Land_use) %>%
  dplyr::summarize(n1 = sum(n_data, na.rm=TRUE))
TAB_LU$prop<-TAB_LU$n1/sum(TAB_LU$n1)*100

# function to draw imbricated circles
circle <- function(center, radius, group) {
  th <- seq(0, 2*pi, len=200)
  data.frame(group=group,
             x=center[1] + radius*cos(th),
             y=center[2] + radius*sin(th))
}

# Create a named vector for your values
obs <- c(A_Other=42.5,B_Forest=32.1, C_grassland=15.2, D_cropland=9.4, E_wetlands=0.8)

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
plot_LU <- ggplot(TAB_Land)+
  geom_polygon(aes(x=x, y=y, group=group, fill=group),
               color="black")+
  scale_fill_manual(values = c("#af8dc3",  "#036901",  "#6e9b6d", "#ff9d00",  "#0493ff"))+
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



 TAB_FIG3$Land_use[is.na(TAB_FIG3$Land_use)] <- 'ALL'

plot_ES<-ggplot(TAB_FIG3, aes(firstup(as.character(TAB_FIG3$Sub_Cat_intervention)),
                     estimate))+
  geom_hline(aes(yintercept=0), linetype=2)+
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high,
        color = Land_use),
    position = position_dodge2(width = 0.7, preserve = "single"),size=0.4
  )+
  ggstar::geom_star(aes(fill = Land_use),starshape=12, size=3,starstroke=1, position= position_dodge2(width = 0.7, preserve = "single"))+
  scale_color_manual(values = c('gray',"#af8dc3", "#0493ff","#6e9b6d", "#036901","#ff9d00", "black"))+
  scale_fill_manual(values = c('gray',"#af8dc3", "#0493ff","#6e9b6d", "#036901","#ff9d00", "white"))+
scale_linetype_manual(values=c("solid","solid","solid","solid","solid","solid")) +
  scale_alpha_manual(values=c(1,1,1,1,1,1,1,1,1))+
  coord_flip()+ theme_pubr()+
  facet_wrap(CAT~., scales="free", ncol=1)+
  labs(y="SOC change (percent)", x="")+
  theme(legend.position="none",
        strip.background =element_rect(color="white", fill= NA),
        strip.text.x = element_text(angle = 0, hjust = 0, size=15),
        panel.border = element_rect(fill = NA, colour = "black",
                                    linewidth = rel(1)), panel.grid = element_line(colour = "black"))



PLOT<-cowplot::plot_grid(plot_LU+  theme(plot.margin=unit(c(-1,0,0,0), "cm")),
                         plot_CC,
                         rel_widths = c(1,1), align='h', ncol=1)
PLOT<-PLOT+   theme(plot.margin=unit(c(-0.2,0,0,0), "cm"))
cowplot::plot_grid(plot_ES,PLOT,rel_widths = c(8,1), align='h', ncol=2)



plot<-cowplot::ggdraw() +
  cowplot::draw_plot(plot_ES) +
  cowplot::draw_plot(plot_LU, x = 0.85, y = 0.2, width = 0.2, height = 0.2)+
  cowplot::draw_plot(plot_CC, x = 0.85, y = 0.7, width = 0.2, height = 0.2)



#
TAB_FIG3$estimate<- as.numeric(as.character(TAB_FIG3$estimate))
TAB_FIG3$conf.low<-round(TAB_FIG3$conf.low,1)
TAB_FIG3$conf.high<-round(TAB_FIG3$conf.high,1)
TAB_FIG3$estimate<-round(TAB_FIG3$estimate,1)
TAB_FIG3$statistic<-round(TAB_FIG3$statistic,2)
TAB_FIG3$std.error<-round(TAB_FIG3$std.error,1)
TAB_FIG3$p.value<-round(TAB_FIG3$p.value,3)


TAB<-TAB_FIG3 %>% dplyr::select(-c(AIC))
names(TAB)[14]<-"model"
TAB<-TAB %>% dplyr::select(-c('Intervention','data',"Weights",'CAT',"model",'fit'))
TAB<-TAB%>% relocate("Land_use", .before = "Sub_Cat_intervention")

GROUP <- dplyr::group_by(TAB, `Sub_Cat_intervention`) %>%
  dplyr::summarize(Number = dplyr::n())


TAB<-TAB%>% arrange(-`Land_use`,`Sub_Cat_intervention`)

reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TAB, `Sub_Cat_intervention` == GROUP$`Sub_Cat_intervention`[index]) %>% select(-`Sub_Cat_intervention`)
    tbl <- reactable(sales,outlined = TRUE, highlight = TRUE, fullWidth = TRUE,
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
                       if (sales[index, "Land_use"] =='ALL') {
                         list(background = "rgba(0, 0, 0, 0.05)")
                       }
                     }
    )
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)


