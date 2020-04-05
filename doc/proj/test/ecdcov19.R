library(rvest)
library(tidyverse)
library(scales)
library(grid)
library(extrafont)
library(ggrepel)

ncov19ecd <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
ecdc <- read_csv(ncov19ecd)

ecdc$day <- as.numeric(ecdc$day)
ecdc$month <- as.numeric(ecdc$month)
ecdc$deaths <- as.numeric(ecdc$deaths)
ecdc$year <- as.numeric(ecdc$year)
ecdc$popData2018 <- as.numeric(ecdc$popData2018)
ecdc$cases <- as.numeric(ecdc$cases)
ecdc$dateRep <- as.Date(ecdc$dateRep)
ecd <- as.data.frame(ecdc %>% arrange(dateRep) %>% filter(deaths>5))

ecd <- ecd %>% mutate(casespm = cases/popData2018*1e+06, deathspm=deaths/popData2018*1e+06)

ecd %>% filter(countriesAndTerritories=="Belgium") %>% summarize(cpm= sum(casespm), dpm=sum(deathspm))


ggplot(ecd %>% filter(dateRep=="05-04-2020") , aes(cases, deaths, label=countriesAndTerritories)) + 
  geom_point(aes(col=countryterritoryCode),shape = 21,fill = "red", size = 5, stroke = 5, alpha = 1/2)+
  scale_x_log10(breaks = c(100, 1000, 5000, 25000))+
  scale_y_log10(breaks= c(10,100,1000))+
  geom_text_repel(size=4)+
  xlab("Tested Positive")+
  ylab("Deaths")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="#fffefc", color = "black"),
        plot.background = element_rect(fill = "#fffefc", color="black"),
        text= element_text(family="Times New Roman"),
        legend.position = "none",
        plot.title = element_text(color="#17202A"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(
          size = 1,
          linetype = "dotted",
          lineend = NULL,
          color = "grey",
          arrow = NULL,
          inherit.blank = FALSE
        ), 
        axis.text = element_text(size=15, colour = "black"),
        #axis.title = element_text(size = 15),
        title = element_text(size=20),
        plot.caption=element_text(color = "black",  hjust=1.01, size=10),
        plot.margin = unit(c(15, 25, 5, 5), "pt"),
        plot.subtitle = element_text(size = 15)
  )+
  labs(title = "Daily increase nCov19", subtitle = date(), caption="Source : European Centre for Disease Prevention and Control")+
  
  annotate("text", x = 50000  , y = 1, label = "Datastak",
           hjust=0.8, vjust=0.2, col="black", cex=6, alpha = 0.1)




#PER MILLION PEOPLE

ggplot(ecd %>% filter(dateRep=="4-04-2020") , aes(cases/popData2018*1000000, deaths/popData2018*1000000, label=countriesAndTerritories)) + 
  geom_point(aes(col=countryterritoryCode),shape = 21,fill = "red", size = 5, stroke = 5, alpha = 1/2)+
  scale_x_log10(breaks = c(1, 2, 3, 5, 10, 15, 25, 40, 60,100, 500))+
  scale_y_log10(breaks= c(0.1, 0.2, 0.3, 0.5, 0.75, 1.5,  1,2,3, 5, 10, 20, 35))+
  geom_text_repel(size=4)+
  xlab("Tested Positive")+
  ylab("Deaths")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="#fffefc", color = "black"),
        plot.background = element_rect(fill = "#fffefc", color="black"),
        text= element_text(family="Times New Roman"),
        legend.position = "none",
        plot.title = element_text(color="#17202A"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(
          colour = "grey",
          size = 1,
          linetype = "dotted",
          lineend = NULL,
          arrow = NULL,
          inherit.blank = FALSE
        ), 
        axis.text = element_text(size=15, colour = "black"),
        #axis.title = element_text(size = 15),
        title = element_text(size=20),
        plot.caption=element_text(color = "black",  hjust=1.01, size=10),
        plot.margin = unit(c(15, 25, 5, 5), "pt"),
        plot.subtitle = element_text(size = 15)
  )+
  labs(title = "Daily nCov19", subtitle = date(), caption="Source : European Centre for Disease Prevention and Control")+
  
  annotate("text", x = 500  , y = 0.01, label = "Datastak",
           hjust=0.8, vjust=0.2, col="black", cex=6, alpha = 0.05)




