library(RCzechia)

cz <- RCzechia::okresy("low")


volby2017 <- read_csv("/cloud/project/data/csv/volby_do_ps_2017.csv")


ano <- volby2017 %>% filter(Name=="ANO")

ggplot(data = volby2017) +
  geom_sf(aes(fill=CelkemČR)) +
  scale_fill_continuous(high = "#DC143C", low = "goldenrod")+
  geom_sf_text(aes(label=Pocet), size=5)+
  theme_wsj()+
  ggtitle("Nakazeno v Kraji / Infected by Region")+
  
  theme(axis.text = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        plot.background = element_rect(fill = "white", color="black"),
        panel.background = element_rect(fill ="white", color = "#17202A"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size = "18"),
        legend.position = "none", 
        
  )