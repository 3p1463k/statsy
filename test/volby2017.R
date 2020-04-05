library(RCzechia)
library(ggplot2)
library(ggthemes)

cz <- RCzechia::kraje("low")


volby2017 <- read.csv("/cloud/project/data/csv/volby_do_ps_2017.csv", 
                      stringsAsFactors = FALSE, check.names=FALSE)
drops <- "CelkemČR"
v1 <- volby2017[ , !(names(volby2017) %in% drops)]
v2 <- as.data.frame(t(v1), stringsAsFactors=FALSE)
names(v2) <- v1[,1]
v2 <- v2 %>% slice(2:nrow(v2))
v2 <- data.frame(lapply(v2, as.numeric), stringsAsFactors=FALSE)
v2 <-  v2 %>% mutate(NAZ_CZNUTS3= names(v1)[2:ncol(v1)])

v3 <- v2 %>% select("ANO", "NAZ_CZNUTS3")

v2017 <-  cz %>% inner_join(v3, by = "NAZ_CZNUTS3")



ggplot(data = v2017) +
  geom_sf(aes(fill=ANO)) +
  scale_fill_continuous(high = "#104E8B", low = "white")+
  geom_sf_text(aes(label=round(ANO/1000,0)), size=5)+
  theme_wsj()+
  theme(axis.text = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        plot.background = element_rect(fill = "white", color="black"),
        panel.background = element_rect(fill ="white", color = "#17202A"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size = "18"),
        legend.position = "none",
        title = element_text(size=20),
        plot.caption=element_text(color = "black",  hjust=1.01, size=10),
        plot.margin = unit(c(15, 25, 5, 5), "pt"),
        plot.subtitle = element_text(size = 15)
        
  )+
  labs(title = "ANO" , subtitle ="Volebni vysledky do PS 2017 v Krajich", caption="ZDROJ: Český statistický úřad")+
  
  annotate("text", x = 19.5  , y = 48.5, label = "Datastak",
            hjust=0.8, vjust=0.2, col="black", cex=6, alpha = 0.1)
