library(RCzechia)

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




ano <- v1 %>% filter(Name=="ANO")
ano1 <- ano %>% as.data.frame(t(ano))


ggplot(data = ano) +
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