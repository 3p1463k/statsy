library(RJSONIO)
library(RCzechia)

#mutate(NAZ_LAU1 = ifelse(NAZ_LAU1 == "Hlavní město Praha", "Praha", NAZ_LAU1)) 
# rename Prague (from The Capital to a regular city)
# cz$NAZ_CZNUTS3[14]="Praha"
# cz$NAZ_CZNUTS3[9]="Vysočina"


cz <- RCzechia::kraje("low")
df = rjson::fromJSON(file="https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
tdf1 <- data.frame(matrix(unlist(df$infectedByRegion), nrow=length(df$infectedByRegion), byrow=T))
names(tdf1) <- c("NAZ_CZNUTS3", "Pocet")
tdf1$NAZ_CZNUTS3 <- as.character(tdf1$NAZ_CZNUTS3)
tdf1$Pocet <- as.numeric(as.character(tdf1$Pocet))
dc <- cz %>% inner_join(tdf1, by = "NAZ_CZNUTS3")

tdf2 <- data.frame(matrix(unlist(df$infectedDaily), nrow=length(df$infectedDaily), byrow=T))
tdf2$X1 <- as.numeric(as.character(tdf2$X1))
tdf2$X2 <- as.Date(as.character(tdf2$X2))
tdf2 <- tdf2[35:nrow(tdf2),]
names(tdf2) <- c("Pocet", "Den")


ggplot(data = dc) +
  geom_sf(aes(fill=Pocet)) +
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


ggplot(tdf2, aes(x=Den, y=Pocet))+geom_line(lwd=2, color="#f9bd2e")+
  ggtitle("NAKAZENYCH")+theme_wsj()+
  theme(panel.background = element_rect(fill ="#2b374b", color = "#17202A"),
        plot.background = element_rect(fill = "#DC143C", color="#17202A"),
        panel.grid.minor = element_blank(),#2b374b
        legend.position = "none",plot.title = element_text(color="#17202A"),
        axis.title.x=element_blank())+
  geom_point(aes(col=Pocet), size=2)+
  geom_text(aes(label=Pocet),hjust=0, vjust=0, color="white", size=5)

tdf3 <- data.frame(matrix(unlist(df$infectedByAgeSex), nrow=length(df$infectedByAgeSex), byrow=T))
tdf3 <- as.data.frame(t(tdf3))


