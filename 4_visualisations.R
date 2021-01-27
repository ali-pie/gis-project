##Visualisations 

library(plyr)
library(tmap)

join <- join %>% 
  mutate(perc=difference2020vs2015*100)

join$winner <- ifelse(join$Duda_percent_2020>0.5,"Duda","Trzaskowski")

tmap_mode("plot")
tm_shape(join, name = "JPT_NAZWA_") + 
              tm_polygons(col = "perc",
                          id = "JPT_NAZWA_",
                          title = "Difference (%) in votes for Duda - 2015 vs 2020",
                          title.size = 25,
                          breaks = c(-50,-5,5,50,100,200),
                          palette = "RdYlGn",
                          legend.hist = TRUE,
                          border.alpha = 0.5)+
              tm_layout(legend.hist.width = 10,
                        legend.outside = TRUE,
                        legend.outside.position = "left")
                        
                        
  tm_shape(join, name = "JPT_NAZWA_") + 
    tm_polygons(col = "FSc",
                id = "JPT_NAZWA_",
                title = "Average family size receiving 500+",
                palette = "RdYlGn",
                legend.hist = TRUE,
                border.alpha = 0.5)+
    tm_layout(title.size = 20,
              legend.hist.width = 10,
              legend.outside = TRUE,
              legend.outside.position = "left")
  
  
  tm_shape(join, name = "JPT_NAZWA_") + 
    tm_polygons(col = "Rn",
                id = "JPT_NAZWA_",
                title = "Resources per working age citizen, 01/19-06/19",
                breaks = c(0,250,500,750,1000,1500,2000,10000),
                palette = "RdYlGn",
                legend.hist = TRUE,
                border.alpha = 0.5)+
    tm_layout(title.size = 20,
              legend.hist.width = 10,
              legend.outside = TRUE,
              legend.outside.position = "left")
  
  tm_shape(join, name = "JPT_NAZWA_") + 
    tm_polygons(col = "Fn",
                id = "JPT_NAZWA_",
                title = "Average number of families receiving 500+, 01/19-06/19",
                #breaks = c(-50,-5,5,50,100,200),
                palette = "RdYlGn",
                legend.hist = TRUE,
                border.alpha = 0.5)+
    tm_layout(title.size = 25,
              legend.hist.width = 10,
              legend.outside = TRUE,
              legend.outside.position = "left")


#histogram of family size for Duda voters and Trzaskowski voters

means <- ddply(join, "winner", summarise, grp.mean=mean(FSc))

ggplot(join, aes(x=FSc, fill=winner, color=winner))+
                         geom_histogram(alpha=0.2, position="identity", bins = 50)+
                         geom_vline(data=means, aes(xintercept=grp.mean, color=winner), linetype="dashed", size = 1.5)+
                         labs(title="Electorial winner in the commune, 2020",
                               x="Average no. of children per family receiving 500+, 1/19-6/19",
                               y="Commute count")+
                         theme(plot.title = element_text(hjust=0.5))+
                        scale_color_brewer(palette="Dark2")+
                        scale_fill_brewer(palette="Dark2")


