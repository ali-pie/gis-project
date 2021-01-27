library(here)
library(gdata)
library(sf)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(stringr)
library(tmaptools)
library(dplyr)
library("pryr")
library("rgdal")
library("rmapshaper")
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(stringr)
library(tidyverse)
library(readxl)
setwd('C:/Users/alina/OneDrive/Desktop/UCL 2020-21/GIS/GIT project')
gmina <- st_read(here::here("data","Gminy","Gminy.shp"))
wojewodztwo <- st_read(here::here("data","Wojewodztwa","Województwa.shp"))

results <- read.csv(here::here("data","wyniki_gl_na_kand_po_obwodach_utf8.csv"),
                    sep = ";",
                    encoding = 'UTF-8')
#since teryt code was in a format that translated to zero, we manually cleaned the strings before import
results2015 <- read_excel(here::here("data","wyniki_tura2.xls"))

rodziny_pobierajace_500 <- read.csv(here::here("data","rodziny_pobierajace_500.csv"),
                    sep = ";",
                    encoding = 'UTF-8')


library(OpenStreetMap)

tmap_mode("view")
tm_shape(gminasimple,
         name = "JPT_NAZWA_") + 
  tm_polygons(col = NA,
              alpha = 0.5,
              id = "JPT_NAZWA_")

gminasimple <- st_simplify(gmina)

##Data modification
results <- read.csv(here::here("data","wyniki_gl_na_kand_po_obwodach_utf8.csv"),
                    sep = ";",
                    encoding = 'UTF-8'
                    )

glimpse(results)
results[11]
results <- results %>%
                    rename(
                      Wojewodztwo = Wojew?dztwo,
                      Rafal.Kazimierz.TRZASKOWSKI = Rafal.Kazimierz.TRZASKOWSKI
                          )

summarise <- results %>%
              clean_names() %>%
              group_by(wojewodztwo) %>%
              summarise(Dudavotes = sum(as.numeric(Andrzej.Sebastian.DUDA)), 
              Trzaskowskivotes = sum(as.numeric(Rafal.Kazimierz.TRZASKOWSKI)),
              Total = sum(as.numeric(Andrzej.Sebastian.DUDA)+as.numeric(Rafal.Kazimierz.TRZASKOWSKI))
                )

summary$duda.perc = summary$Dudavotes/summary$Total

results2015cleaned <- results2015 %>%
                clean_names()%>%
                group_by(teryt_gminy) %>%
                summarise(Dudavotes2015 = sum(as.numeric(andrzej_sebastian_duda)), 
                          Komorowskivotes2015 = sum(as.numeric(bronislaw_maria_komorowski)),
                          Total2015 = sum(as.numeric(andrzej_sebastian_duda)+as.numeric(bronislaw_maria_komorowski))
                )

results2015cleaned$teryt_gminy <- as.character(results2015cleaned$teryt_gminy)
results2015cleaned$teryt_gminy <- ifelse(str_length(results2015cleaned$teryt_gminy)==5,
                                   paste("0",results2015cleaned$teryt_gminy, sep = ""),
                                   results2015cleaned$teryt_gminy
)

                

join <- left_join(wojewodztwo,
                  summarise,
                  by = c("JPT_NAZWA_"="Wojewodztwo")
                  )

join$Duda_percentage <- join$Dudavotes/join$Total

##Map duda % vote in POland
tmap_mode("view")
tm_shape(join,
         name = "JPT_NAZWA_") + 
  tm_polygons(col = "Duda_percentage",
              alpha = 0.5,
              id = "JPT_NAZWA_")

summarisegmina <- results %>%
  group_by(Kod.TERYT) %>%
  summarise(Dudavotes = sum(as.numeric(Andrzej.Sebastian.DUDA)), 
            Trzaskowskivotes = sum(as.numeric(Rafal.Kazimierz.TRZASKOWSKI)),
            Total = sum(as.numeric(Andrzej.Sebastian.DUDA)+as.numeric(Rafal.Kazimierz.TRZASKOWSKI))
  )


gminasimple$Kod.TERYT <- str_sub(gminasimple$JPT_KOD_JE,
                                 1,
                                 str_length(gminasimple$JPT_KOD_JE)-1)

gminasimple$Kod.TERYT <- ifelse(str_length(gminasimple$Kod.TERYT)==5,
       gminasimple$Kod.TERYT <- paste("0",gminasimple$Kod.TERYT, sep = ""),
       gminasimple$Kod.TERYT
)

summarisegmina$Kod.TERYT <- ifelse(str_length(summarisegmina$Kod.TERYT)==5,
                                paste("0",summarisegmina$Kod.TERYT, sep = ""),
                                summarisegmina$Kod.TERYT
)




summarisegmina$Kod.TERYT = as.character(summarisegmina$Kod.TERYT)

summarisegmina$Kod.TERYT[summarisegmina$Kod.TERYT=='146502'] <- '146501' #(changing Warsaw's code since it was wrong')
join <- left_join(gminasimple,
                  summarisegmina,
                  by = c("Kod.TERYT"="Kod.TERYT")
)
join$Duda_percentage <- join$Dudavotes/join$Total

current_style <- tmap_style("col_blind")

tm_shape(join,
         name = "JPT_NAZWA_") + 
  tm_polygons(col = "Duda_percentage",
              alpha = 0.5,
              id = "JPT_NAZWA_") +
  tm_layout(main.title = "Second round election results, 2020", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

tmap_mode("view")
qtm(join, 
    fill = "Duda_percentage",
    title = "Election results - Duda's popularity 2020",
    borders = NULL,
    polygons.id = "JPT_NAZWA_",
    fill.palette = "Blues")

rodziny_pobierajace_500$Kod <- as.character(rodziny_pobierajace_500$Kod)

#rodziny_pobierajace_500$Kod <- str_sub(rodziny_pobierajace_500$Kod,
 #                                1,
 #                                str_length(rodziny_pobierajace_500$Kod)-1)

rodziny_pobierajace_500$Kod <- ifelse(str_length(rodziny_pobierajace_500$Kod)==6,
                                      paste("0",rodziny_pobierajace_500$Kod, sep = ""),
                                     rodziny_pobierajace_500$Kod
)

rodziny_pobierajace_500<- rodziny_pobierajace_500 %>% 
                                      rename(
                                        liczba_rodzin = przecietna.miesieczna.liczba.rodzin.otrzymujacych.swiadczenie.wychowawcze.w.okresie.od.1.stycznia.2019.do.30.czerwca.2019.2019....
                                      )

subset(join_500, is.na(liczba_rodzin)) ## wrong codes for Lututow (1018043 instead of 1018042), Piatek ())
rodziny_pobierajace_500$Kod[rodziny_pobierajace_500$Kod == '1018042'] = '1018043'
rodziny_pobierajace_500$Kod[rodziny_pobierajace_500$Kod == '1004062'] = '1004063'
rodziny_pobierajace_500$Kod[rodziny_pobierajace_500$Kod == '1420042'] = '1420043'
rodziny_pobierajace_500$Kod[rodziny_pobierajace_500$Kod == '2609032'] = '2609033'


join_500 <- left_join(join,
                     rodziny_pobierajace_500,
                     by = c("JPT_KOD_JE"="Kod")
)

tmap_mode("view")
tm_shape(join_500,
         name = "JPT_NAZWA_") + 
  tm_polygons(col = "liczba_rodzin",
              alpha = 0.5,
              id = "JPT_NAZWA_")

qtm(join_500, 
    fill = "liczba_rodzin",
    title = "Election results - Duda's popularity 2020",
    borders = NULL,
    polygons.id = "JPT_NAZWA_",
    fill.palette = "Blues")


#Population
population <- read.csv(here::here("data","LUDN_2137_CREL_20210123152322.csv"),
                                    sep = ";",
                                    encoding = 'UTF-8')
population[c("Jednostka.miary","Atrybut","Rok","X","Plec")]<- NULL
population$Kod <- as.character(population$Kod)
working_age_groups <- c("20-24","25-29","30-34", "35-39", "40-44","45-49")
population_trimmed <- population %>%
                      filter(Wiek %in% working_age_groups)%>%
                      group_by(Kod) %>%
                      summarise(workingpopulation = sum(as.numeric(Wartosc)))

population_trimmed2 <- population %>%
                      group_by(Kod) %>%
                      summarise(totalpopulation = sum(as.numeric(Wartosc)))

population_trimmed <- left_join(population_trimmed,
                                population_trimmed2,
                                by = c("Kod"="Kod"))


population_trimmed$Kod[population_trimmed$Kod == '1018042'] <- '1018043'
population_trimmed$Kod[population_trimmed$Kod == '1004062'] <- '1004063'
population_trimmed$Kod[population_trimmed$Kod == '1420042'] <- '1420043'
population_trimmed$Kod[population_trimmed$Kod == '2609032'] <- '2609033'

population_trimmed$Kod <- ifelse(str_length(population_trimmed$Kod)==6,
                           paste("0",population_trimmed$Kod, sep = ""),
                           population_trimmed$Kod
)

join_pop_500 <- left_join(join_500,
                          population_trimmed,
                          by = c("JPT_KOD_JE"="Kod")
)

join_pop_500$Kod <- ifelse(str_length(join_pop_500$Kod)==6,
                                      paste("0",join_pop_500$Kod, sep = ""),
                           join_pop_500$Kod
)

join_pop_500$families_per_capita <- join_pop_500$liczba_rodzin/join_pop_500$totalpopulation

join_pop_500$families_per_working_capita <- join_pop_500$liczba_rodzin/join_pop_500$workingpopulation


join_pop_500_gmina <- join_pop_500
  
join_pop_500_gmina <- left_join(join,
                                join_pop_500,
                                by = c("JPT_KOD_JE"="Kod")
)
results2015cleaned$teryt_gminy[results2015cleaned$teryt_gminy=='146502'] <- '146501'
join_pop_500_gmina_compare <- left_join(join_pop_500_gmina,
                                        results2015cleaned,
                                        by = c("Kod.TERYT"="teryt_gminy")
)

join_pop_500_gmina_compare$Duda_percentage_2015 <- join_pop_500_gmina_compare$Dudavotes2015/join_pop_500_gmina_compare$Total2015

join_pop_500_gmina_compare$Difference <- (join_pop_500_gmina_compare$Duda_percentage - join_pop_500_gmina_compare$Duda_percentage_2015)/join_pop_500_gmina_compare$Duda_percentage_2015

tmap_mode("view")
tm_shape(join_pop_500_gmina_compare,
         name = "JPT_NAZWA_") + 
  tm_polygons(col = "Difference",
              alpha = 0.5,
              id = "JPT_NAZWA_",
              lwd = 0.5,
              border.col = "white",
              border.alpha = 0.5)

# library
library(ggplot2)

# dataset:
data=data.frame(value=rnorm(100))

# basic histogram
p <- ggplot(join_pop_500_gmina_compare,
            aes(x=families_per_capita)) + 
  geom_histogram( binwidth=0.01, 
                  fill="#69b3a2", 
                  color="#e9ecef", 
                  alpha=0.9) +
  ggtitle("Bin size = 0.01")+
  theme(
    plot.title = element_text(size=15)) +
  geom_vline(aes(xintercept=mean(families_per_capita)),
             color="white", linetype="dashed", size=1)
print(p)

q <- qplot(x = `families_per_capita`, 
           y = `Difference`, 
           data=join_pop_500_gmina_compare)
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()
#this looks like a log, so let's try plotting it that way

join_pop_500_gmina_compare$log_families_per_capita <- log(join_pop_500_gmina_compare$families_per_capita)

lfmc <- qplot(x = `log_families_per_capita`, 
             y = `Difference`, 
             data=join_pop_500_gmina_compare)
lfmc + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

#Regression 

Regressiondata<- join_pop_500_gmina_compare%>%
  clean_names()%>%
  dplyr::select(families_per_capita, 
                families_per_capita)

model1 <- Regressiondata %>%
  lm(Difference ~
       families_per_capita,
     data=.)
summary(model1)
#There is a positive correlation between the number of families per working person
#and number of votes in the area

library(rsample)
set.seed(99)

GCSE_boot <-st_drop_geometry(Regressiondata) %>%
  bootstraps(times = 1000, apparent = TRUE)

GCSE_models <- GCSE_boot %>%
  #make new column
  mutate(
    #column name is model that contains...
    model = map(splits, ~ lm(duda_percentage ~ families_per_capita, 
                             data = .)))

# let's look at the first model results
GCSE_models$model[[1]]
GCSE_models_tidy <- GCSE_models %>%
  mutate(
    coef_info = map(model, tidy))
GCSE_coef <- GCSE_models_tidy %>%
  unnest(coef_info)
GCSE_coef
coef <- GCSE_coef %>% 
  filter(term == "families_per_capita")
coef
coef %>%
  ggplot(aes(x=estimate)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="lightblue2", col="lightblue3")+
  geom_vline(aes(xintercept=mean(estimate)),
             color="blue",
             linetype="dashed")+
  labs(title="Bootstrap resample estimates",
       x="Coefficient estimates",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
library(rsample)
int_pctl(GCSE_models_tidy, coef_info, alpha = 0.05)

full_data500 <- read.csv(here::here("data","data_unzipped","OCHR_3803_CREL_20210124132209.csv"),
                         sep = ";",
                         encoding = 'UTF-8')
full_data500[c("Jednostka.miary","Atrybut","X")]<- NULL
full_data500cleaned <- full_data500 %>%
  clean_names()%>%
  pivot_wider(names_from = swiadczenie_wychowawcze, values_from = wartosc)%>%
  clean_names()



full_data500cleaned$kod <- as.character(full_data500cleaned$kod)
full_data500cleaned$kod <- ifelse(str_length(full_data500cleaned$kod)==6,
                                         paste("0",full_data500cleaned$kod, sep = ""),
                                         full_data500cleaned$kod
)

join_pop_500_gmina_compare_new500 <- left_join(join_pop_500_gmina_compare,
                                        full_data500cleaned,
                                        by = c("JPT_KOD_JE"="kod")
)

join_pop_500_gmina_compare_new500$children_per_capita <- join_pop_500_gmina_compare_new500$"przecietna miesieczna liczba dzieci, na kt?re rodziny otrzymuja swiadczenie wychowawcze w okresie od 1 lipca 2019 do 31 grudnia 2019"/
                                                        join_pop_500_gmina_compare_new500$totalpopulation

join_pop_500_gmina_compare_new500$children_per_family500 <- join_pop_500_gmina_compare_new500$"przecietna miesieczna liczba dzieci, na kt?re rodziny otrzymuja swiadczenie wychowawcze w okresie od 1 lipca 2019 do 31 grudnia 2019"/
                                                            

model1 <- join_pop_500_gmina_compare_new500 %>%
  lm(Difference ~
       children_per_capita,
     data=.)
summary(model1)

rodzina_500 <- read.csv(here::here("data","data_unzipped","OCHR_3803_CREL_20210124132209.csv"),
                        sep = ";",
                        encoding = 'UTF-8')
