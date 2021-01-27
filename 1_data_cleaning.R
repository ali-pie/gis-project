##Data cleaning and compiling

#In the first part of the analysis, we will import 
# a) the population data,
# b) Rodzina 500+ benefit data,
# c) Election data from 2015 and 2020, second round
# d) shapefile (.shp) of the communes in Poland

library(here)
library(sf)
library(janitor)
library(readxl)
library(plyr)
library(tmap)
library(tidyverse)
library(dplyr)

#All the data comes from the governmental websites. all the zip data has been moved to the data_raw folder, and unzipped into data_unzipped
#Next we load our commune shapefile and simplify it, for quicker processing

communes <- st_read(here::here("data", "data_unzipped", "Gminy","Gminy.shp"))

communes_simple <- st_simplify(communes)
communes_simple$JPT_KOD_JE <- as.character(communes_simple$JPT_KOD_JE)
communes_simple$kod_teryt <- str_sub(communes_simple$JPT_KOD_JE,
                                 1,
                                 str_length(communes_simple$JPT_KOD_JE)-1)

#Then we proceed to load up the population data

population <- read.csv(here::here("data","data_unzipped", "LUDN_2137_CREL_20210123152322.csv"),
                       sep = ";",
                       encoding = 'UTF-8')

population[c("Jednostka.miary","Atrybut","Rok","X","Plec")]<- NULL

population$Kod <- as.character(population$Kod)

working_age_groups <- c("15-19","20-24","25-29","30-34", "35-39", "40-44","45-49","50-54")

population_cleaned <- population %>%
                        clean_names() %>%
                        filter(wiek %in% working_age_groups)%>%
                        group_by(kod) %>%
                        dplyr::summarise(workingpopulation = sum(as.numeric(wartosc)))

#7 digit Codes are wrongly assigned to Lututow, Piatek, Czerwinsk nad Wisla and Klimotow
population_cleaned$kod[population_cleaned$kod == '1018042'] <- '1018043'
population_cleaned$kod[population_cleaned$kod == '1004062'] <- '1004063'
population_cleaned$kod[population_cleaned$kod == '1420042'] <- '1420043'
population_cleaned$kod[population_cleaned$kod == '2609032'] <- '2609033'

population_cleaned$kod <- ifelse(str_length(population_cleaned$kod)==6,
                                             paste("0",population_cleaned$kod, sep = ""),
                                         population_cleaned$kod)

#Then onto the Rodzina 500+ data

rodzina_500 <- read.csv(here::here("data","data_unzipped","OCHR_3803_CREL_20210124132209.csv"),
                         sep = ";",
                         encoding = 'UTF-8')

rodzina_500[c("Jednostka.miary","Atrybut","X","nazwa")]<- NULL

rodzina_500_cleaned <- rodzina_500 %>%
                          clean_names()%>%
                          pivot_wider(names_from = swiadczenie_wychowawcze, values_from = wartosc)%>%
                          clean_names()

rodzina_500_cleaned$kod <- as.character(rodzina_500_cleaned$kod)
rodzina_500_cleaned$kod <- ifelse(str_length(rodzina_500_cleaned$kod)==6,
                                      paste("0",rodzina_500_cleaned$kod, sep = ""),
                                      rodzina_500_cleaned$kod)

#7 digit Codes are wrongly assigned to Lututow, Piatek, Czerwinsk nad Wisla and Klimotow
rodzina_500_cleaned$kod[rodzina_500_cleaned$kod == '1018042'] = '1018043'
rodzina_500_cleaned$kod[rodzina_500_cleaned$kod == '1004062'] = '1004063'
rodzina_500_cleaned$kod[rodzina_500_cleaned$kod == '1420042'] = '1420043'
rodzina_500_cleaned$kod[rodzina_500_cleaned$kod == '2609032'] = '2609033'


elections_2020 <-  read.csv(here::here("data","data_unzipped","wyniki_gl_na_kand_po_obwodach_utf8.csv"),
                            sep = ";",
                            encoding = 'UTF-8'
)

elections_2020_cleaned <- elections_2020 %>%
                                  clean_names() %>%
                                  group_by(kod_teryt) %>%
                                  dplyr::summarise(Dudavotes2020 = sum(as.numeric(andrzej_sebastian_duda)), 
                                            Trzaskowskivotes2020 = sum(as.numeric(rafal_kazimierz_trzaskowski)),
                                            totalvotes2020 = sum(as.numeric(liczba_glosow_waznych_oddanych_lacznie_na_wszystkich_kandydatow)))


elections_2020_cleaned$Duda_percent_2020 = elections_2020_cleaned$Dudavotes2020/elections_2020_cleaned$totalvotes2020

elections_2020_cleaned$kod_teryt <- as.character(elections_2020_cleaned$kod_teryt)
elections_2020_cleaned$kod_teryt <- ifelse(str_length(elections_2020_cleaned$kod_teryt)==5,
                                         paste("0",elections_2020_cleaned$kod_teryt, sep = ""),
                                         elections_2020_cleaned$kod_teryt
)

elections_2020_cleaned$kod_teryt[elections_2020_cleaned$kod_teryt=='146502'] <- '146501' #(changing Warsaw's code since it was wrong')

elections_2015 <- read_excel(here::here("data","data_unzipped","wyniki_tura2.xls"))

elections_2015_cleaned <- elections_2015 %>%
                                  clean_names()%>%
                                  group_by(teryt_gminy) %>%
                                  dplyr::summarise(Dudavotes2015 = sum(as.numeric(andrzej_sebastian_duda)), 
                                            Komorowskivotes2015 = sum(as.numeric(bronislaw_maria_komorowski)),
                                            Totalvotes2015 = sum(as.numeric(andrzej_sebastian_duda)+as.numeric(bronislaw_maria_komorowski))
                                  )


elections_2015_cleaned$teryt_gminy <- as.character(elections_2015_cleaned$teryt_gminy)
elections_2015_cleaned$teryt_gminy <- ifelse(str_length(elections_2015_cleaned$teryt_gminy)==5,
                                         paste("0",elections_2015_cleaned$teryt_gminy, sep = ""),
                                         elections_2015_cleaned$teryt_gminy
)
elections_2015_cleaned$teryt_gminy[elections_2015_cleaned$teryt_gminy=='146502'] <- '146501'


elections_2015_cleaned$Duda_percent_2015 = elections_2015_cleaned$Dudavotes2015/elections_2015_cleaned$Totalvotes2015

