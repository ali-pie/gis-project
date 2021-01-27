#Data merge

#Now we're merging the population data, election data, Rodzina 500+ and the shapefile.
#Terytorial code is contained to 7 digits - the 7th digit can be either a 1 (for an urban commune),
#2 (for a rural commune) or 3 (an urban-rural commune). For communes containing mixed areas
#(urban, rural and urban-rural), 0 number is used with cummulative results. Hence, even though
#we're joining strings of different lenghts, the data is essentially wholely correct.

join <- left_join(communes_simple,
                  elections_2020_cleaned,
                  by = c("kod_teryt"="kod_teryt")) %>%
        left_join(.,
                  elections_2015_cleaned,
                  by = c("kod_teryt"="teryt_gminy")) %>%
        left_join(.,
                  population_cleaned,
                  by = c("JPT_KOD_JE"="kod")) %>%
        left_join(.,
                  rodzina_500_cleaned,
                  by = c("JPT_KOD_JE"="kod"))

