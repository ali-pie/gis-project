##Now we perform spatially lagged logistic regression

#First we obtain the difference between support for Duda in 2020 vs 2015. For that, 
#we will calculate the % change of the proportion of the votes for him per commune.

join$difference2020vs2015 <- (join$Duda_percent_2020-join$Duda_percent_2015)/join$Duda_percent_2015

#Then we move onto obtaining 1.	Amount of resource, in zloty, that went into the 500+ benefit in the commune

library(tidyverse)

join$Rn <- join$wydatki_na_swiadczenia_wychowawcze/join$workingpopulation

model1 <- join %>%
  lm(difference2020vs2015 ~
       Rn,
     data=.)
summary(model1)


#Then we move onto obtaining 2. Average family size receiving 500+ benefit in the area

join$FSc <- join$przecietna_miesieczna_liczba_dzieci_na_ktore_rodziny_otrzymuja_swiadczenie_wychowawcze_w_okresie_od_1_stycznia_2019_do_30_czerwca_2019/
            join$przecietna_miesieczna_liczba_rodzin_otrzymujacych_swiadczenie_wychowawcze_w_okresie_od_1_stycznia_2019_do_30_czerwca_2019

model2 <- join %>%
  lm(difference2020vs2015 ~
       FSc,
     data=.)
summary(model2)

#Then we moved onto obtaining 3. Number of families taking the 500+ benefit in the area

join$Fn <- join$przecietna_miesieczna_liczba_rodzin_otrzymujacych_swiadczenie_wychowawcze_w_okresie_od_1_stycznia_2019_do_30_czerwca_2019/
           join$workingpopulation

model3 <- join %>%
  lm(difference2020vs2015 ~
       Fn,
     data=.)
summary(model3)


#inthis case we can build a pultiple regression model based on the three variables

model4 <- join %>%
  lm(difference2020vs2015 ~
       FSc+
       Rn+
       Fn,
     data=.)
summary(model4)
        