
library(readxl)
library(ggplot2)
library(esquisse)
library(dplyr)
library(tidyverse)
library(gridExtra)

ROMSdata <- read_excel("For Masanao Class - ROMS Full Data Set.xlsx", sheet = "Master Data Set")

ROMSdata <- ROMSdata %>% distinct()

paydata <- read_excel("For Masanao Class - ROMS Full Data Set.xlsx", sheet = "Claim and Payment Info")


## keeping consistency in Chronic Pain values 

ROMSdata$`Chronic Pain (Yes/No)`[ROMSdata$`Chronic Pain (Yes/No)` == "no"] <- "No"
ROMSdata$`Chronic Pain (Yes/No)`[ROMSdata$`Chronic Pain (Yes/No)` == "yes"] <- "Yes"
ROMSdata <- filter(ROMSdata, `Chronic Pain (Yes/No)` != "1")

ggplot(ROMSdata) +
  aes(x = Surgical, fill = `Chronic Pain (Yes/No)`) +
  geom_bar() +
  scale_fill_hue() +
  labs(title = "Surgical Vs. Non Surgical Injuries 
       by Chronic Pain")+
  theme_minimal()


## removed the one row where chronic = 1, for now 
## "Unknown" for chronic??

#################################

## Looking to see if pain is chronic, if this seems to have an association with either 
## outcome changes or pain changes overall after all visits 


ggplot(ROMSdata) +
  aes(x = `Chronic Pain (Yes/No)`, y = `Outcome Change Scores`) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Chronic Pain against Overall Pain Change Scores") +
  theme_minimal()

## doesn't seem to have much of an effect on Outcome score changes 

ggplot(ROMSdata) +
  aes(x = `Chronic Pain (Yes/No)`, y = `Pain Change Scores`) +
  geom_boxplot(fill = "#0c4c8a") + 
  labs(title = "Chronic Pain against Overall Pain Change Scores") +
  theme_minimal()

## also no effect here 

###################################################################

## Decision of Surgery based on Chronic Pain (need to normalize)

ggplot(ROMSdata) +
  aes(x = Surgical, fill = `Chronic Pain (Yes/No)`) +
  geom_bar()+
  scale_fill_hue() +
  labs(title = "Decision of Surgery Based on Chronic Pain") +
  theme_minimal()


## Conservative and surgical follow the same looking spread, of actually more not
## chronic pain versus chronic. Would think that more of the surgical people actually 
## did have chronic pain, but do not??

## "Non-Specific" under surgical column? (3 rows)


###################################################################

## Looking at surgical and chronic counts among distribution of different
## body region injuries


data1 <- ROMSdata %>% group_by(`ROMS ID`, `Body Region`, `Surgical`, 
                               `Chronic Pain (Yes/No)`, `Pain Change Scores`,
                               `Length Of Stay (days)`) %>% summarize(Visits = n())


dim(data1)

## surgical

ggplot(data1) +
 aes(x = `Body Region`, fill = Surgical) +
 geom_bar() +
 scale_fill_hue() +
 labs(title = "Distribution of Body Region Injuries, 
      Surgical Vs. Non - Surgical") +
 theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Chronic Pain

ggplot(data1) +
  aes(x = `Body Region`, fill = `Chronic Pain (Yes/No)`) +
  geom_bar() +
  scale_fill_hue() +
  labs(title = "Distribution of Body Region Injuries, 
                  Chronic vs. Not") +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


##############################################################################

## does the length of days spent, affect pain changes scores? By body region

ggplot(data1) +
  aes(x = `Length Of Stay (days)`, y = `Pain Change Scores`) +
  geom_point() + facet_wrap(~ `Body Region`)
  theme_minimal()
  

ggplot(data1) +
    aes(x =`Length Of Stay (days)`, y = `Outcome Change Scores`) +
    geom_point() + facet_wrap(~ `Body Region`)
  theme_minimal()
  
## Same question, but number of Visits? 
  
ggplot(data1) + aes(x = `Length Of Stay (days)`, y = Visits) + geom_point() + theme_minimal()

## not y = x line, but as days increase (overall from first visit to last), visit numbers increase

ggplot(data1) +
  aes(x = `Visits`, y = `Pain Change Scores`) +
  geom_point() + facet_wrap(~ `Body Region`)
theme_minimal()

ggplot(data1) +
  aes(x =`Visits`, y = `Outcome Change Scores`) +
  geom_point() + facet_wrap(~ `Body Region`)
theme_minimal()

## nope. 

 View(ROMSdata)

esquisser(data1)


library(ggplot2)


## there is one ROMSID value that is six digit at 100000, while all other are four digit, removing for now

data1 <- filter(data1, `ROMS ID` < 10000)



data2 <- filter(data1, `Body Region` == 'Balance' | `Body Region` == 'Cervical' |
                  `Body Region` =='Elbow' | `Body Region` == 'Foot/Ankle' | 
                  `Body Region` == 'Hand'| `Body Region` =='Hip')

data3 <- filter(data1, `Body Region` == 'knee' | `Body Region` == 'Knee' |
                  `Body Region` =='lumbar' | `Body Region` == 'Lumbar' | 
                  `Body Region` == 'Pelvis'| `Body Region` == 'Shoulder' |
                  `Body Region` == 'Thoracic'| `Body Region` == 'Wrist' )

## cleaing the 'knee' and 'lumbar' duplicate factors 

data3$`Body Region`[data3$`Body Region` == 'knee'] <- 'Knee'
data3$`Body Region`[data3$`Body Region` == 'lumbar'] <- 'Lumbar'


a <- ggplot(data2) +
  aes(x = log(Visits), color = `Body Region`) +
  geom_density(adjust = 1L, alpha = 0.5, size = 1) +
  scale_fill_hue() +
  theme_minimal()

b <- ggplot(data3) +
  aes(x = log(Visits), color = `Body Region`) +
  geom_density(adjust = 1L, alpha = 0.5, size = 1) +
  scale_fill_hue() +
  theme_minimal()

grid.arrange(a, b, ncol = 1, top = "Patient Visit Numbers by Injury Body Region")







