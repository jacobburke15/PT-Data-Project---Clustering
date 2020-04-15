
library(readxl)
library(ggplot2)
library(esquisse)
library(dplyr)
library(tidyverse)
library(gridExtra)

ROMSdata <- read_excel("For Masanao Class - ROMS Full Data Set.xlsx", sheet = "Master Data Set")

## keeping consistency in Chronic Pain values 

ROMSdata$`Chronic Pain (Yes/No)`[ROMSdata$`Chronic Pain (Yes/No)` == "no"] <- "No"
ROMSdata$`Chronic Pain (Yes/No)`[ROMSdata$`Chronic Pain (Yes/No)` == "yes"] <- "Yes"
ROMSdata <- filter(ROMSdata, `Chronic Pain (Yes/No)` != "1")

## fixing typos

ROMSdata$`Body Region`[ROMSdata$`Body Region` == 'knee'] <- 'Knee'
ROMSdata$`Body Region`[ROMSdata$`Body Region` == 'lumbar'] <- 'Lumbar'

## there is one ROMSID value that is six digit at 100000, while all other are four digit, removing for now
## (was told by PT team this would have been an error)

ROMSdata <- filter(ROMSdata, `ROMS ID` < 10000)

## Fiona and Vic, feel free to add to this so we have all data cleaning consistent all together in one file. 

