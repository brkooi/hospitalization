
#load packages
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(car)
library(zoo)
library(randomForest)

loadMessage <- "Waiting for downloading data..........."

# read datasets from RIVM

CovidTests<-read.csv("https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen.csv",sep=";")
CovidTests<-CovidTests[c("Date_of_statistics", "Tested_with_result", "Tested_positive")]
CovidTests<-CovidTests %>% group_by(Date_of_statistics) %>%
  summarise(Tested_with_result = sum(Tested_with_result),
            Tested_positive = sum(Tested_positive))

InfectionRadar<-read.csv("https://data.rivm.nl/covid-19/COVID-19_Infectieradar_symptomen_per_dag.csv",sep=";")
InfectionRadar<-InfectionRadar[c("Date_of_statistics", "MA_perc_covid_symptoms")]
InfectionRadar$MA_perc_covid_symptoms <- na.locf(InfectionRadar$MA_perc_covid_symptoms, na.rm=FALSE)

HospitalAdmissions<-read.csv("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv",sep=";")
HospitalAdmissions<-HospitalAdmissions[c("Date_of_statistics", "Hospital_admission")]

HospitalAdmissions<-HospitalAdmissions %>% group_by(Date_of_statistics) %>%
  summarise(Hospital_admission = sum(Hospital_admission))

IcAdmissions <- read.csv("https://data.rivm.nl/covid-19/COVID-19_ic_opnames.csv",sep=";")
IcAdmissions<-IcAdmissions[c("Date_of_statistics", "IC_admission")]

IcAdmissions<-IcAdmissions %>% group_by(Date_of_statistics) %>%
  summarise(IC_admission = sum(IC_admission))


Sewage<-read.csv("https://data.rivm.nl/covid-19/COVID-19_rioolwaterdata.csv",sep=";")
Sewage$Date_of_statistics <- Sewage$Date_measurement
Sewage$RNA_flow_per_100000 <- Sewage$RNA_flow_per_100000 / 100000000000
Sewage<-Sewage[c("Date_of_statistics", "RNA_flow_per_100000")]

Sewage<-Sewage %>% group_by(Date_of_statistics) %>%
  summarise(RNA_flow_per_100000 = sum(RNA_flow_per_100000),
            AmountMeasures = n())
Sewage$RNA_flow_per_measurement <- Sewage$RNA_flow_per_100000 / Sewage$AmountMeasures
Sewage<-Sewage[c("Date_of_statistics", "RNA_flow_per_100000", "RNA_flow_per_measurement")]

Covid19 <- left_join(CovidTests, HospitalAdmissions, by = "Date_of_statistics") 

Covid19 <- left_join(Covid19, IcAdmissions, by = "Date_of_statistics")

Covid19 <- left_join(Covid19, Sewage, by = "Date_of_statistics")

Covid19 <- left_join(Covid19, InfectionRadar, by = "Date_of_statistics")

Covid19$Tested_with_result <- as.numeric(Covid19$Tested_with_result)
Covid19$Tested_positive <- as.numeric(Covid19$Tested_positive)
Covid19$Hospital_admission <- as.numeric(Covid19$Hospital_admission)
Covid19$IC_admission <- as.numeric(Covid19$IC_admission)
Covid19$RNA_flow_per_100000[Covid19$RNA_flow_per_100000 == 0] <- NA
Covid19$RNA_flow_per_100000<-na.locf(Covid19$RNA_flow_per_100000)
Covid19$RNA_flow_per_measurement[Covid19$RNA_flow_per_measurement == 0] <- NA
Covid19$RNA_flow_per_measurement<-na.locf(Covid19$RNA_flow_per_measurement)


Covid19$Date_of_statistics<-as.Date(Covid19$Date_of_statistics, format =  "%Y-%m-%d")

Covid19 <- data.frame(Covid19)

