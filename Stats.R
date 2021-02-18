#Importing required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(gridExtra)

#Importing  datasets
cases_othr_cntry<- read.csv("Number of cases other countries .csv")
cases_nz <- read.csv("Number of cases NZ.csv")

#Preprocessing
df_cases <- rbind(cases_nz,cases_othr_cntry) #joining 2 datasets
df_cases <- as_tibble(df_cases)              # Converting as tibble
names(df_cases)[names(df_cases) == "series_name"] <- "country_name"
names(df_cases)[names(df_cases) == "parameter"] <- "date"
names(df_cases)[names(df_cases) == "sub_series_name"] <- "status"


df_cases <- separate(data = df_cases, col = country_name, into = c("country", "sub_cntry"), sep = "-") #stripping  the  first name of countries
df_cases <- df_cases[!(df_cases$country=="Rest of world"),]
df_cases$country <- trimws(df_cases$country, which = c("both"))
df_cases$date <- as.POSIXct(df_cases$date)  #Converting date-time format


dfplot_d <- df_cases %>% 
  filter(status=="Deceased")

dfplot_a <- df_cases %>% 
  filter(status=="Active")

dfplot_r <- df_cases %>% 
  filter(status=="Recovered")

#Plotting for number cases in New Zealand vs other countries listed
plot1 <- ggplot(dfplot_d, mapping = aes(x = date, y = value, color = country) ) + geom_line() + ggtitle("Number of Deceased") +
  xlab("Date") + ylab("Count")
plot2 <- ggplot(dfplot_a, mapping = aes(x = date, y = value, color = country) ) + geom_line() + ggtitle("Number of Active") +
  xlab("Date") + ylab("Count")
plot3 <- ggplot(dfplot_r, mapping = aes(x = date, y = value, color = country) ) + geom_line() + ggtitle("Number of Recovered") +
  xlab("Date") + ylab("Count")
grid.arrange(plot1, plot2, plot3, ncol=3)

##############
# Plotting number of cases in New Zealand vs Rest of world
target <- c("New Zealand", "Rest of world")
df <- df_cases %>% filter(country %in% target)

dfplot_d1 <- df %>% 
  filter(status=="Deceased")

dfplot_a1 <- df %>% 
  filter(status=="Active")
dfplot_r1 <- df %>% 
  filter(status=="Recovered")

plot4 <- ggplot(dfplot_d1, mapping = aes(x = date, y = value, color = country) ) + geom_line() + ggtitle("Number of Deceased") +
  xlab("Date") + ylab("Count")
plot5 <- ggplot(dfplot_a1, mapping = aes(x = date, y = value, color = country) ) + geom_line() + ggtitle("Number of Deceased") +
  xlab("Date") + ylab("Count")
plot6 <- ggplot(dfplot_r1, mapping = aes(x = date, y = value, color = country) ) + geom_line() + ggtitle("Number of Deceased") +
  xlab("Date") + ylab("Count")
grid.arrange(plot4, plot5, plot6, ncol=3)

#############
#Comparing change in income to ability of New Zealander's to meet bills

df_bills <- read.csv("Ability to meet bills.csv")
df_income <- read.csv("Change in income.csv")

df_bills <- as_tibble(df_bills)
df_income <- as_tibble(df_income)

names(df_bills)[names(df_bills) == "sub_series_name"] <- "Wave"
names(df_bills)[names(df_bills) == "parameter"] <- "ability to meet bills"
names(df_bills)[names(df_bills) == "value"] <- "count"

names(df_income)[names(df_income) == "sub_series_name"] <- "Wave"
names(df_income)[names(df_income) == "parameter"] <- "change in income"

target1 <- c("Wave", "ability to meet bills", "count")
df1 <- df_bills %>% select(target1)

target2 <- c("Wave", "change in income", "value")
df2 <- df_income %>% select(target2)
