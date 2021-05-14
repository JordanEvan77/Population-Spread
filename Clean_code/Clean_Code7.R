library(tidyverse)
library(haven)
#loading libraries for data manipulation
#Question 1 and 2 Dimensions of data
nfhs <- read_dta('Raw_data/IAHR52FL.dta') #This is the full initial raw data
#Question 3, variables between "hhid" and "shstruc".
nfhs_reduced <- select(nfhs, hhid:shstruc) %>% 
  rename(survey_month = hv006) %>%
  rename(loc_type = hv026) #much easier to view table, less columns, only necessary data.
nfhs_urban <- select(nfhs, hhid:shstruc) %>% 
  rename(survey_month = hv006) %>%
  rename(home_loc = hv025) %>%
  rename(loc_type = hv026) %>%
  filter(home_loc == 1)#smaller, urban only, rural dropped
#Question 4, Plot the distribution of the number of listed household members
#for the entire sample. 
ggplot(data = nfhs_reduced,
       mapping = aes(x = hv009), binwidth = 1) + 
  geom_histogram() + 
  xlab("Number of household members") #Works! simple bar plot, skewed to right showing full data distribution.
#QUestion 5, Create a boxplot plot using the data frame for urban area. 
#FACTOR:
nfhs_1 <-as.factor(nfhs_urban$loc_type) #factor type for sorting urban only

#PLOT:
urban_plot <- ggplot(nfhs_urban) + aes(x = nfhs_1, y = hv009)
urb_labels <- c("Large City", "Small city", "Town", "Country Side", "Missing")
urban_plot + geom_boxplot() + xlab("Home Location") + 
  ylab("Number Of Household Members") +
  scale_x_discrete(labels = urb_labels)#very nice box plot for showing the house member counts per location type. 

#Question 6,Use "group_by" and "summarise" to find the means and medians of the number of household members
#by type of urban area.
nfhs_urban %>%
  group_by(loc_type) %>%
  summarise_at(vars(hv009), list(name=mean))#list of means should show below

nfhs_urban %>%
  group_by(loc_type) %>%
  summarise_at(vars(hv009), list(name=median))#list of medians should show as below
#LIST OF MEANS PEOPLE PER HOUSE
#[capital, large city]  4.65
#[small city]           4.88
#[town]                 4.69

#LIST OF MEDIAN PEOPLE PER HOUSE
#[capital, large city]     4
#[small city]              4
#[town]                    4

#Question 7, What does the relationship between the mean and median tell you about
#the distribution of household size? 
#The distribution doesn't vary much. This might indicate that large intergenerational homes
#aren't the norm for the urban families, because the means are all within .23 "people" of eachother
#and the median is the same for each location type. Additional comments on quiz. 