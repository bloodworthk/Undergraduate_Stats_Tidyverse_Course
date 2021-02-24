#### R-Coding TidyVerse Lesson ####
#Written By: Kathryn Bloodworth#
##Created: 02/24 ##
## Goal: teach undergraduate students about tidyverse and how to use tidyverse to clean up their own data ##

#### Install and Load Packages ####

#install package only needs to happen one time on you device
install.packages("tidyverse") #need quotes

#you must load the library everytime you start an R session
library(tidyverse) #no quotes

#### Set Working Directory ####
#setting working directory for my mac to work out of a file on my computer where I am storing the data we are working with 
#note - this is not using tidyverse
setwd("~/Dropbox (Smithsonian)/Projects/Undergrad_Stats_Course")

#### Read in Data ####
#note that  both datasheets are saved as CSV files which is the best format when needing to manipulate them R
#note - this is not using tidyverse

#read in the biomass LLP data and put it into a dataframe called "Biomass"
Biomass<-read.csv("Biomass_LLP_Data_Spring2019.csv")

#read in the remaining LLP data and put it into a dataframe called "LLP_Data"
LLP_Data<-read.csv("LLP_Data_Spring2019.csv")

#### Look at Data ####
#we can use View() to look at our dataframe
View(Biomass) 
View(LLP_Data) 

#### Manipulate and Clean Data ####
#using colnames() (not tidyverse) we can look at just the column names of our dataframes to get a feel for what we're working with
colnames(LLP_Data)
colnames(Biomass)
#when we do this, we can see that LLP_Data refers to treatment (lowercase t) and ID, whereas Biomass refers to Treatment (uppercase T) and PlantID. while these have the same information, R does not recognize that they are the same so we need to make the column headers the same 

#Let's rename the columns but while making a new dataframe so we know we've fixed the issue
#Make new data frame called LLP_Data2 using the dataframe LLP_Data
LLP_Data2<-LLP_Data %>%  #this is called a pipe, which makes code more readable and allows you to manipulate multiple things within one line of code
  #Change ID to PlantID
  rename(PlantID=ID) %>% 
  #Change treatment to Treatment
  rename(Treatment=treatment) %>% 
  #now, let's focus on just one date, the latest week from the experiment - week 9
  filter(Week==9)

#now that we've done that, we can merge together our two data frames for ease of looking at data - let's make a new data frame to do that
#Make a new data frame called Merged_Data using the dataframe LLP_Data2
Merged_Data<-LLP_Data2 %>% 
  #use left_join() to match rows from Biomass dataframe to the new dataframe
  left_join(Biomass) #now we can see that the biomass data is in our merged dataframe too

#Great! now we have a merged dataframe. Let's take a look at it to see what it looks like
View(Merged_Data)

#for the purposes of this course, let's just focus on three variables - stem number air temperature and alive biomass
#create a new data frame called Count_Data where we look at just these three variables and our treatments
Count_Data<-Merged_Data %>% 
  #select allows us to choose the columns we want to keep or remove. a minus sign (-) means remove, and no sign means keep
  select(Week,Plant_Species,PlantID,Treatment,Replicate,Stem_Number,Air_Temp,Alive) %>% 
  #now if we want to count up the total number of plants per species, we can use two functions - one is group_by() and the other count()
  #group_by() allows us to say to R that we need it to be thinking about our next task based on this grouping statement - so for this, we are saying, keep the data seperated by species
  group_by(Plant_Species) %>% 
  #now that it's grouped by plant species, let's ask R to count up the total number of plants in each species for this week for each treatment
  count(Treatment) %>%  #now we can view our dataframe and see how many individual plants there were in each treatment and species
  #don't forget to ungroup() or you might run into issues later!
  ungroup()

#now, instead let's find the mean stem number, temperature, and alive biomass  per species of plant in each treatment
Mean_Stem_Temp_Alive_Number<-Merged_Data %>% 
  select(Week,Plant_Species,PlantID,Treatment,Replicate,Stem_Number,Air_Temp,Alive) %>%
  #this time, we want to group by plant species AND treatment
  group_by(Plant_Species,Treatment) %>% 
  #to find the mean, we want to use a summarise() statement
  #here we're saying, take the column Stem Number and find the mean (ignoring all NAs). Once you find the mean, put it into a column called Stem_Num_Mean. Then, do the same for Stem_Num and Air_Temp. na.rm is a special function that allows us to tell R that there are NAs in our data and we want R to ignore them when calculating our mean.
  summarise(Stem_Num_Mean=mean(Stem_Number,na.rm = TRUE),Temp_Mean=mean(Air_Temp,na.rm = TRUE),Biomass_Mean=mean(Alive,na.rm = TRUE)) %>% 
  #don't forget to ungroup!
  ungroup()

#Great job!! now try this with your own data. what types of columns are you interested in keeping or removing? do you need to change the names of your columns?

