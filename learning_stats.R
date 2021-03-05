##### YAY FOR LEARNING STATS!!! #####

#The first thing to do is set your working directory - this is where on YOUR computer you have saved
#your files (where is this R script stored on your computer?)
setwd("~/Box Sync/PhD Work/Teaching/Bio499_R")
search()

#next we want to load in any packages that are not pre-included in R that we want to use
install.packages("codyn") #stands for community dynamics - this is a commonly used ecology package
library(codyn)
install.packages("ggplot2") #there's a separate tutorial all about this! (for making graphs)
library(ggplot2)

#We are going to use quite a few different data sets to learn different statistical analyses. This is 
#because depending on what your data looks like (hence why we are using ggplot), you will need to 
#analyze your data in different ways

#Statistical tests allow us to objectively look for relationships in our data. Most of the time 
#we are looking to see if groups are significantly different from one another, or if a relationship
#between our variables is signficant. Usually, we want to end up with a p-value. A p-value <0.05 is
#considered significant, while a p-value >0.05 is not significant. There is a little grey area around 
#0.05, but the closer the number gets to zero, the less and less likely it is that our results occurred 
#just do to random chance and the more certain we are that the relationship we found in our data is real

#### T Tests ####

#Let's start with a t-test. A t-test is what we will use if we have 2 groups and we want to compare
#the differences in averages between those 2 groups

#For this example, let's use the same biomass dataset you were using in the Tidyverse tutorial
Biomass <- read.csv("Biomass_LLP_Data_Spring2019.csv") #pull the data set into R and give it a new name

#In this dataset, we have 2 groups (control and heatwave) and we want to see if these groups differ in 
#average alive biomass. We might also phrase this as, did the heatwave significantly reduce alive biomass
#compared to controls? Let's find out with a t-test!
t.test(data = Biomass, Alive ~ Treatment) #t tests take the formula y ~ x (how does y change with x)
#looking at the output, the mean for the control group is 0.341 g and the mean for the heatwabe group is 0.215
#our p-value is 0.0006, meaning there is a highly significant difference in our treatments, so a heatwave did reduce
#alive biomass. Now you try the same thing with dead biomass! (I get a p value of 0.181)


#### Simple Linear Regression ####

#A simple linear regression allows us to test if there is a linear relationship between two variables.
#IE does a change in x result in a "constant" change in y? A linear regression works to fit a line that correlates 
#an independent and dependent variable

#For this example, we are going to use one of R's built in datasets called Loblolly. 
View(Loblolly)

#This data set looks at how loblolly pine trees grow as they age. Before we can use a linear regression, we need to 
#actually plot our data and see if it looks like our data has a linear relationship
ggplot(Loblolly, aes(x = age, y = height)) +
  geom_point() #use ggplot to make a scatterplot of age of tree vs height of tree

#Looking at this graph, as age increases, height also increases in what looks like a very linear way. A change
#in age will result in a similar change in y no matter what age you pick. There is no apparent curve or odd shape 
#to our data that would make it seem like a linear relationship is not a good fit. 

#So, now we can use a statistical test to see if a linear fit to our data does a good job of explaining this relationship. 
linear_reg <- lm(data = Loblolly, height ~ age) #lm is the code for a linear regression, and we use the same y ~ x formula
summary(linear_reg) #summary gives us the output of our linear regression test (ie how did our model do?)
#In the output, you will see lots of different things. Most important is the slope estimate, the p value, and R^2
#The slope estimate is in the table under estimate next to age (2.590) - this # is +, so we there is a positive relationship between age and height 
#(this should make sense - trees get bigger as they get older!)
#There are 2 p values (<2.2e-16)- 1 for the slope estimate and 1 for the overall model. Currently they are the same, but we will come back to this later
#But, our p value is reallllyyy small, meaning a linear relationship is a really good fit of our data
#2 kinds of R-squared: People most commonly report adjusted but either way the # should be similar. 0.9797. This means that >97% of the variation in our data
#can be explained by our model fit. This is a REALLY good number and hardly ever happens in real life. 


#### ANOVAS - aka Analysis of Variance ####

#While a t-tests compares averages of TWO groups, what happens if we have more than 2 groups? We can use an ANOVA. An ANOVA will tell you if 
#there is a significant difference between your groups. THEN, if our p value is significant, we can use what's called a post-hoc test 
#to test which groups are different from one another. 

#To demonstrate this, let's use a different dataset in R called PlantGrowth. This is comparing plant weight (biomass) between 3 different groups:
#one control group and two treatments
View(PlantGrowth)

#We will start by graphing with a boxplot. We could use a scatterplot, but that wouldn't really make sense to do because the "group" variable is not consecutive - it's 
#not like trt1 comes after ctrl in any sort of sequential way. Really, we have 3 independent groups that we just want to see if they are different from one another
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot() #just by looking at our boxplot, it does look like the averages (center black line in each box), may be different from one another

#Now we can use an anova to test and see if the average weight for each group is significantly different
anova1 <- aov(data = PlantGrowth, weight ~ group) #this line of code should look very familiar - it's much the same as what we've previously done!
summary(anova1) 
#Our output shows our p-value is 0.0159, meaning average weight does significantly differ between our groups
#BUT, how do we know which groups are different from one another?

#We can use post-hoc testing to figure this out. We use what is called a tukey test to see which groups significantly differ
TukeyHSD(anova1)
#This output looks a bit different than the previous ones. What it's doing is looking at all pairwise comparisons to see which are different
#For ex, the first line compares trt1 to ctrl by giving us a diff in means (-0.371), a confidence interval (lwr and upr), and a p value (0.3908)
#You can see that the only significant difference in average weight is between our 2 trt groups (trt 1 and 2) on line 3 of the table, with a p value of 0.012



##### Multiple Linear Regressions #####

#Let's think back to our linear regression. We said this demonstrated how y relates to x in a linear way. But, what if y relates to more than 1 variable? 
#If we have multiple predictor (x) variables and we want to know how they each help to explain what happens with y, we can use a multiple linear regression to test this

#For this example, we are going to use a dataset that comes with the R package codyn called collins08. This data looks at how fire frequency affects species abundance over time
#We want to know how species abundance changes with time and fire frequency
multreg <- lm(data = collins08, abundance ~ year + replicate) 
summary(multreg)
#Note the + sign between year and replicate. This means we want to know if each of these predictors contribute to abundance. If we had used a *, that would indicate we think there is an
#interaction between our predictors. It all depends on the kind of question we are asking and what our data is!
#p value is 0.334 for overall model and insignificant for each predictor, so year and replicate do not help explain abudance

#Just for fun, try it with * instead of + and see how the summary output is different
multreg2 <- lm(data = collins08, abundance ~ year * replicate) 
summary(multreg2)
#Now we see a line in the table for year, replicate, and year:replicate. This 3rd line is for the interaction between year and replicate. 
#This is just for practice b/c there again is no reason to think year and replicate are interacting


##### Generalized Linear Model #####

#Up until now, we have been assuming things about our data. We have been assuming that our data is normally distributed and the variances are equal (our groups have equal amounts of variation)
#But, this is not always the case. If we have data that breaks these assumptions, we can use a generalized linear model instead. This still fits a relationship to our data, but isn't quite so 
#stringent on whether or not the relationship is actually linear and how our data looks

#For ex, in the collins08 data, let's make a histogram of abundance data
histogram(data = collins08, ~ abundance)
#This is definitely not normally distributed (bell-shaped curve) and is really skewed

#So maybe a generalized linear model would be a better model type for our data
gen_lin_model <- glm(data = collins08, abundance ~ year + replicate) #note glm instead of lm
summary(gen_lin_model)
#still nothing is significant, but that's ok!


##### Community Analyses ####

#Let's think about some other ecology-specific analyses we may want to do with our data
#The codyn package gives us all kinds of analyses we can do!
?`codyn-package` #look through the help page to see all kinds of analyses you can do; there is SO much depending on the question you are asking!

#As some examples, let's look at the pplots dataset from codyn
View(pplots)
#This dataset looked at how different nitrogen treatments affected species abundance over time. They used multiple
#blocks (replicates) and had multiple plots for the different treatments in each block
#Note that this is just a portion of the data; there is not data for all 48 plots

#Let's start by looking at richness and evenness (2 important community metrics)
pplots_structure <- community_structure(pplots, abundance.var = "relative_cover", replicate.var = "plot", time.var = "year")
View(pplots_structure)
#Now we have a new data frame with richness and evenness for each plot/year
#From here we can do all kinds of things with this data - make figures, join with other datasets, etc


##### Mixed Effects Models #####

#Let's do one more thing using the pplots data again, but we are going to combine it with our pplots_structure data
#If you look at pplots, you'll see all the species are separated within a plot, but pplots_structure is combined species data
#Using what you learned from tidyverse, we can do this!

install.packages("tidyverse") 
library(tidyverse)

pplots2 <- pplots %>%
  group_by(treatment, plot, block, year) %>%
  summarise(total_cover = sum(relative_cover)) %>% #b/c pplots uses relative cover, all the indiv species data should add up to 1 (or 100% cover)
  ungroup()
View(pplots2)
#If you look at pplots2, total cover is 1 for everything, but that's what we expected bc all the indiv species cover should add up to 1
#This will not always be the case depending on your data, but for the sake of practice we are using this data

pplotsjoin <- full_join(pplots2, pplots_structure) 

#Merging these 2 datasets will now let us do analyses with our richness and evenness calculations from above

#Thinking big picture, what is the question you typically want to know? Usually, we want to know how our treatment affects our response variables
#In this case, we want to know if the diff nitrogen treatments affect the plant community - we will look at richness for our example

#Remember earlier when we talked about linear regressions and finding relationships between our variables? We are going to do that again here, but we 
#have a few more things to consider. 
#When we analyze data like this, we must think about what factors are fixed and what factors are random. 
#Fixed = the thing we control for - ie we are specifically manipulating (usually your treatment)
#Random = what we can't control for that may add random variation to our data (in ecology, usually our block or site, etc), meaning when we think about our 
#plot location, where they are located may just have inherent differences from one another, and we need to control for that in our stats

#Therefore, usually in ecology we want to used what are called Mixed Effects Models! (meaning we have random and fixed variables)
#We need to install a new package called nlme to do mixed models

#Side note, if you ever have issues with packages loading and get an error that it cannot be unloaded bc of another package, use this code and say "yes" to restart R session
#Usually when you restart R you have to reload all your packages again, but sometimes it's unavoidable
install.packages("nlme", dependencies = TRUE, repos = "http://cran.us.r-project.org")
library(nlme)

mixed <- lme(data = pplotsjoin, richness ~ treatment * year, random = ~1|block, control = lmeControl(returnObject = TRUE))
summary(mixed) #full output
anova(mixed) #overall model output
#This mixed model code is putting together nearly everything we have learned. We used * instead of + here b/c there is reason to believe that the 
#treatment could be affected by year of study (they could be related)
#We used block as our random effect bc just based on the block you are looking at, something could be happening to affect the results

#From the overall anova, we see treatment and year independently affect species richness, but treatment by year (interaction) does not



##### THE END #####

#And by the end, of course I mean just the beginning of your stats knowledge! Good job!!! :) 





