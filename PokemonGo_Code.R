dev.off()
pokemon <- read.csv("Pokemon_Go_Data.csv")
summary(pokemon)

##Cleaning Data
drops <- c("id", "submitdate","ipaddr","ATTENTION_filter1")
pokemon2<- pokemon[,!(names(pokemon) %in% drops)]

##New Variable: Attitude mean (3 is reversed)
##6 questions actually target the same thing. So we took the mean of the attitudes.
pokemon2$avg_attitude <- rowMeans(subset(pokemon2, select = c("attitude_attitude1", "attitude_attitude2", "attitude_attitude4", "attitude_attitude5", "attitude_attitude6")), na.rm = TRUE)

##New Variable2: stepattitude mean (2 is reversed)
pokemon2$avg_stepattitude <- rowMeans(subset(pokemon2, select = c("stepsattitude_attitudeB1", "stepsattitude_attitudeB3", "stepsattitude_attitudeB4", "stepsattitude_attitudeB5", "stepsattitude_attitudeB6")), na.rm = TRUE)

summary(pokemon2)
#age, education, Gender, avg_attitude, social_Sharing
#Interactions: attitude
#Model: Linear Regression
#stepsattitude: for only validation

colnames(pokemon2)
#Indepedent variables to be used for model building: age, educaiton, Gender, avg_attitude, social sharing
#Before using pokemon behavior: RecenecypastBeheavior_recencybike, RecencypastBehavior_recencywalk, RecencypastBehavior_recencyrun
#After using pokemon behavior: PokemonPastBehavior_pokPast1, PokemonPastBehavior_pokPast2, PokemonPastBehavior_pokPast3
#validation: avg_stepattitude

###########Models to find out the effect of the treatment(Using Pokemon Go)###########

##T-test

#App Usage = 1 or 2
inactive <- subset(pokemon2, app_usage_PokemonGoApp_pokemonusage1 == "1" | app_usage_PokemonGoApp_pokemonusage1 == "2", 
                    select = c(app_usage_PokemonGoApp_pokemonusage1, perceivedBehav_freqWalking, perceivedBehav_freqRunning, perceivedBehav_freqBikeing))
#App Usage > 2
active <- subset(pokemon2, app_usage_PokemonGoApp_pokemonusage1 != "1" | app_usage_PokemonGoApp_pokemonusage1 != "2", 
                  select = c(app_usage_PokemonGoApp_pokemonusage1, perceivedBehav_freqWalking, perceivedBehav_freqRunning, perceivedBehav_freqBikeing))

t.test(untreated$perceivedBehav_freqWalking, treated$perceivedBehav_freqWalking)
t.test(untreated$perceivedBehav_freqRunning, treated$perceivedBehav_freqRunning)
t.test(untreated$perceivedBehav_freqBikeing, treated$perceivedBehav_freqBikeing)

##1. Linear Regression 

##We would first check each model with walk variable, since walk is the most important part of Pokemon GO
##Bike and run are secondary physical activity as most of the Game functions (pokemon spawns and egg hatch) do not function optimally during these activities

##Regressing walk post game over usage and past walk
reg_walk_usage_past <-lm(pokemon2$PokemonPastBehavior_pokPast1~ pokemon2$app_usage_PokemonGoApp_pokemonusage1+pokemon2$perceivedBehav_freqWalking)
summary(reg_walk_usage_past)
(pokemon2$education)

##Checking residual patterns of the model
reg.res = resid(reg_walk_usage_past)
plot(pokemon2$app_usage_PokemonGoApp_pokemonusage1+pokemon2$perceivedBehav_freqWalking, reg.res, xlab = "Independent variables", ylab= "Residuals")



##Regressing walk post game over interaction of app usage and social sharing and past walk
reg_int_walk<-lm(pokemon2$PokemonPastBehavior_pokPast1~ pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$social_sharing+pokemon2$perceivedBehav_freqWalking)
summary(reg_int_walk)

##Checking residual pattern of the model
reg1.res = resid(reg_walk_usage_past)
plot( pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$social_sharing+pokemon2$perceivedBehav_freqWalking, reg1.res,xlab= "Independent varibables",ylab= "Residuals")

##Regressing walk over interaction of Past physical activity behavior x app usage, gender and social sharing of the user
reg_walk_usage_demo_past <-lm(pokemon2$PokemonPastBehavior_pokPast1~ pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$perceivedBehav_freqWalking
                              +pokemon2$Gender+pokemon2$social_sharing)
summary(reg_walk_usage_demo_past)
reg2.res = resid(reg_walk_usage_demo_past)
plot(pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$perceivedBehav_freqWalking+pokemon2$Gender+pokemon2$social_sharing, reg2.res, xlab ="Indpendent variables", ylab="Residuals")

##Regressing run over interaction of Past physical activity behavior x app usage, gender and social sharing of the user
reg_run_usage_demo_past <-lm(pokemon2$PokemonPastBehavior_pokPast2~ pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$perceivedBehav_freqRunning
                             +pokemon2$Gender+pokemon2$social_sharing)
summary(reg_run_usage_demo_past)
reg3.res = resid(reg_run_usage_demo_past)
plot(pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$perceivedBehav_freqRunning+pokemon2$Gender+pokemon2$social_sharing, reg2.res, xlab ="Indpendent variables", ylab="Residuals")

##Regressing bike over interaction of Past physical activity behavior x app usage, gender and social sharing of the user
reg_bike_usage_demo_past <-lm(pokemon2$PokemonPastBehavior_pokPast3~ pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$perceivedBehav_freqBikeing
                              +pokemon2$Gender+pokemon2$social_sharing)
summary(reg_bike_usage_demo_past)
reg4.res = resid(reg_bike_usage_demo_past)
plot(pokemon2$app_usage_PokemonGoApp_pokemonusage1*pokemon2$perceivedBehav_freqBikeing+pokemon2$Gender+pokemon2$social_sharing, reg2.res, xlab ="Indpendent variables", ylab="Residuals")



##2. Regression Discontinuity

#install packages - for ggplot, %>%, and gang
install.packages("tidyverse")
library(tidyverse)

#Create a total variable indicating the cumulative physical activity for Walking, Running and Biking 
pokemon2$total_activity_pokemon <- pokemon2$PokemonPastBehavior_pokPast1 + pokemon2$PokemonPastBehavior_pokPast2 + pokemon2$PokemonPastBehavior_pokPast3

# Mutate the dataframe to have an indicator for the treatment (i.e. app usage >= 2)
pokemon2<-mutate(pokemon2,over2 = as.integer(app_usage_PokemonGoApp_pokemonusage1 >= 2))

#Mutate the datafame to measure app usage relative to 2.
pokemon2 <- mutate(pokemon2, app = app_usage_PokemonGoApp_pokemonusage1 - 2)

#create a variable list to test as control variables in 3 different models
varlist <- c("age" = "All Age Groups",
             "education" = "All education levels",
             "social_sharing" = "Social Desirability")

#graphic of the discontinuity design with respect to the three control variables
pokemon2 %>%
  select(app_usage_PokemonGoApp_pokemonusage1, over2, one_of(names(varlist))) %>%
  gather(response, value, -app_usage_PokemonGoApp_pokemonusage1, -over2, na.rm = TRUE) %>%
  mutate(response = recode(response, !!!as.list(varlist))) %>%
  ggplot(aes(x = app_usage_PokemonGoApp_pokemonusage1, y = value)) +
  geom_point() +
  geom_smooth(mapping = aes(group = over2), se = FALSE, method = "lm",
              formula = y ~ poly(x, 32)) +
  geom_smooth(mapping = aes(group = over2), se = FALSE, method = "lm",
              formula = y ~ x, color = "black") +
  facet_grid(response ~ ., scales = "free_y") +
  labs(y = "Physical Activity", x = "App Usage")

#rd models with age, education and social media as control variables
Social_Media <- lm(log(total_activity_pokemon) ~ app_usage_PokemonGoApp_pokemonusage1 + over2 + social_sharing, data = pokemon2)
summary(Social_Media)

Education <- lm(log(total_activity_pokemon) ~ app_usage_PokemonGoApp_pokemonusage1 + over2 + education, data = pokemon2)
summary(Education)

Age <- lm(total_activity_pokemon ~ app_usage_PokemonGoApp_pokemonusage1 + over2 + age, data = pokemon2)
summary(Age)

#Final rd graphic design
ggplot(data = pokemon2,
       mapping = aes(x = app_usage_PokemonGoApp_pokemonusage1,
                     y = total_activity_pokemon, color = Test)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 2, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm")


##Exploratory Data Analysis

#ggpair plot
install.packages("ggplot2")
library(GGally)
ggpairs(pokemon2 [,c(1:3, 22:26)])

#Correlation Plot
install.packages("corrplot")
library(corrplot)

keeps <- c("RecencypastBehavior_recencybike",                   
           "RecencypastBehavior_recencywalk",                   
           "RecencypastBehavior_recencyrun",                    
           "perceivedBehav_freqWalking",                        
           "perceivedBehav_freqRunning" ,                       
           "perceivedBehav_freqBikeing",                      
           "app_usage_PokemonGoApp_pokemonusage1",              
           "social_sharing",
           "PokemonPastBehavior_pokPast1" ,                    
           "PokemonPastBehavior_pokPast2",                    
           "PokemonPastBehavior_pokPast3" , 
           "age","Gender","avg_attitude") ##keeping variables we want to check correlation for

pokemon2_keep <- pokemon2[,names(pokemon2) %in% keeps]
names(pokemon2_keep)
colnames(pokemon2_keep) <- c("age","Gender","lastwalk","lastrun","lastbike",
                            "oldwalk","oldrun","oldbike",
                            "appusage","socialshare","newwalk",
                            "newrun","newbike","attitude")
names(pokemon2_keep)
pokemon2_keep <- cor(pokemon2_keep)
corrplot(pokemon2_keep)


