#10/16/2023
#Scratch R script (that's why its in the scratch folder were we just mess around with data)

#load libraries
library(tidyverse)

    #read and inspect. read_csv from Tidyverse, instead read.csv (less info)
    surveys <- read_csv ("data/portal_data_joined.csv")
    head(surveys) #tibbles are a nicer version of tables
    summary(surveys)

#Q1: what's the type of column species_id? Of hindfoot_length?
head(surveys) #then read..
  #species_id = chr
  #hindfoot_length = dbl

      #other options
    typeof(surveys$species_id)
    typeof(surveys$hindfoot_length)
    
    class(surveys$hindfoot_length)
    class(surveys$hindfoot_length)

#Q2: how many rows and columns are in surveys?
head(surveys) # = 6 x 13 
length(surveys) 
nrow(surveys)
    
    #select cols from an existing data frame
    select (surveys, plot_id, species_id, weight) # dataframe, colsyouwanttokeep
    select (surveys, plot_id, weight_g=weight) # chooses and rename, eg add grams to weight col name
    select(surveys,-record_id,-species_id) # minus sign removes cols
    
    #how to change rows using conditions
    filter(surveys, year == 1995) # == comparison operator
    filter(surveys, year == 1995, plot_id ==7) # you can select multiple criteria
    filter(surveys, month == 2 | day == 20) # OR, returns | single value

#Q3: filter() surveys to records collected in November where hindfoot_length is creater than 36.0
filter(surveys, month==11, hindfoot_length>36)

#Q4: Fix these errors
filter(surveys, year == 1995) # = instead of ==
filter(surveys, plot_id == 2) # polt instead of plot

  #Pipes
  select(filter(surveys,year == 1995), plot_id, weight) #instead use pipes
  
  surveys_psw <- surveys %>%
    filter(year == 1995) %>%
    select (plot_id, weight)

#Q5: Use pipes to subset surveys to animals collected before 1995 retaining just the culmns year, sex and weight

surveys_new <- surveys %>%
  filter(year == 1995) %>%
  select(year, sex, weight)

  #Mutate - add/change columns place
surveys %>%
  mutate(weight_kg=weight/1000) %>% # gr to kg!
  view() #show us!!

surveys %>%
  mutate(weight_kg = weight/1000,
         weight_lb = weight*2.2) %>%
  view()

  #get rid of N/As!
  surveys %>%
    filter(!is.na(weight)) %>% # "!" before the function indicates to do the contrary! such as: take everything that is NOT NAs
    mutate(weight_kg = weight/1000,
           weight_lb = weight*2.2) %>%
    view() 

#Q6: Create a new data frame from the surveys data that meets the following criteria: contains only the species_id col and a new col called hindfoot_cm containing the hinfoot_length values (currently in mm) converted to centimiter. In this hindfoot_cm column, there are no NAs and all values are less than 3. HINT: think about how the commands should be ordered to produce this data frame!
  
 surveys %>%
    mutate(hindfoot_cm = hindfoot_length/10) %>% #make sure you create the col before manipulating!
    select(species_id, hindfoot_cm) %>%
    filter(hindfoot_cm <3)%>%
    filter(!is.na(hindfoot_cm)) %>%
    view()
    
  #Summarize
  surveys %>%
    group_by(sex)%>%
    summarize(mean_weight = mean (weight, na.rm=TRUE))
  
  surveys %>%
    drop_na(weight) %>%
    group_by(species_id,sex)%>%
    summarize(mean_weight = mean (weight), na.rm=TRUE, #na.rm to remove NaNs
                                  min_weight = min(weight),
                                  .groups = "drop") %>% #this makes the output not grouped - unique to summarize...alternatively use ungroup()
    arrange(desc(mean_weight))%>%           
     view()
  
#Q7: How many animals were caught in each plot_type surveyed?
 final_counts <- surveys %>%
   select(plot_type) %>%
   group_by(plot_type) %>%
   summarize(amount=n()) %>%
   view()

  
# Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). Also add the number of observations (hint: see ?n).
   surveys %>%
     group_by(species_id)%>%
     summarise(mean_hindfoot= mean (hindfoot_length),
               min_hindfoot = min (hindfoot_length),
               max_hindfoot = max (hindfoot_length),
               amount=n()) %>%
       view()
       
   
# Q9: What was the heaviest animal measured in each year? Return the columns year, genus, species_id, and weight.
   surveys %>%
    select(year, genus, species_id, weight) %>%
    group_by(year, genus, species_id,) %>%
    summarize(max_weight = max (weight)) %>%
    filter(!is.na(max_weight)) %>%
    arrange(desc(max_weight))%>%
    view()
    
    
