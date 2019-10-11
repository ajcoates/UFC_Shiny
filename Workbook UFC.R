
##Setting working directory
setwd("/home/arlisscoates/Documents/R/ArlissUFC/Shiny_Data")
##Testing working directory(wd)
getwd()
##Loading tidyverse; contains dplyr and ggplot2
library(tidyverse)
##Loading data: "data.csv" within > "/home/arlisscoates/Documents/R/ArlissUFC/Shiny_Data"
ufc_data <- read.csv("data.csv")
##Set of data from csv including win_by and fight_type
ufc_data_raw <- read.csv("raw_total_fight_data.csv", sep=';')
##Concatenating ufc_data and ufc_data_raw in ufc_bind
ufc_bind <- data.frame(ufc_data$r_fighter,ufc_data$b_fighter,ufc_data$referee,ufc_data$date,ufc_data$weight_class,ufc_data$title_bout,ufc_data_raw$win_by,ufc_data_raw$last_round,ufc_data_raw$last_round_time,ufc_data_raw$Winner)
ufc_bind <- ufc_bind()
##Translates all variable names to lowercase
names(ufc_bind) <- tolower(names(ufc_bind))
##top 20 refs
ufc_bind_top20 = filter(ufc_bind, referee == 'Herb Dean' | referee == 'John McCarthy' | referee == 'Mario Yamasaki' | referee == 'Dan Miragliotta' | referee == 'Marc Goddard' | referee == 'Yves Lavigne' | referee == 'Steve Mazzagatti' | referee == 'Leon Roberts' | referee == 'Keith Peterson' | referee == 'Josh Rosenthal' | referee == 'Chris Tognoni' | referee == 'Jason Herzog')#####counting refs

####code for sorting referees by experience
##sort(table(ufc_data$referee),decreasing = TRUE))

##
ggplot(data=ufc_bind)
##Translates all variable names to lowercase to make calling variables easier
names(ufc_data) <- tolower(names(ufc_data))
##Creates a dummy df  (ufc_data1) to experiment on
ufc_data1 <- ufc_data
##Replaces all blank fields with NA
ufc_data1[ufc_data1==""]<- NA
##Removes rows with NAs across whole dataset
ufc_data1 <- na.omit(ufc_data1, cols=Referee)
##Removes rows with NAs in variable=Referee ONLY
ufc_data1 <- na.omit(ufc_data1, col=Referee)
##Sorts referees by number of fights reffed
referees_sorted <- sort(table(ufc_data$referee),decreasing = TRUE)
referees_sorted
##Referees df; takes r fighter, b fighter, referee, winner, title_bout
referees <- data.frame(ufc_data1$r_fighter,ufc_data1$b_fighter,ufc_data1$referee,ufc_data1$winner,ufc_data1$title_bout)

##Bar plot of title bouts vs non title, referee
ggplot(data=ufc_bind_top20) +
  geom_bar(aes(x=ufc_bind_top20$referee,fill=ufc_bind_top20$title_bout)) +
  labs(title='Referees By Experience',
       x='Referee',
       y='# of bouts') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() + 
  theme_bw() +
  theme(legend.key=element_blank())

##Filtered to reflect top referees so as not to clutter graph
order(referee)
refereesFiltered = semi_join(referees, topreferee_filtered, by = 'referee')

ggplot(data=topreferee_filtered) +
  geom_bar(aes(x=reorder(), position='dodge') +
  labs(title='Referees by bout type',
       x='referee',
       y='# of bouts') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() + 
  theme_bw() +
  theme(legend.key=element_blank())
                   

