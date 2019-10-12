########Dataframe setup and cleaning
########
##Setting working directory
setwd("/home/arlisscoates/workspace/UFC_Shiny")
##Testing working directory(wd)
getwd()
##Loading tidyverse; contains dplyr and ggplot2
library(tidyverse)
##Loading data: "data.csv" within > "/home/arlisscoates/Documents/R/ArlissUFC/Shiny_Data"
ufc_data <- read.csv("data.csv")
##Set of data from csv including win_by and fight_type
ufc_data_raw <- read.csv("raw_total_fight_data.csv", sep=';')
names(ufc_data) <- tolower(names(ufc_data))
names(ufc_data_raw) <- tolower(names(ufc_data_raw))
##Concatenating ufc_data and ufc_data_raw in ufc_bind
ufc_bind <- data.frame(ufc_data$r_fighter,ufc_data$b_fighter,ufc_data$referee,ufc_data$date,ufc_data$weight_class,ufc_data$title_bout,ufc_data_raw$win_by,ufc_data_raw$last_round,ufc_data_raw$last_round_time,ufc_data_raw$winner)
##Translates all variable names to lowercase
names(ufc_bind) <- tolower(names(ufc_bind))
##Renaming variable names NOTE: when running this again, the right side of "=" must be given the correct column name; i.e "r_fighter" will be "ufc_data.r_fighter". This is the result of having run the code in pieces (after the first, R recognizes r_fighter is r_fighter, not ufc_data.r_fighter)
ufc_bind <- rename(ufc_bind, r_fighter = r_fighter, b_fighter = b_fighter, referee = referee, date = date, weight_class = weight_class, title_bout = ufc_data.title_bout, win_by = ufc_data_raw.win_by, last_round = ufc_data_raw.last_round, last_round_time = ufc_data_raw.last_round_time, winner = ufc_data_raw.winner)
##top 20 refs
ufc_bind_top20 = filter(ufc_bind, referee == 'Herb Dean' | referee == 'John McCarthy' | referee == 'Mario Yamasaki' | referee == 'Dan Miragliotta' | referee == 'Marc Goddard' | referee == 'Yves Lavigne' | referee == 'Steve Mazzagatti' | referee == 'Leon Roberts' | referee == 'Keith Peterson' | referee == 'Josh Rosenthal' | referee == 'Chris Tognoni' | referee == 'Jason Herzog')
##ufc_bind_top20 by interventions (decision vs non-decision)
#ufc_bind_stoppage <- ufc_bind_top20 %>% for (win in ufc_bind_top20$win_by)
#  if(win == "KO/TKO" | win == "Submission" | win == "DQ"){
 #   gsub(win, "Stoppage", ufc_bind_top20$win_by)}
  #    else{
   #     gsub(win,"Decision", ufc_bind_top20$win_by)
    #  }
##

###Better solution than for loop and conditional above:
ufc_bind_top20 <- ufc_bind_top20 %>% mutate(stoppage_decision = case_when(grepl("KO/TKO",win_by) ~ "Stoppage",
                                       grepl("Submission",win_by) ~ "Stoppage",
                                       grepl("Could Not Continue",win_by) ~ "Stoppage",
                                       grepl("DQ",win_by) ~ "Stoppage",
                                       grepl("TKO - Doctor's Stoppage",win_by) ~ "Stoppage",
                                       grepl("Decision - Unanimous",win_by) ~ "Decision",
                                       grepl("Decision - Majority",win_by) ~ "Decision",
                                       grepl("Other",win_by) ~ "NA",
                                       grepl("Overturned",win_by) ~ "NA",
                                       grepl("Decision - Split",win_by) ~ "Decision"))

ufc_bind_top20 <- 

                                      
#  if (win == "KO/TKO" | win == "Submission"){
sum(ufc_bind_stoppage)

####code for sorting referees by experience
##sort(table(ufc_data$referee),decreasing = TRUE))

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

##bar plot referee and win_by
ggplot(data=ufc_bind_top20) +
  geom_bar(aes(x=ufc_bind_top20$referee,fill=ufc_bind_top20$win_by)) +
  labs(title='Referees By Experience',
       x='Referee',
       y='Number of matches') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() + 
  theme_bw() +
  theme(legend.key=element_blank())

##Bar plot of referee by stoppage_decision
ggplot(data=ufc_bind_top20) +
  geom_bar(aes(x=referee,fill=stoppage_decision,position="dodge2")) +
  labs(title='Referees By Experience',
       x='Referee',
       y='Number of matches') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() + 
  theme_bw() +
  theme(legend.key=element_blank())

##scatter plot win_by and years by count
hist(ufc_bind_stoppage_top20$last_round,
     main="Round Stoppage by Ref",
     xlab="last_round",
     xlim=c(1,5),
     col="darkmagenta",
     freq=FALSE
)

##histogram of 
ggplot(data=ufc_bind_top20,aes(ufc_bind_to$last_round, position='dodge')) + 
  geom_histogram(binwidth=.5)
##
ggplot(ufc_bind_top20,aes(x=referee))
  geom_histogram((data=ufc_bind_top20, year == '1993')fill = "red", alpha = 0.2)

##barplot by stoppage_decision
ggplot(ufc_bind_top20, aes(x=last_round)) + 
  geom_histogram(stat='count') +
  geom_histogram(aes(x=ufc_bind_top20$last_round))

ggplot(ufc_bind_top20, aes(x=last_round)) +
  geom_histogram(stat)

##mbarplots of stoppage type per weightclass
ggplot(data=ufc_bind_top20) +
  geom_bar(aes(x=ufc_bind_top20$weight_class,fill=ufc_bind_top20$win_by,position='fill')) +
  labs(title='Referees By Experience',
       x='Weight Class',
       y='# of bouts') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() + 
  theme_bw() +
  theme(legend.key=element_blank())
##barplot of weightclass by stoppage_decision
ggplot(data=ufc_bind_top20) +
  geom_bar(aes(x=ufc_bind_top20$weight_class,fill=ufc_bind_top20$stoppage_decision,position='fill')) +
  labs(title='Referees By Experience',
       x='Weight Class',
       y='# of bouts') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() + 
  theme_bw() +
  theme(legend.key=element_blank())

##weight class by stoppage_decision STACKED
ggplot(data = ufc_bind_top20, aes(x = weight_class)) +
  geom_bar(aes(fill = stoppage_decision), position = 'fill') +
  labs(title='Weight class by win 100% STACK',
       x='weight class',
       y=''
  scale_fill_brewer(palette='Set1') +
  coord_flip() +
  theme_bw() +
  theme(legend.key=element_blank())

##weight class by win_by STACKED
ggplot(data = ufc_bind_top20, aes(x = weight_class)) +
  geom_bar(aes(fill = win_by), position = 'fill') +
  labs(title='Weight class by win-type 100% STACK',
       x='weight class',
       y='') +
  scale_fill_brewer(palette='Set1') +
  coord_flip() +
  theme_bw() +
  theme(legend.key=element_blank())

##Referee by 
ggplot(data=ufc_bind_top20, aes(date, )) +
  geom_point(aes())
         

       
       
##Weight class by round
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





