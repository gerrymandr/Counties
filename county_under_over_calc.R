#These functions create a series of tables and counts for the number of counties
#that are 0.5% above or below the average Congressional/State Senate/State House
#district population needed for one district.

#input of data files
coarea.est2017.alldata <- read.csv("C:/Users/sherl/Miniconda3/envs/vrdi/vrdi_data/week_5/coarea-est2017-alldata.csv")
StateMaster <- read.csv("C:/Users/sherl/Miniconda3/envs/vrdi/vrdi_data/week_5/StateMaster.csv")

#creates list of state abbreviation
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
            "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
            "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
            "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
            "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

##For Congress
##Outputs two .csv files for each state: one of counties +0.5% required population
#and one of counties -0.5% the required population.
state_in <-51 #for the for function
for(state_in in 1:state_in) { #this for function runs through every state plus DC
   state_up_005 <- 745540.3*1.005 #district average at 100.5%
   state_down_005 <- 745540.3*0.995 #district average at 99.5%
   #picks out rows from the data frame that belong to the state (each state is a different number, from 1 to 51, plus DC)
   counties_temp <- coarea.est2017.alldata[which(coarea.est2017.alldata$STATE == state_in),]
   #drops state from counties (the data frame has county data as well as the full state as the county 0,
   #which doubles the population, as each state is counted twice, so this corrects that)
   drop_state <- counties_temp$COUNTY > 0
   counties <- counties_temp[drop_state,]
   #creates a data frame for the counties above 0.5%
   county_up_005 <- counties$POPESTIMATE2017 > state_up_005
   counties_above <- counties[county_up_005,]
   #creates a data frame for the counties below 0.5%
   county_down_005 <- counties$POPESTIMATE2017 < state_down_005
   counties_below <- counties[county_down_005,]
   #outputs both data frames to a csv
   write.csv(counties_above, paste(state_in,'Above','csv',sep='.'))
   write.csv(counties_below, paste(state_in,'Below','csv',sep='.'))
}

##For State Senate Districts (State Legislature)
##Outputs two .csv files for each state: one of counties +0.5% required population
#and one of counties -0.5% the required population.state_in <-51
##Writes an extra .csv tabulating the total numbers of counties above and below
#for easy reference.
state_in <-51
#these empty lists will be filled in by the simple total of the counties above and below
senate_above_count <- c(); 
senate_below_count <- c();
for(state_in in 1:state_in) {
   senate_pop <- StateMaster$SDPE[state_in]
   state_up_005 <- senate_pop*1.005
   state_down_005 <- senate_pop*0.995
   counties_temp <- coarea.est2017.alldata[which(coarea.est2017.alldata$STATE == state_in),]
   drop_state <- counties_temp$COUNTY > 0
   counties <- counties_temp[drop_state,]
   county_up_005 <- counties$POPESTIMATE2017 > state_up_005
   counties_above <- counties[county_up_005,]
   county_down_005 <- counties$POPESTIMATE2017 < state_down_005
   counties_below <- counties[county_down_005,]
   write.csv(counties_above, paste(state_in,'Above','State_Senate','csv',sep='.'))
   write.csv(counties_below, paste(state_in,'Below','State_Senate','csv',sep='.'))
   
   #this code tabulates the simple above and below totals
   senate_above_count <- c(senate_above_count, nrow(counties_above));
   senate_below_count <- c(senate_below_count, nrow(counties_below));
}
#creates the data frame for the above and below
senate_total_count <- data.frame(states, senate_above_count, senate_below_count)
write.csv(senate_total_count, "Senate_total_count.csv")

#The same as State Senate but for State House Districts
state_in <-51
house_above_count <- c();
house_below_count <- c();
for(state_in in 1:state_in) {
   house_pop <- StateMaster$HDPE[state_in]
   state_up_005 <- house_pop*1.005
   state_down_005 <- house_pop*0.995
   counties_temp <- coarea.est2017.alldata[which(coarea.est2017.alldata$STATE == state_in),]
   drop_state <- counties_temp$COUNTY > 0
   counties <- counties_temp[drop_state,]
   county_up_005 <- counties$POPESTIMATE2017 > state_up_005
   counties_above <- counties[county_up_005,]
   county_down_005 <- counties$POPESTIMATE2017 < state_down_005
   counties_below <- counties[county_down_005,]
   write.csv(counties_above, paste(state_in,'Above','State_House','csv',sep='.'))
   write.csv(counties_below, paste(state_in,'Below','State_House','csv',sep='.'))
   
   house_above_count <- c(house_above_count, nrow(counties_above));
   house_below_count <- c(house_below_count, nrow(counties_below));
}
house_total_count <- data.frame(states, house_above_count, house_below_count)
write.csv(house_total_count, "House_total_count.csv")