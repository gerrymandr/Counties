library(statsr)
library(xlsx)

#Include state geoids


#df is the data frame of the original census data csv including land area for each county
#df_projected is the data frame that we have to fill with information extracted from df, then export it as an excel
#df_SC is the data frame of counties in each state that are less than and above .5% state senate district population deviation coded and extraced by Sherlock
#df_HC is the data frame of counties in each state that are less than and above .5% state house district population deviation identified and extraced by Sherlock 
df = read.csv("C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/coarea-est2017-alldata.csv")
df_projected = read.csv("C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Projected 2020 Dataset Integratable with Districtmakr.csv")
df_SC = read.csv("C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/week_5/Senate_total_count.csv")
df_HC = read.csv("C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/week_5/House_total_count.csv")

#df_counties is the original data frame but with the state totals taken out, so we can just manipulate county information
#df_states is the state information from the original data frame extracted to a seperate data frame
df_counties = df[which(df$COUNTY != 000),]
df_states = read.csv("C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/StateMaster.csv")

#This command fills the 2017PopEst in the projected data frame with the PopEstimate2017 from the original data frame
df_2017PopEst = df_states[,17, drop=FALSE]
df_projected$X2017PopEst <- df_2017PopEst$POPESTIMATE2017

#Setting all constants
#This command fills the CongDistPopEst in the projected data frame with the the 'real' projected population estimate 745,540.26
#This command fills the Deviation column in the projected data frame with the 'general' expected population deviation the to ensure population equality in a state by state case +/- 0.5%
#specific population deviation rules by state still has to be accounted 
df_projected$CongDistPopEst <- 745540.26
df_projected$Deviation <- "+/- 0.5%"

#This command devides the 2017 populatino estimate with the senate numbers for each state to get the senate district population estimate and adds it as a new column in df states
#This command fills the df projected Senate District Population Estimate column with the df states' SDPE column
df_states <- transform(df_states, SDPE = POPESTIMATE2017 / Senators)
df_projected$SenateDistrictPopEst <- df_states$SDPE

#These lines function the same as the last two lines but estimates house district population size instead of senate
df_states <- transform(df_states, HDPE = POPESTIMATE2017 / Representatives)
df_projected$HouseDistPopEst <- df_states$HDPE


#countover is a data frame of every county with a population size over the congressional district deviation of .5%
#countunder is a data frame of every county with a population size under the congressional district deviation of .5%
#countundertenth is a data frame of every county with a population size under one tenth of the size of a projected congressional district of 745540.26 people
countover = df_counties[which(df_counties$POPESTIMATE2017 >= 745540.26+(745540.26*.005)),]
countunder = df_counties[which(df_counties$POPESTIMATE2017 <= 745540.26 - (745540.26*.005)),]
countundertenth = df_counties[which(df_counties$POPESTIMATE2017 <= 745540.26*.1),]


#Exported countover, countunder,and countundertenth
write.csv(countover, "C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/CountiesOverDeviation.csv")
write.csv(countunder, "C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/CountiesUnderDeviation.csv")
write.csv(countundertenth, "C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data/CountiesUnderOneTenthCDP.csv")



#Loops through df_projected and puts in the number of counties over deviation in its respective column
for(i in 1:51){
df_projected$CountiesOverDeviation[i] <- length(which(countover$STATE == i))
}

#Does the same as above but for counties under
for(i in 1:51){
df_projected$CountiesUnderDeviation[i] <- length(which(countunder$STATE == i))
}

#Does the same as above but for counties under one tenth
for(i in 1:51){
df_projected$CountiesUnder1.10thCDP[i] <- length(which(countundertenth$STATE == i))
}

#All of Sherlock's Senate and House county information placed in their respective df projected column
df_projected$CountiesOverSDP <- df_SC$senate_above_count
df_projected$CountiesUnderSDP <- df_SC$senate_below_count
df_projected$CountiesOverHDP <- df_HC$house_above_count
df_projected$CountiesUnderHDP <- df_HC$house_below_count


#Exported incomplete df_projected to send to Moon
write.csv(df_projected, "C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data Set.csv")

#Exported complete df_projected to send to Moon
write.csv(df_projected, "C:/Users/odimo_000/SkyDrive/College/VRDI/Week 5 Project - Counties/Data Set Complete.csv")
