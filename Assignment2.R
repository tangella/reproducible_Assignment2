# load the data
storm_data <- read.csv("repdata_data_StormData.csv")

names(storm_data)

# compute the fatal and injury data by event type and order them 
# (highest to lowest)
fatal <- aggregate(FATALITIES ~ EVTYPE, storm_data, FUN = sum)
fatal <- fatal[order(-fatal$FATALITIES), ]
injury <- aggregate(INJURIES ~ EVTYPE, storm_data, FUN = sum)
injury <- injury[order(-injury$INJURIES), ]

# bar graphs for top 10 
par(mfrow = c(1, 2), mar = c(14, 4, 4, 4))
barplot(fatal[1:10,]$FATALITIES, las = 3, names.arg = fatal[1:10,"EVTYPE"], 
        main = "Top 10 Events with \nHighest Fatalities", 
        ylab = "Number of fatalities", col = "blue")
barplot(injury[1:10,]$INJURIES, las = 3, names.arg = injury[1:10,"EVTYPE"], 
        main = "Top 10 Events with \nHighest Injuries", 
        ylab = "Number of injuries", col = "blue")

#from data find the multipliers for k,m,B and other values 
damage_multiplier <- data.frame(
                        PROPDMGEXP = c ("-","?","+","","0","1","2","3","4",
                                        "5","6","7","8","h","H",
                                        "K","m","M","B"),
                        MLTPLYR = c(0,0,0,0,1,10,100,1000,10000,
                                    100000,1000000,10000000,100000000,
                                    100,100,1000,1000000,1000000,1000000000))
damage_multiplier2 <- data.frame(
                CROPDMGEXP = c ("?","","0","2","k","K","m","M","B"),
                MLTPLYR2 = c(0,0,1,100,1000,1000,1000000,1000000,1000000000))

# merge the data to get the MLTYPLR, MLTYPLYR2 added to the data set

storm_data <- merge(storm_data, damage_multiplier, by = "PROPDMGEXP")

# compute the property value by 
storm_data$PROPDMGVAL <- storm_data$PROPDMG * storm_data$MLTPLYR

storm_data <- merge(storm_data, damage_multiplier2, by = "CROPDMGEXP")

storm_data$CROPDMGVAL <- storm_data$CROPDMG * storm_data$MLTPLYR2

# compute the property and crop damage data by event type and order them 
# (highest to lowest)
propdmg <- aggregate(PROPDMGVAL ~ EVTYPE, storm_data, FUN = sum)
propdmg <- propdmg[order(-propdmg$PROPDMGVAL), ]

cropdmg <- aggregate(CROPDMGVAL ~ EVTYPE, storm_data, FUN = sum)
cropdmg <- cropdmg[order(-cropdmg$CROPDMGVAL), ]

# bar graphs for top 10 
par(mfrow = c(1, 2), mar = c(14, 4, 4, 4))

barplot(propdmg[1:10,]$PROPDMGVAL/(10^9), las = 3, 
        names.arg = propdmg[1:10,"EVTYPE"], 
        main = "Events with Highest \nProperty damages", 
        ylab = "Value of Damage in Billions", col = "blue")
barplot(cropdmg[1:10,]$CROPDMGVAL/(10^9), las = 3, 
        names.arg = cropdmg[1:10,"EVTYPE"], 
        main = "Events with Highest \nCrop damages", 
        ylab = "Value of Damage in Billions", col = "blue")


