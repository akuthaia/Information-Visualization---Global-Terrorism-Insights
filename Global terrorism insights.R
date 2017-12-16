#Install Libraries
library(ggmap)
library(ggplot2)
library(maptools)
library(maps)
library(alluvial)
library(plyr)

#Reading the file
myfile <- file.choose()

terror.data <- read.csv(file = myfile, header = TRUE, stringsAsFactors = FALSE)


#************************************************************************
#Cleaning the Data - Picking relevan columns
main_df <- terror.data[,c(1,2,3,4,9,11,12,13,14,15,30,36,38,59,83,99,102,108)]
#Picking last 20 years data
main_df <- main_df[main_df$iyear > 1995,]
#Filtering Targets
main_df$targtype1_txt <- gsub("Government (Diplomatic)","Government", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Government (General)","Government", main_df$targtype1_txt)
#main_df$targtype1_txt <- gsub("Police","Police & Military", main_df$targtype1_txt)
#main_df$targtype1_txt <- gsub("Military","Police & Military", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Airports & Aircraft","Transportation", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Educational Institution","Educational/ Religious Institution", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Religious Figures/Institutions","Educational/ Religious Institution", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Violent Political Party","Government", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Unknown","Other", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Telecommunication","Other", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Food or Water Supply","Utilities", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Terrorists/Non-State Militia","Other", main_df$targtype1_txt)
main_df$targtype1_txt <- gsub("Abortion Related","Other", main_df$targtype1_txt)

#************************************************************************
# PLOT 1 - World Map - How and where has terrorism affected the world?

mp <- NULL
mapWorld <- borders("world", colour="grey", fill="darkorange") # create a layer of borders
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=main_df$longitude, y=main_df$latitude) 
                     ,color="brown4", size=.5, na.rm = TRUE) 
mp <- mp + labs(x = "Longitude", y = "Latitude", title = "Attack Locations")
mp

#************************************************************************
# PLOT 2 - Number of attacks vs number of fatalities over the years

plot2.data <- tapply(main_df$nkill, list(year = main_df$iyear)
                     , FUN = sum, na.rm = TRUE) #Aggregate of number of fatalities over the years

plot2.data2 <- tapply(main_df$eventid, list(year = main_df$iyear)
                     , FUN = length) #Aggregate of number of attacks over the years

year.name <- rownames(plot2.data)
year.name2 <- rownames(plot2.data2)

plot2.df <- data.frame(year = year.name, fatalities = plot2.data)
plot2.df$attacks <- plot2.data2

#plot2.df2 <- data.frame(year = year.name2, count.s = plot2.data2)

df2 <- ggplot(plot2.df, aes(x = year))
df2 <- df2 + geom_bar(aes(y = fatalities),stat = "identity", color = "black", fill = "red")
df2 <- df2 + theme(axis.text.x = element_text(angle=90)) 
df2 <- df2 + labs(x="Year", y= "Number of Fatalities", title = "Number of Attacks and Fatalities")
df2 <- df2 + geom_line(aes(y = attacks*2, group = 1))
df2 <- df2 + scale_y_continuous(sec.axis = sec_axis(~.*.5, name = "Number of Attacks"))
df2


#************************************************************************
# Plot 3 - WHich region is the most affected by terrorism

plot3.agg <- aggregate(main_df$region 
                       ,by = list(main_df$iyear, main_df$region_txt)
                       , FUN = length
)

df3 <- ggplot(data = plot3.agg, aes(x = Group.1, y = x, group = Group.2)) +
  stat_smooth(se = FALSE, method = "lm", formula = y ~ poly(x, 10), aes(color = Group.2)) +
  scale_y_continuous(limit=c(0,NA))
df3 <- df3 + labs(x = "Number of Attacks", y = "Year" , title = "Affected Regions over the Years")
df3

#************************************************************************
# Plot 4 - Comparison of fatalities and injured in each region

df4 <- main_df

options(scipen = 999)

df4$nkill[is.na(df4$nkill)] <- 0

df4$nwound[is.na(df4$nwound)] <- 0

data.aggreagate <- aggregate(list(fatal = df4$nkill, injured = df4$nwound), list(atyp = df4$region_txt)
                            , FUN = sum)
data.aggreagate$perfatal <- 100*data.aggreagate$fatal/(data.aggreagate$injured+data.aggreagate$fatal)
data.aggreagate$perinj <- 100*data.aggreagate$injured/(data.aggreagate$injured+data.aggreagate$fatal)


data.aggreagate1 <- data.aggreagate[,c(1,4)]
data.aggreagate1$type <- "Fatal"
data.aggreagate2 <- data.aggreagate[,c(1,5)]
data.aggreagate2$type <- "Injured"

colnames(data.aggreagate1) <- c("region","number", "group")
colnames(data.aggreagate2) <- c("region","number", "group")

#combine 2 Data Frames
data.plot4 <- rbind(data.aggreagate1, data.aggreagate2)

#Preping data for the Piecharts
data1 <- data.plot4[data.plot4$region == "Asia",]
data3 <- data.plot4[data.plot4$region == "Europe",]
data4 <- data.plot4[data.plot4$region == "Middle East & North Africa",]
data5 <- data.plot4[data.plot4$region == "North America",]
data6 <- data.plot4[data.plot4$region == "South America",]
data7 <- data.plot4[data.plot4$region == "Sub-Saharan Africa",]

#par(mfrow = c(3,3))

df4_pie1 <- ggplot(data1, aes(x="", y=number, fill=group))+
  geom_bar(width = 1, stat = "identity") + labs(title = "Asia")
pie1 <- df4_pie1 + coord_polar("y", start=0) #Asia
pie1

df4_pie3 <- ggplot(data3, aes(x="", y=number, fill=group))+
  geom_bar(width = 1, stat = "identity") + labs(title = "Europe")
pie3 <- df4_pie3 + coord_polar("y", start=0) #Europe
pie3

df4_pie4 <- ggplot(data4, aes(x="", y=number, fill=group))+
  geom_bar(width = 1, stat = "identity") + labs(title = "Middle East & North Africa ")
pie4 <- df4_pie4 + coord_polar("y", start=0) #Middle East & North Africa 
pie4

df4_pie5 <- ggplot(data5, aes(x="", y=number, fill=group))+
  geom_bar(width = 1, stat = "identity") + labs(title = "North America")
pie5 <- df4_pie5 + coord_polar("y", start=0) #North America
pie5

df4_pie6 <- ggplot(data6, aes(x="", y=number, fill=group))+
  geom_bar(width = 1, stat = "identity") + labs(title = "South America")
pie6 <- df4_pie6 + coord_polar("y", start=0) #South America
pie6

df4_pie7 <- ggplot(data7, aes(x="", y=number, fill=group))+
  geom_bar(width = 1, stat = "identity") + labs(title = "Sub-Saharan Africa")
pie7 <- df4_pie7 + coord_polar("y", start=0) #Sub-Saharan Africa
pie7

#************************************************************************
# Plot 5 - Aluvial Diagram - Insight into the 10 terrorist groups (Sorted by number of attacks), the regions they concentrate in and the target types

df_10 <- main_df
df_10 <- subset(df_10, df_10$gname != "Unknown")

#Test Code TO CHECK top 10
#test <- aggregate(df_10$eventid 
#                  ,by = list(df_10$gname)
#                  , FUN = length)
#c <- head(arrange(test,desc(x)), n = 10)

df_10 <- subset(df_10, df_10$gname == "Taliban" | df_10$gname == "Islamic State of Iraq and the Levant (ISIL)"
                | df_10$gname == "Al-Shabaab" | df_10$gname == "Boko Haram" 
                | df_10$gname == "Communist Party of India - Maoist (CPI-Maoist)" | df_10$gname == "New People's Army (NPA)"
                | df_10$gname == "Revolutionary Armed Forces of Colombia (FARC)" | df_10$gname == "Maoists"
                | df_10$gname == "Kurdistan Workers' Party (PKK)" | df_10$gname == "Al-Qaida")


plot5.agg <- aggregate(df_10$eventid 
                       ,by = list(Group = df_10$gname
                                  , Region = df_10$region_txt
                                  , Type = df_10$attacktype1_txt)
                       , FUN = length
)

plot5.agg <- aggregate(df_10$eventid 
                       ,by = list(Group = df_10$gname
                                  , Region = df_10$region_txt
                                  , Type = df_10$targtype1_txt)
                       , FUN = length
)


alluvial(plot5.agg[,1:3], freq=plot5.agg$x
         , col = ifelse(plot5.agg$Group == "Taliban", "darkred"
                        ,ifelse(plot5.agg$Group == "Islamic State of Iraq and the Levant (ISIL)", "darkgreen"
                                ,ifelse(plot5.agg$Group == "Al-Shabaab", "cyan" 
                                        ,ifelse(plot5.agg$Group == "Boko Haram", "darkgoldenrod1"  
                                                ,ifelse(plot5.agg$Group == "Communist Party of India - Maoist (CPI-Maoist)", "azure"
                                                        ,ifelse(plot5.agg$Group == "New People's Army (NPA)", "pink"
                                                                ,ifelse(plot5.agg$Group == "Revolutionary Armed Forces of Colombia (FARC)", "orchid1" 
                                                                        ,ifelse(plot5.agg$Group == "Maoists", "lawngreen"
                                                                                ,ifelse(plot5.agg$Group == "Kurdistan Workers' Party (PKK)", "yellow" 
                                                                                        ,"chocolate1")))))))))
         ,border = ifelse(plot5.agg$Group == "Taliban", "darkred"
                          ,ifelse(plot5.agg$Group == "Islamic State of Iraq and the Levant (ISIL)", "darkgreen"
                                  ,ifelse(plot5.agg$Group == "Al-Shabaab", "cyan" 
                                          ,ifelse(plot5.agg$Group == "Boko Haram", "darkgoldenrod1"  
                                                  ,ifelse(plot5.agg$Group == "Communist Party of India - Maoist (CPI-Maoist)", "azure"
                                                          ,ifelse(plot5.agg$Group == "New People's Army (NPA)", "pink"
                                                                  ,ifelse(plot5.agg$Group == "Revolutionary Armed Forces of Colombia (FARC)", "orchid1" 
                                                                          ,ifelse(plot5.agg$Group == "Maoists", "lawngreen"
                                                                                  ,ifelse(plot5.agg$Group == "Kurdistan Workers' Party (PKK)", "yellow" 
                                                                                          ,"chocolate1")))))))))
         , cex = 0.7
)



