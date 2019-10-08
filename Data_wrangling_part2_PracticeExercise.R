# EEOB590A
# Data_wrangling part 2 practice exercise
# practice tidying and wrangling 

#from the tidy folder, read in the partialtidy file for pollination from last week's assignment
part <- read_csv("data/tidy/poll_long_partialtidy.csv")
###########################################################
#####Part 1: finish tidying & wrangling dataframe #########

#1) Broad changes to the database

#1a) Change the class of each variable as appropriate (i.e. make things into factors or numeric)
part$numinsects <- as.numeric(part$numinsects)
part$uniqueID<- as.factor(part$uniqueID)
part$island<- as.factor(part$island)
part$site<- as.factor(part$site)
part$transect<- as.factor(part$transect)
part$topcolor<- as.factor(part$topcolor)
part$bowlcolor<- as.factor(part$bowlcolor)
part$insectorder<- as.factor(part$insectorder)
#or
part <- part %>%
  mutate_at(vars(numinsects,uniqueID,island,site,transect,topcolor,bowlcolor,insectorder), 
            factor)
#2) Fix the errors below within cells 

##2a) Fix the levels of site so that they have consistent names, all in lowercase
levels(part$site)
part$site <- tolower(part$site)
levels(part$site)

part <- part %>%
  mutate_at(vars(site), factor)
levels(part$site)
##2b) What format are the dates in? Do they look okay? 
str(part)
levels(part$`date traps coll`)
part$`date traps coll`<- dmy(part$`date traps coll`)
part$`date traps out` <- dmy(part$`date traps out`)

#Warning message:
#All formats failed to parse. No formats found. 
#? can I undo? It was the same format as "betterdates" from the website
str(part)


##2c) Do you see any other errors that should be cleaned up? 

#3) Create a new column for the duration of time traps were out
part$duration <- part$`date traps coll` - part$`date traps out`
part$duration <- as.numeric(part$duration)
summary(part$duration)
#4) Arrange data by the number of insects
summary(part$numinsects)
#5) Print tidied, wrangled database
write.csv(part, "data/tidy/part_tidy.csv", row.names = F)
#Error in is.data.frame(x) : object 'part_summ' not found


#####################################################
####Part 3: start subsetting & summarizing ##########

#6) Make a new dataframe with just the data from Guam at the racetrack site and name accordingly. 
part_guam <- part %>%
  filter(island == "guam", site == "racetrack")
#7) Make a new dataframe with just the uniqueID, island, site, transect, insectorder, numinsects, and duration columns. 
basic_part <- part %>%
  select(uniqueID, island, site, transect, insectorder, numinsects, duration)

#8) With the full database (not the new ones you created in the two previous steps), summarize data, to get: 
#8a) a table with the total number of insects at each site
part$numinsects <- as.numeric(part$numinsects)
part %>%
  group_by(site) %>%
  summarize (total = sum(numinsects))
#8b) a table that shows the mean number of insects per island
part %>%
  group_by(island) %>%
  summarize (avg = mean(numinsects))
#why is it "NA" for Saipan?

#8c) a table that shows the min and max number of insects per transect
part %>%
  group_by(transect) %>%
  summarize (min = min(numinsects), max = max(numinsects))
#9a) Figure out which insect order is found across the greatest number of sites
order <- part %>%
  group_by(insectorder) %>%
  count(site)
order2 <- count(order,insectorder)
arrange(order2,n)

order2 %>%
  arrange(desc(n))
#can't figure this out

#9b) For that insect order, calculate the mean and sd by site. 
