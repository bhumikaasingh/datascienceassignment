library(tidyverse)
library(dplyr)
library(stringr)



#broadband
broadband1 <- read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/broadband/fixedperformance.csv")
broadband2 <- read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/broadband/fixedpccoverage.csv")

#Population
population <- read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/population.csv")

#housepricing
housepricing2019 = read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/houseprice/housepricing2019.csv")
housepricing2020 = read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/houseprice/housepricing2020.csv")
housepricing2021 <- read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/houseprice/houseprice2021.csv")
housepricing2022 <- read.csv("C:/Users/saksh/OneDrive/Desktop/datascienceassignments/obtaineddatsets/houseprice/housepricing2022.csv")



#school
schoolkent20222021final <- read.csv("/Users/hp/OneDrive/Desktop/datascienceassignment/obtaineddatsets/schoolkent20212022/2021-2022/886_ks4final.csv")
schoolsurrey20222021final <- read.csv("/Users/hp/OneDrive/Desktop/datascienceassignment/obtaineddatsets/schoolsurrey20212022/2021-2022/936_ks4final.csv")


#towncode
townCode = read_csv("/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/0_postCodeToTown.csv")


#------------------------------------Extracting towncode------------------------

shortPostToTown = housePrice %>% 
  select(shortPostCode, Town, District, County) %>% 
  group_by(shortPostCode) %>% 
  distinct(shortPostCode, .keep_all = TRUE) %>% 
  arrange(shortPostCode)

write.csv(shortPostToTown,"C:/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/0_postCodeToTown.csv", row.names = FALSE)






#------------------------------------------------------Broadband cleaning---------------------------------------------------------------------------------

View(broadBandPerformance)
broadBandService = broadband1

#Separating  first part of post code.
broadBand = separate(broadBandService, postcode_space, into = c("shortPostCode", NA), sep = "\\s") 

names(broadBand)
#Selecting necessary columns.
broadBand = select(broadBand, c(shortPostCode, "Average.download.speed..Mbit.s.",
                                "Maximum.download.speed..Mbit.s.",
                                "Average.upload.speed..Mbit.s." 
                                , "Maximum.upload.speed..Mbit.s."))

#Selecting maximum and average speed of short post codes.
broadBand = broadBand %>%  
  group_by(shortPostCode) %>% 
  mutate("Avg download speed (Mbit/s)" = mean(Average.download.speed..Mbit.s.)) %>% 
  mutate("Max download speed (Mbit/s)" = mean(Maximum.download.speed..Mbit.s.)) %>% 
  mutate("Avg upload speed (Mbit/s)" = mean(Average.upload.speed..Mbit.s.)) %>% 
  mutate("Max upload speed (Mbit/s)" = mean(Maximum.upload.speed..Mbit.s.)) %>% 
  select(-c("Average.download.speed..Mbit.s.", "Maximum.download.speed..Mbit.s.",
            "Average.upload.speed..Mbit.s.", "Maximum.upload.speed..Mbit.s."))

#Removing duplicate rows.
broadBand = distinct(broadBand)
broadBand = na.omit(broadBand)

broadBand = inner_join(broadBand, townCode, by="shortPostCode")
names(broadBand)
#Store the final data frame of population.

write.csv(broadBand,"/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/cleanedbroadband.csv", row.names = FALSE)
















#-----------------------Population clean data----------------------------------------------------------------------------------------------------------------------




pop = separate(population, Postcode, into = c("shortPostCode", NA), sep = "\\s") 
View(pop)
pop
#Renaming second column to pop2011 to signify population of 2011.
colnames(pop)[2] = "pop2011"
names(pop)

#Simulating population.
# Assuming "pop" is your dataframe
populationFinal <- pop %>%
  mutate(pop2011Edit = as.numeric(gsub(",", "", pop$pop2011))) %>%
  mutate(pop2019 = pop2011Edit * 1.0069) %>%
  mutate(pop2020 = pop2019 * 1.0069) %>%
  mutate(pop2021 = pop2020 * 1.0069) %>%
  mutate(pop2022 = pop2021 * 1.0069)


view(populationFinal)
#Finding mean population of the multiple unique short post code.
populationFinal = populationFinal %>%  
  group_by(shortPostCode) %>% 
  mutate(population2011 = mean(pop2011)) %>% 
  mutate(population2019 = mean(pop2019)) %>% 
  mutate(population2020 = mean(pop2020)) %>% 
  mutate(population2021 = mean(pop2021)) %>%
  mutate(population2022 = mean(pop2022)) %>% 
  select(-c(pop2011, pop2018, pop2019, pop2020, pop2021))

#Removing duplicate rows.
populationFinal = distinct(populationFinal)

View(populationFinal)
populationFinal <- subset(populationFinal, select = -pop2011)

populationFinal <- populationFinal %>%
  rename(pop2011 = pop2011Edit)


townCode = read_csv("/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/0_postCodeToTown.csv")

populationFinal = inner_join(populationFinal, townCode, by="shortPostCode")
view(populationFinal)


#Store the final data frame of population.
write.csv(populationFinal,"/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/2_population.csv", row.names = FALSE)














#----------------------Data cleaning house price-----------------------------------------------------------------------------------------------------------------

colnames(housepricing2019) = c ("ID","Price","Year","Postcode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")
colnames(housepricing2020) = c ("ID","Price","Year","Postcode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")
colnames(housepricing2021) = c ("ID","Price","Year","Postcode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")
colnames(housepricing2022) = c ("ID","Price","Year","Postcode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")


houseprice = rbind(housepricing2019, housepricing2020, housepricing2021,housepricing2022)
colnames(houseprice) = c("houseId", "Price", "Date","Postcode", "PAON", "SAON", "FL", "HouseNum", "Flat", "Street", "Locality",
                         "Town", "District", "County", "Type1", "Type2")

names(houseprice)

#Select necessary columns only.
cleanedData = select(houseprice, c(Postcode, Price, Year, Town, District, County, Type1))
View(cleanedData)
head(cleanedData)

#filtering data of kent and surrey
filteredData = filter(cleanedData, County == "KENT" | County == "SURREY")
View(filteredData)
summary(filteredData)
head(filteredData$Year)
filteredData$Postcode2 = filteredData$Postcode

#Create a column with first half of postcode. 
housePrice = separate(filteredData, Postcode2, into = c("shortPostCode", NA), sep = "\\s") 
names(housePrice)
head(housePrice)

#Taking year from date and rearranging the columns.
housePrice = housePrice %>% 
  mutate(Year = as.numeric(str_extract(housePrice$Year, "\\d{4}"))) %>% 
  mutate(Id = 1:n()) %>% 
  select(Id, Postcode, shortPostCode, Price, Year, Town, County, District, Type1)

head(housePrice)
View(housePrice)

population = read.csv("C:/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/2_population.csv")

population = population %>% 
  select(-c(Town,County,District))
housePrice = inner_join(housePrice, population, by="shortPostCode")

head(housePrice)
#Store the final data frame of house price.
write.csv(housePrice,"C:/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/housepricing.csv", row.names = FALSE)










#--------------------------School data cleaning------------------------------------------------

schoolkent20222021final$postCode = schoolkent20222021final$PCODE 

#Separating  first part of post code.
schoolkent20222021 = separate(schoolkent20222021final, PCODE, into = c("shortPostCode", NA), sep = "\\s") 

names(schoolkent20222021)
#Selecting necessary columns.
schoolkent20222021 = schoolkent20222021 %>% 
  select(c(SCHNAME,postCode, shortPostCode, ATT8SCR)) %>% 
  
  filter(ATT8SCR != "NE" & ATT8SCR != "SUPP" ) %>% 
  mutate(att8score2021 = as.numeric(ATT8SCR)) %>% 
  na.omit() %>% 
  select(-ATT8SCR)
View(schoolkent20222021)

#-----surrey 2021-2022---------------

schoolsurrey20222021final$postCode = schoolsurrey20222021final$PCODE 

#Separating  first part of post code.
schoolsurrey20222021 = separate(schoolsurrey20222021final, PCODE, into = c("shortPostCode", NA), sep = "\\s") 

#Selecting necessary columns.
schoolsurrey20222021 = schoolsurrey20222021 %>% 
  select(c(SCHNAME,postCode, shortPostCode, ATT8SCR)) %>% 
  na.omit() %>% 
  filter(ATT8SCR != "NE" & ATT8SCR != "SUPP" ) %>% 
  mutate(att8score2021 = as.numeric(ATT8SCR)) %>% 
  select(-ATT8SCR)
View(schoolsurrey20222021)

#binding kent and surrey together
schoolData = rbind(schoolkent20222021,schoolsurrey20222021)
head(schoolData)
#joining towncode dataset
schoolData = inner_join(schoolData, townCode, by="shortPostCode")


view(schoolData)
write.csv(schoolData,"C:/Users/hp/OneDrive/Desktop/datascienceassignment/cleaneddatasets/cleanschooldata.csv", row.names = FALSE)
















#----------------------------------------Crime data Cleaning-------------------------------------------------------------



#Reading data from the CSV files.
datakent <- list.files(path = "/Users/hp/OneDrive/Desktop/datascienceassignment/obtaineddatsets/crimekent/",
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%         # Store all files in list.
  bind_rows                    # Combine data sets into one data set. 
View(dataLancashire)      


















crimedata = crimekent202101 %>%
  add_row(crimekent202102) %>% add_row(crimesurrey202102) %>% 
  add_row(crimekent202103) %>% add_row(crimesurrey202103) %>% 
  add_row(crimekent202104) %>% add_row(crimesurrey202104) %>%
  add_row(crimekent202105) %>% add_row(crimesurrey202105) %>% 
  add_row(crimekent202106) %>%  add_row(crimesurrey202106) %>% 
  add_row(crimekent202107) %>% add_row(crimesurrey202107) %>% 
  add_row(crimekent202108) %>%  add_row(crimesurrey202108) %>% 
  add_row(crimekent202109) %>%  add_row(crimesurrey202109) %>% 
  add_row(crimekent202110) %>%  add_row(crimesurrey202110) %>% 
  add_row(crimekent202111) %>% add_row(crimesurrey202111) %>% 
  add_row(crimekent202112) %>%  add_row(crimesurrey202112) %>% 
  mutate (Year = substring(Month,1,4)) %>% 
  rename(lsoa11cd = "LSOA",Crime.type="Crime type") %>% 
  select(lsoa11cd,Year,Crime.type)