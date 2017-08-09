library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Educational Need
# Created by Jenna Daly
# On 08/08/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

ell_data <- dir(path_to_raw, recursive=T, pattern = "ell") 
lunch_data <- dir(path_to_raw, recursive=T, pattern = "lunch") 
sped_data <- dir(path_to_raw, recursive=T, pattern = "edu") 

#Create 3 indicator DFs
ell_df <- data.frame(stringsAsFactors = F)
for (i in 1:length(ell_data)) {
  current_ell_file <- read.csv(paste0(path_to_raw, "/", ell_data[i]), stringsAsFactors=F, header=F )
  #remove first 4 rows
  current_ell_file <- current_ell_file[-c(1:4),]
  colnames(current_ell_file) = current_ell_file[1, ]
  current_ell_file = current_ell_file[-1, ] 
  current_ell_file <- current_ell_file[, !(names(current_ell_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(ell_data[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_ell_file$Year <- get_year
  current_ell_file$`Indicator of Educational Need` <- "English Language Learner"
  current_ell_file[current_ell_file == "*"] <- NA
  cols.num <- c("Yes", "No", "Total")
  current_ell_file[cols.num] <- sapply(current_ell_file[cols.num],as.numeric)
  ell_df <- rbind(ell_df, current_ell_file)
}

lunch_df <- data.frame(stringsAsFactors = F)
for (i in 1:length(lunch_data)) {
  current_lunch_file <- read.csv(paste0(path_to_raw, "/", lunch_data[i]), stringsAsFactors=F, header=F )
  #remove first 4 rows
  current_lunch_file <- current_lunch_file[-c(1:4),]
  colnames(current_lunch_file) = current_lunch_file[1, ]
  current_lunch_file = current_lunch_file[-1, ] 
  current_lunch_file <- current_lunch_file[, !(names(current_lunch_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(lunch_data[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_lunch_file$Year <- get_year
  current_lunch_file$`Indicator of Educational Need` <- "Eligible for Free or Reduced Price Lunch"
  current_lunch_file[current_lunch_file == "*"] <- NA
  #set columns to numeric
  cols.num <- c("Free", "Non-Subsidized", "Reduced", "Total")
  current_lunch_file[cols.num] <- sapply(current_lunch_file[cols.num],as.numeric)
  #assign No column to Non-Subsidized values
  current_lunch_file$No <- current_lunch_file$`Non-Subsidized`
  #assign Yes column to Free + Reduced values (only if both are not suppressed)
  current_lunch_file$Yes <- (current_lunch_file$Free + current_lunch_file$Reduced)
  #if both F and R are suppressed, calculate Yes by subtracting No from Total
  current_lunch_file <- current_lunch_file %>% mutate(Yes = ifelse(is.na(Yes), (Total - No), Yes))
  #remove extra columns
  null_cols <- c("Free", "Non-Subsidized", "Reduced")
  current_lunch_file[null_cols] <- NULL
  lunch_df <- rbind(lunch_df, current_lunch_file)
}

sped_df <- data.frame(stringsAsFactors = F)
for (i in 1:length(sped_data)) {
  current_sped_file <- read.csv(paste0(path_to_raw, "/", sped_data[i]), stringsAsFactors=F, header=F )
  #remove first 4 rows
  current_sped_file <- current_sped_file[-c(1:4),]
  colnames(current_sped_file) = current_sped_file[1, ]
  current_sped_file = current_sped_file[-1, ] 
  current_sped_file <- current_sped_file[, !(names(current_sped_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sped_data[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_sped_file$Year <- get_year
  current_sped_file$`Indicator of Educational Need` <- "Special Education"
  current_sped_file[current_sped_file == "*"] <- NA
  cols.num <- c("Yes", "No", "Total")
  current_sped_file[cols.num] <- sapply(current_sped_file[cols.num],as.numeric)
  sped_df <- rbind(sped_df, current_sped_file)
}

#Combine 3 indicators
edu_need <- rbind(ell_df, lunch_df, sped_df)

#Remove "Total" rows
edu_need <- edu_need[edu_need$District != "Total",]

#Calculate Percent
edu_need$Percent <- round((edu_need$Yes / edu_need$Total)*100, 1)

edu_need <- edu_need %>% 
  select(District, Year, `Indicator of Educational Need`, Yes, Percent, Total)

#Merge in FIPS, backfill districts, indicators, and years
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

#FIPS, districts
edu_need_fips <- merge(edu_need, districts, by = "District", all=T)
edu_need_fips$District <- NULL
edu_need_fips<-edu_need_fips[!duplicated(edu_need_fips), ]

#indicators, years
years <- c("2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016", 
           "2016-2017")

indicator <- c("English Language Learner", 
               "Eligible for Free or Reduced Price Lunch", 
               "Special Education")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years,
  `Indicator of Educational Need` = indicator
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)
backfill_years <- arrange(backfill_years, FixedDistrict)

edu_need_fips_backfill <- merge(edu_need_fips, backfill_years, all=T)

#remove duplicated Year rows
edu_need_fips_backfill <- edu_need_fips_backfill[!is.na(edu_need_fips_backfill$Year),]

#convert to long format
cols_to_stack <- c("Yes", "Percent", "Total")

long_row_count = nrow(edu_need_fips_backfill) * length(cols_to_stack)

edu_need_fips_backfill_long <- reshape(edu_need_fips_backfill, 
                         varying = cols_to_stack, 
                         v.names = "Value", 
                         timevar = "Measure Type", 
                         times = cols_to_stack, 
                         new.row.names = 1:long_row_count,
                         direction = "long"
)

edu_need_fips_backfill_long$id <- NULL


#Isolate Totals for "Total Students Evaluated" indicator
totals <- edu_need_fips_backfill_long[edu_need_fips_backfill_long$`Measure Type` == "Total",]
totals$`Indicator of Educational Need` <- "Total Students Evaluated"
totals <- unique(totals)

#Surgically remove a duplicate total
totals <- totals[!(totals$Year == "2011-2012" & totals$FixedDistrict == "Department of Mental Health and Addiction Services" & is.na(totals$Value)),]

#Strip out non-totals
no_totals <- edu_need_fips_backfill_long[edu_need_fips_backfill_long$`Measure Type` != "Total",]

#Combine again
complete_edu_need <- rbind(no_totals, totals)

#Assign Measure Type column
complete_edu_need$`Measure Type`[complete_edu_need$`Measure Type` == "Yes"] <- "Number"
complete_edu_need$`Measure Type`[complete_edu_need$`Measure Type` == "Total"] <- "Number"

#Assign Variable column
complete_edu_need$Variable <- "Indicator of Educational Need"

#Select, rename, and sort columns
complete_edu_need <- complete_edu_need %>% 
  select(FixedDistrict, FIPS, Year, Variable, `Indicator of Educational Need`, `Measure Type`, Value) %>% 
  rename(District = FixedDistrict) %>% 
  arrange(District, Year, `Indicator of Educational Need`, `Measure Type`)

#Remove CT
complete_edu_need <- complete_edu_need[complete_edu_need$District != "Connecticut",]

#Convert NAs in FIPS column to blanks
complete_edu_need$FIPS[is.na(complete_edu_need$FIPS)] <- ""

write.table(
  complete_edu_need,
  file.path(getwd(), "data", "educational_need_2016-2017.csv"),
  sep = ",",
  na = "-9999",
  row.names = F
)

