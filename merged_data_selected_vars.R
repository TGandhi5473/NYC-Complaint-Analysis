#Merged_data_selected_vars

library(dplyr)
library(readr)
library(lubridate)

#Last full year after pandemic imported
data <- read.csv("/Users/Robin/Desktop/Research proposal/311_Service_Request all 2023.csv")

# Display the structure of the data
str(data)

#Check data for Police.Precident and Police.Precincts 
data %>%
  filter(Police.Precinct != Police.Precincts) %>%
  select(Incident.Address, Incident.Zip, Street.Name, Cross.Street.1, Cross.Street.2, Intersection.Street.1, Intersection.Street.2, City, Landmark, Location, Police.Precinct, Police.Precincts)
#1955 values with different police precinct. Checking variables, both are consistently incorrect with actual precinct. 

#Columns to drop
columns_to_drop <- c("Facility.Type", "Due.Date", "BBL", "Vehicle.Type", "Taxi.Company.Borough", "Taxi.Pick.Up.Location", 
                     "Bridge.Highway.Name", "Bridge.Highway.Direction", "Road.Ramp", "Bridge.Highway.Segment", "Community.Districts",
                     "Borough.Boundaries", "Zip.Codes", "Police.Precincts", "Police.Precinct")
#Community.Districts, Borough.Boundaries dropped because variables are not described. 
#Zip.Codes dropped because of incorrect zip codes, other variable retained. 
#Police.Precincts and Police.Precinct both dropped because data consistently didn't match with real values. 

# Select columns not in the drop list
data <- data %>%
  select(-one_of(columns_to_drop))

# Check for blanks and replace with NA
data[data == ""] <- NA
data[data == "N/A"] <- NA
# Display the number of missing values in each column
check_blanks <- sapply(data, function(x) sum(is.na(x)))
print(check_blanks)

#We will probably not use all variables for analysis. However, zip code is necessary. All data without zip code deleted. Remove more NA values based on analysis. 
data <- data[!is.na(data$Incident.Zip), ]

#Calculate duration between Closed.data and Created.Date in minutes
data <- data %>%
  mutate(
    Created.Date = mdy_hms(Created.Date),
    Closed.Date = mdy_hms(Closed.Date),
    Duration = round(as.numeric(difftime(Closed.Date, Created.Date, units = "mins")))
  ) %>%
  relocate(Duration, .after = Closed.Date)

#Join total population
total_population_per_zip <- read.csv("/Users/Robin/Desktop/Research proposal/Cencus_total_population_2020.csv", sep = ";")
total_population_per_zip$Zip <- as.character(total_population_per_zip$Zip)
merged_data <- left_join(data, total_population_per_zip, by = c("Incident.Zip" = "Zip"))

#Join density
density_per_zip <- read.csv("/Users/Robin/Desktop/Research proposal/density_opendatasoft.csv")
density_per_zip$Zip <- as.character(density_per_zip$Zip)
merged_data <- left_join(merged_data, density_per_zip, by = c("Incident.Zip" = "Zip"))

#Omit rows with no density or total population
merged_data <- merged_data[complete.cases(merged_data[, c("Density_per_square_mile", "Total_population")]), ]

#Join gender/age 
gender <- read.csv('/Users/Robin/Desktop/Research proposal/Cencus_2022_gender_age.csv', sep = ";")
# Convert percentage columns to numeric
gender <- gender %>%
  mutate(
    Male_percentage = as.numeric(sub("%", "", Male_percentage)),
    Female_percentage = as.numeric(sub("%", "", Female_percentage)),
    Median_age_years = as.numeric(Median_age_years)
  )
gender$Zip <- as.character(gender$Zip)
merged_data <- left_join(merged_data, gender, by = c("Incident.Zip" = "Zip"))

#Join data education level 
edu_data <- read.csv("/Users/Robin/Desktop/Research proposal/CensusReporterEducation.csv")

#Set names
variable_names <- c(
  "geoid" = "geoid",
  "name" = "name",
  "B15003001" = "edu_Total",
  "B15003002" = "edu_No_schooling_completed",
  "B15003003" = "edu_Nursery_school",
  "B15003004" = "edu_Kindergarten",
  "B15003005" = "edu_1st_grade",
  "B15003006" = "edu_2nd_grade",
  "B15003007" = "edu_3rd_grade",
  "B15003008" = "edu_4th_grade",
  "B15003009" = "edu_5th_grade",
  "B15003010" = "edu_6th_grade",
  "B15003011" = "edu_7th_grade",
  "B15003012" = "edu_8th_grade",
  "B15003013" = "edu_9th_grade",
  "B15003014" = "edu_10th_grade",
  "B15003015" = "edu_11th_grade",
  "B15003016" = "edu_12th_grade_no_diploma",
  "B15003017" = "edu_Regular_high_school_diploma",
  "B15003018" = "edu_GED_or_alternative_credential",
  "B15003019" = "edu_Some_college_less_than_1_year",
  "B15003020" = "edu_Some_college_1_or_more_years_no_degree",
  "B15003021" = "edu_Associate_degree",
  "B15003022" = "edu_Bachelor_degree",
  "B15003023" = "edu_Master_degree",
  "B15003024" = "edu_Professional_school_degree",
  "B15003025" = "edu_Doctorate_degree",
  "B15003001..Error" = "edu_Total_error",
  "B15003002..Error" = "edu_No_schooling_completed_error",
  "B15003003..Error" = "edu_Nursery_school_error",
  "B15003004..Error" = "edu_Kindergarten_error",
  "B15003005..Error" = "edu_1st_grade_error",
  "B15003006..Error" = "edu_2nd_grade_error",
  "B15003007..Error" = "edu_3rd_grade_error",
  "B15003008..Error" = "edu_4th_grade_error",
  "B15003009..Error" = "edu_5th_grade_error",
  "B15003010..Error" = "edu_6th_grade_error",
  "B15003011..Error" = "edu_7th_grade_error",
  "B15003012..Error" = "edu_8th_grade_error",
  "B15003013..Error" = "edu_9th_grade_error",
  "B15003014..Error" = "edu_10th_grade_error",
  "B15003015..Error" = "edu_11th_grade_error",
  "B15003016..Error" = "edu_12th_grade_no_diploma_error",
  "B15003017..Error" = "edu_Regular_high_school_diploma_error",
  "B15003018..Error" = "edu_GED_or_alternative_credential_error",
  "B15003019..Error" = "edu_Some_college_less_than_1_year_error",
  "B15003020..Error" = "edu_Some_college_1_or_more_years_no_degree_error",
  "B15003021..Error" = "edu_Associate_degree_error",
  "B15003022..Error" = "edu_Bachelor_degree_error",
  "B15003023..Error" = "edu_Master_degree_error",
  "B15003024..Error" = "edu_Professional_school_degree_error",
  "B15003025..Error" = "edu_Doctorate_degree_error"
)
colnames(edu_data) <- variable_names[match(colnames(edu_data), names(variable_names))]


# Calculate ratios within edu_data
edu_data <- mutate(edu_data,
                   ratio_no_education = edu_No_schooling_completed / edu_Total,
                   ratio_at_least_lower_school = (edu_5th_grade +
                                                    edu_6th_grade) / edu_Total,
                   ratio_at_least_high_school = (edu_Regular_high_school_diploma +
                                                   edu_GED_or_alternative_credential) / edu_Total,
                   ratio_finished_higher_degree = (edu_Bachelor_degree +
                                                     edu_Master_degree +
                                                     edu_Professional_school_degree +
                                                     edu_Doctorate_degree) / edu_Total
)


merged_data <- left_join(merged_data, edu_data, by = c("Incident.Zip" = "name"))

#Omit rows with no education data
merged_data <- merged_data[complete.cases(merged_data[, c("ratio_no_education", "ratio_at_least_lower_school", "ratio_at_least_high_school", "ratio_finished_higher_degree")]), ]

#Drop geoid, zip code is used 
merged_data <- merged_data %>%
  select(-geoid)

#Join household income
income_data <- read.csv("/Users/Robin/Desktop/Research proposal/acs2022_5yr_B19001_86000US11414.csv")

#Set names
income_data <- rename(income_data,
                      inc_Total_Households = B19001001,
                      inc_Less_than_10K = B19001002,
                      inc_10K_to_14_999 = B19001003,
                      inc_15K_to_19_999 = B19001004,
                      inc_20K_to_24_999 = B19001005,
                      inc_25K_to_29_999 = B19001006,
                      inc_30K_to_34_999 = B19001007,
                      inc_35K_to_39_999 = B19001008,
                      inc_40K_to_44_999 = B19001009,
                      inc_45K_to_49_999 = B19001010,
                      inc_50K_to_59_999 = B19001011,
                      inc_60K_to_74_999 = B19001012,
                      inc_75K_to_99_999 = B19001013,
                      inc_100K_to_124_999 = B19001014,
                      inc_125K_to_149_999 = B19001015,
                      inc_150K_to_199_999 = B19001016,
                      inc_200K_or_more = B19001017,
                      inc_Total_Households_error_error = B19001001..Error,
                      inc_Less_than_10K_error = B19001002..Error,
                      inc_10K_to_14_999_error = B19001003..Error,
                      inc_15K_to_19_999_error = B19001004..Error,
                      inc_20K_to_24_999_error = B19001005..Error,
                      inc_25K_to_29_999_error = B19001006..Error,
                      inc_30K_to_34_999_error = B19001007..Error,
                      inc_35K_to_39_999_error = B19001008..Error,
                      inc_40K_to_44_999_error = B19001009..Error,
                      inc_45K_to_49_999_error = B19001010..Error,
                      inc_50K_to_59_999_error = B19001011..Error,
                      inc_60K_to_74_999_error = B19001012..Error,
                      inc_75K_to_99_999_error = B19001013..Error,
                      inc_100K_to_124_999_error = B19001014..Error,
                      inc_125K_to_149_999_error = B19001015..Error,
                      inc_150K_to_199_999_error = B19001016..Error,
                      inc_200K_or_more_error = B19001017..Error
)

# Calculate ratios within income data and calculate total average income 
income_data <- mutate(income_data,
                      ratio_less_than_50K = (inc_Less_than_10K + inc_10K_to_14_999 + 
                                               inc_15K_to_19_999 + inc_20K_to_24_999 + 
                                               inc_25K_to_29_999 + inc_30K_to_34_999 + 
                                               inc_35K_to_39_999 + inc_40K_to_44_999)/ inc_Total_Households,
                     ratio_50K_to_99_999 = (inc_45K_to_49_999 + inc_50K_to_59_999 + 
                                                  inc_60K_to_74_999 + inc_75K_to_99_999) / inc_Total_Households,
                      ratio_100K_to_149_999 = (inc_100K_to_124_999 + inc_125K_to_149_999 + 
                                                 inc_150K_to_199_999) / inc_Total_Households, 
                      ratio_200K_or_more = inc_200K_or_more/ inc_Total_Households
)

merged_data <- left_join(merged_data, income_data, by = c("Incident.Zip" = "name"))

#Merge with median income in last 12 months 2022 
median_income_data <- read.csv("/Users/Robin/Desktop/Research proposal/median_household_income_2022.csv", sep = ";")
median_income_data$Zip <- as.character(median_income_data$Zip)
merged_data <- left_join(merged_data, median_income_data, by = c("Incident.Zip" = "Zip"))

#Omit rows with no income data
merged_data <- merged_data[complete.cases(merged_data[, c("ratio_less_than_50K", "ratio_50K_to_99_999", "ratio_100K_to_149_999", "ratio_200K_or_more", "Median_income")]), ]

#Drop geoid, zip code is used 
merged_data <- merged_data %>%
  select(-geoid)

#Join weather 
weather <- read.csv("/Users/Robin/Desktop/Research proposal/weather.csv", sep = ";")
head(merged_data$Created.Date)

# Extract the date part from Created.Date
merged_data$Date <- as.Date(as.POSIXct(merged_data$Created.Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
merged_data$Date <- as.character(merged_data$Date)
merged_data <- left_join(merged_data, weather, by = c("Date" = "date"))
merged_data <- merged_data %>% select(-Date)

#Change classes 
merged_data <- merged_data %>%
  mutate(
    Agency.Name = as.factor(Agency.Name),
    Complaint.Type = as.factor(Complaint.Type),
    Open.Data.Channel.Type = as.factor(Open.Data.Channel.Type),
    Status = as.factor(Status),
    Borough = as.factor(Borough),
    Community.Board = as.factor(Community.Board),
    Location.Type = as.factor(Location.Type)
  ) 

#Columns to drop for selected variables only, not used in further analysis
columns_to_drop_for_selection <- c("Agency", "Incident.Address", "Street.Name", "Cross.Street.1", "Cross.Street.2", "City", "Intersection.Street.1","Intersection.Street.2",
                                   "Address.Type", "Landmark", "Resolution.Action.Updated.Date", "X.Coordinate..State.Plane.", "Y.Coordinate..State.Plane.", "Park.Facility.Name", "Park.Borough", 
                                   "edu_Total","edu_Total_error","edu_No_schooling_completed","edu_No_schooling_completed_error","edu_Nursery_school","edu_Nursery_school_error","edu_Kindergarten","edu_Kindergarten_error","edu_1st_grade","edu_1st_grade_error","edu_2nd_grade","edu_2nd_grade_error","edu_3rd_grade","edu_3rd_grade_error","edu_4th_grade","edu_4th_grade_error","edu_5th_grade","edu_5th_grade_error","edu_6th_grade","edu_6th_grade_error","edu_7th_grade","edu_7th_grade_error","edu_8th_grade","edu_8th_grade_error","edu_9th_grade","edu_9th_grade_error","edu_10th_grade","edu_10th_grade_error","edu_11th_grade","edu_11th_grade_error","edu_12th_grade_no_diploma","edu_12th_grade_no_diploma_error","edu_Regular_high_school_diploma","edu_Regular_high_school_diploma_error","edu_GED_or_alternative_credential","edu_GED_or_alternative_credential_error","edu_Some_college_less_than_1_year","edu_Some_college_less_than_1_year_error","edu_Some_college_1_or_more_years_no_degree","edu_Some_college_1_or_more_years_no_degree_error","edu_Associate_degree","edu_Associate_degree_error","edu_Bachelor_degree","edu_Bachelor_degree_error","edu_Master_degree","edu_Master_degree_error","edu_Professional_school_degree","edu_Professional_school_degree_error","edu_Doctorate_degree","edu_Doctorate_degree_error",
                                   "inc_Total_Households","inc_Total_Households_error_error","inc_Less_than_10K","inc_Less_than_10K_error","inc_10K_to_14_999","inc_10K_to_14_999_error","inc_15K_to_19_999","inc_15K_to_19_999_error","inc_20K_to_24_999","inc_20K_to_24_999_error","inc_25K_to_29_999","inc_25K_to_29_999_error","inc_30K_to_34_999","inc_30K_to_34_999_error","inc_35K_to_39_999","inc_35K_to_39_999_error","inc_40K_to_44_999","inc_40K_to_44_999_error","inc_45K_to_49_999","inc_45K_to_49_999_error","inc_50K_to_59_999","inc_50K_to_59_999_error","inc_60K_to_74_999","inc_60K_to_74_999_error","inc_75K_to_99_999","inc_75K_to_99_999_error","inc_100K_to_124_999","inc_100K_to_124_999_error","inc_125K_to_149_999","inc_125K_to_149_999_error","inc_150K_to_199_999","inc_150K_to_199_999_error","inc_200K_or_more","inc_200K_or_more_error")
                                  
# Select columns not in the drop list
merged_data_selected_vars <- merged_data %>%
  select(-one_of(columns_to_drop_for_selection))

#Write to a csv file for merged and cleaned data
write_csv(merged_data_selected_vars, "/Users/Robin/Desktop/Research proposal/311_analysis_data.csv")