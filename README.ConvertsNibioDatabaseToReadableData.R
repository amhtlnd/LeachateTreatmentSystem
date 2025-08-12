###SET-UP###

#Clean surfaces:
rm(list = ls())

#Change text to universal (UTF-8):
Sys.setlocale("LC_ALL", "en_US.UTF-8")
Sys.getlocale()


#Install libraries
#install.packages("tidyverse")
#install.packages("readr")
#install.packages("openxlsx")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("writexl")
#install.packages("lubridate")
#install.packages("tibble")
#install.packages("lmer4")
#install.packages("jsonlite")
#install.packages("tidyr")
#install.packages("factoextra")
#install.packages("cluster")
#install.packages("dtw")
#install.packages("ggplot2")
#install.packages("rrcov")
#install.packages("FactoMineR")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("thoree/BIAS.data")
#install.packages("stats")
#install.packages("httr")
#install.packages("Hmisc")
#install.packages("pwr")
#install.packages("dagitty")
#install.packages("ggdag")
#install.packages("broom.mixed")
#install.packages("lmerTest")
#install.packages("effects")
#install.packages("interactions")
#install.packages("officer")
#install.packages("ggplot")
#install.packages("MuMIn")


#Open libraries
library(tidyverse)
library(readr)
library(openxlsx)
library(readxl)
library(dplyr)
library(writexl)
library(lubridate)
library(tibble)
library(lme4)
library(jsonlite)
library(tidyr)
library(factoextra)
library(cluster)
library(dtw)
library(ggplot2)
library(rrcov)
#library(FactoMineR)
#library(BIAS.data)
library(httr)
library(nlme)
library(lme4)
library(Hmisc)
library(broom)
library(purrr)
library(pwr)
library(dagitty)
library(ggdag)
library(broom.mixed)
library(lmerTest)
library(effects)
library(interactions)
library(officer)
library(MuMIn)
library(lmerTest)
library(bestNormalize)


### OPEN DATA ###

#Path to project:
path <- ""

#Set-up pathways:
data_raw <- paste0(path, "data/data_raw/")
data_out <- paste0(path, "data/data_out/")
data_temp <- paste0(path, "data/data_temp/")
data_lookup <- paste0(path, "data/data_lookup/")
data_clean <- paste0(path, "data/data_clean/")
templates <- "templates/"

#Change name of original datafile of rawdata and store it with new name:

input_file_analysis <- "data_NIBIO_raw.xlsx"
input_file_temperature <- "WaterTemperatureNibio.xlsx"

data_analysis_raw <- read_excel(paste0(data_raw, input_file_analysis), na ="")
data_temperature_raw_Spillhaug <- read_excel(paste0(data_raw, input_file_temperature), sheet = "Spillhaug", na ="")
data_temperature_raw_Boelstad <- read_excel(paste0(data_raw, input_file_temperature), sheet = "Boelstad", na ="")


###IMPORT DATA###

#input_file_temperature <- "data_temperature_raw.xlsx"

look_up_table<- "LookUpTable.xlsx"


#Set-up pathways and create various dataframes for further data implementation (lookup-tables):
#ra_environ_con_import_template_raw <- read_excel(paste0(templates,"IMPORT_EXAMPLE_TEST_V4.xlsx"), sheet = "RA_ENVIRONMENTAL_CON_EXT_V13", na ="")
#ra_sites_import_template_raw <-  read_excel(paste0(templates, "IMPORT_EXAMPLE_TEST_V4.xlsx"), sheet = "RA_SITES_EXT", na ="")

lookup_landfill <- read_excel(paste0(data_lookup, look_up_table), sheet = "Landfills", na ="")
lookup_samplesites <- read_excel(paste0(data_lookup, look_up_table), sheet = "SampleSites", na ="")
lookup_chemicals <- read_excel(paste0(data_lookup, look_up_table), sheet = "Chemicals", na ="")

Input_file_analysis_df<- read_excel(paste0(data_raw, input_file_analysis)) #Read Excel-sheet and return a df.

data_analysis_raw <- Input_file_analysis_df$Verdi
data_location_raw <- Input_file_analysis_df$Deponinavn
data_sampling_raw <- Input_file_analysis_df$Prøvepunkt
data_parameter_raw <- Input_file_analysis_df$Parameter
data_unit_raw <- Input_file_analysis_df$Enhet

#----------------------------------------------------------------------------
  
### WRANGLED DATA ###


#Change names of columns in lookup_samplesites:
colnames(lookup_samplesites)[1] <- "LOCATION"
colnames(lookup_samplesites)[2] <- "LOCAL_ID"

#--------------------------------------------------------------------------------

#Change names in df and extract "Boelstad" and "Spillhaug".
#Input_file_analysis_df$Deponinavn <- gsub("Bølstad", "Boelstad", Input_file_analysis_df$Deponinavn)

colnames(Input_file_analysis_df)[colnames(Input_file_analysis_df) == "Deponinavn"] <- "LANDFILL"
colnames(Input_file_analysis_df)[colnames(Input_file_analysis_df) == "Prøvepunkt"] <- "SAMPLE_SITES"
colnames(Input_file_analysis_df)[colnames(Input_file_analysis_df) == "Dato"] <- "DATE"
colnames(Input_file_analysis_df)[colnames(Input_file_analysis_df) == "Parameter"] <- "STRESSOR"
colnames(Input_file_analysis_df)[colnames(Input_file_analysis_df) == "Verdi"] <- "VALUE"
colnames(Input_file_analysis_df)[colnames(Input_file_analysis_df) == "Enhet"] <- "UNIT"

Ex2_two_landfills <-  Input_file_analysis_df %>% filter(LANDFILL %in% c("Boelstad", "Spillhaug"))

#Substitute "," with "." in the column "Verdi" in df:
Ex2_two_landfills$Verdi <- gsub("," , "." , Ex2_two_landfills$VALUE)
Ex2_two_landfills$Kontrollverdi <- NA

#create a column to keep track of LOQ-values.
Ex2_two_landfills$LOQ <- ifelse(grepl("^<", Ex2_two_landfills$VALUE), 1, 0)

#LOQ/2 in the column VALUE.
Ex2_two_landfills$Verdi <- gsub("<", "", Ex2_two_landfills$Verdi)

Ex2_two_landfills$Kontrollverdi <- ifelse(Ex2_two_landfills$LOQ == "1", as.numeric(Ex2_two_landfills$Verdi)/2, Ex2_two_landfills$Verdi) #NAs introduced by coercion.


#Create a control column to check if the column "Enhet" contains NA that is not in either % og pH.
Ex2_two_landfills$control <- ifelse (grepl("NA", Ex2_two_landfills$Kontrollverdi) & (!grepl("pH", Ex2_two_landfills$STRESSOR) | (!grepl("%", Ex2_two_landfills$UNIT))), "sjekk", "OK")

#Checking if the column "sjekk" contains any "sjekk" (not pH or %):
Not_OK_index <- which((Ex2_two_landfills$control == "sjekk")) #Empty. Hence, OK.

#Since Not_OK_index is empty, we may implement pH into the column "UNIT":
Ex2_two_landfills$UNIT <- ifelse(grepl("pH", Ex2_two_landfills$STRESSOR), "pH", Ex2_two_landfills$UNIT)

#Remove all values connected to EC10, EC20, EC50 etc.:
Ex2_two_landfills <- Ex2_two_landfills %>% 
  filter(!STRESSOR %in% c("EC10", "EC20", "EC50"))

#Paste edited values.
Ex2_two_landfills$VALUE <- Ex2_two_landfills$Kontrollverdi

#Remove redundant columns.
Ex2_two_landfills <- Ex2_two_landfills[,c(1:6,9)]

#Extract temporary dataset from "lookup-chemicals" and change column names:
temp_lookup <- lookup_chemicals[,c("parameter_no", "parameter_eng", "CAS nr. 1 (English)", "CAS nr. 2/comments to CAS nr. 1", "std_inchikey")]

colnames(temp_lookup) <- c("parameter_no", "parameter_eng", "CAS1", "CAS2", "std_inchikey")

#Sorting df Ex2 by alphabetical order for the column "Parameter":
Ex2_two_landfills <- Ex2_two_landfills[order(Ex2_two_landfills$STRESSOR),]

#Since each value of parameter_no in df "lookup_chemicals" only occurs once, we may use match()-function to combine df Ex2 and temp_lookup. This match is used by index-creation:

Ex2_two_landfills$test <- NA #Create dummy column for testing.
index <- match(Ex2_two_landfills$STRESSOR, temp_lookup$parameter_no)
Ex2_two_landfills$test[!is.na(index)] <- temp_lookup$parameter_eng[index[!is.na(index)]]

#The (already existing NA-values in the dataset) in Ex2$test are located (by searching in R). These are corrected manually:
Ex2_two_landfills$test <- ifelse(Ex2_two_landfills$STRESSOR == "2,3,5-Trimetylfenol", "2,3,5-trimethylphenol", Ex2_two_landfills$test) 
Ex2_two_landfills$test <- ifelse(Ex2_two_landfills$STRESSOR == "2,4,6-Trimetylfenol", "2,4,6-trimethylphenol", Ex2_two_landfills$test) 
Ex2_two_landfills$test <- ifelse(Ex2_two_landfills$STRESSOR == "2,6-Dimetylfenol", "2,6-dimethylphenol", Ex2_two_landfills$test)
Ex2_two_landfills$test <- ifelse(Ex2_two_landfills$STRESSOR == "3,4-Dimetylfenol", "3,4-dimethylphenol", Ex2_two_landfills$test)
Ex2_two_landfills$test <- ifelse(Ex2_two_landfills$STRESSOR == "3,5-Dimetylfenol", "3,5-dimethylphenol", Ex2_two_landfills$test)

#Change column name from "test" to "parameter_eng":
names(Ex2_two_landfills)[8] <- paste("parameter_eng")

#Add the remaining columns from lookup_chemical, to start setup for export to RADb:
index <- match(Ex2_two_landfills$parameter_eng, temp_lookup$parameter_eng)

Ex2_two_landfills$CAS1[!is.na(index)] <- temp_lookup$CAS1[index[!is.na(index)]]

Ex2_two_landfills$CAS2[!is.na(index)] <- temp_lookup$CAS2[index[!is.na(index)]]

  
#Format to ra_environ_con_import using SCR 1.155 in ConvertR.R.

Ex3_temp_two_landfills <- Ex2_two_landfills #Multiply df (to avoid mistakes).

Ex3_temp_two_landfills$COMMENTS <- NA

#Importing values from lookup_chemicals:
index1 <- match(Ex3_temp_two_landfills$parameter_eng, lookup_chemicals$parameter_eng)

Ex3_temp_two_landfills$COMMENTS [!is.na(index1)] <- lookup_chemicals$Comment[index1[!is.na(index1)]]

#Change order of columns.
Ex3_temp_two_landfills <- Ex3_temp_two_landfills[,c(1:6,8:11,7)]

#Change names of columns.
colnames(Ex3_temp_two_landfills) <- c("LOCATION", "SAMPLE_SITE", "SAMPLE_DATE", "PARAMETER_NO", "MEASURED_VALUE", "MEASURED_UNIT", "PARAMETER_ENG", "CAS1", "CAS2", "COMMENTS", "LOQ")

#Create index and compare LOCAL_ID, to paste SAMPLE_ID into df:
Ex3_temp_two_landfills$SITE_CODE <- NA

#Change name of variable to "Boelstad".
lookup_samplesites$LOCATION[lookup_samplesites$LOCATION == "Bølstad"] <- "Boelstad"

#Extract rows with Boelstad and Spillhaug from lookup_samplesites.
chosen_rows_lookup <- subset(lookup_samplesites, LOCATION %in% c("Boelstad", "Spillhaug"))

#Change names of columns:
colnames(chosen_rows_lookup)[2] <- "SAMPLE_SITE"
colnames(chosen_rows_lookup)[3] <- "SITE_CODE"

#Create an index variable that matches two columns from main dataset and lookup_samplesites:
index2 <- match(paste(Ex3_temp_two_landfills$SAMPLE_SITE, Ex3_temp_two_landfills$LOCATION), paste(chosen_rows_lookup$SAMPLE_SITE, chosen_rows_lookup$LOCATION))

#Paste columns from lookup_samplesites into Ex3:
Ex3_temp_two_landfills$SITE_CODE <- ifelse(!is.na(index2), chosen_rows_lookup$SITE_CODE[index2], NA)

colnames(Ex3_temp_two_landfills)[2] <- "SAMPLE_SITE"
colnames(Ex3_temp_two_landfills)[12] <- "SITE_CODE"

#Change order of columns:
Ex3_temp_two_landfills <- Ex3_temp_two_landfills[,c(1,12,2:11)]

#---------------------------------------------------------------------------------

#Import temperature data:
input_file_temperature <- "WaterTemperatureNibio.xlsx"

data_temperature_raw_Spillhaug <- read_excel(paste0(data_raw, input_file_temperature), sheet = "Spillhaug", na ="")
data_temperature_raw_Boelstad <- read_excel(paste0(data_raw, input_file_temperature), sheet = "Boelstad", na ="")

#Change column names:
colnames(data_temperature_raw_Boelstad) <- c("DATE", "TEMPERATURE")
colnames(data_temperature_raw_Spillhaug) <- c("DATE", "TEMPERATURE")

#Remove two upper columns to wrangle data (columns contains text, not data):
data_temp_Sp_without_toprows <- data_temperature_raw_Spillhaug[-c(1,2),]
data_temp_Bo_without_toprows <- data_temperature_raw_Boelstad[-c(1,2),]

#NB! Excel counts year 1900 as a leap year (which it isn't. This is an known error. This error is accounted for by implementing:
excel_date_to_r <- function(excel_date) {
origin <- ifelse(excel_date <= 60, "1899-12-31", "1899-12-30")
return(as.Date(origin) + excel_date)
}

data_temp_Bo_without_toprows$DATE <- excel_date_to_r(as.numeric(data_temp_Bo_without_toprows$DATE))

#As Date. Here, rawdata Spillhaug is not defined as a date (but as "standard" in Excel rawdata), and hence is not affected by the 1900-error. The df is already in the format dd.mm.yyyy in character. To convert to Y-m-d (to make it applicable in R), we need to first convert to date in existing format (done by stating format = %d.%m.%y). Then R converts automatically to Y-m-d by using "as.Date"-function.
data_temp_Sp_without_toprows$DATE <- as.Date(data_temp_Sp_without_toprows$DATE, format="%d.%m.%Y")

#Import temperature into main dataset. Create new name of dataset:

#Add column to dataset for location:
data_temp_Bo_without_toprows$LOCATION <- "Boelstad"

data_temp_Sp_without_toprows$LOCATION <- "Spillhaug"

#Create index to correlate temperature with main dataset:
index3_paste_temperature_Boelstad <- match(paste(Ex3_temp_two_landfills$LOCATION, Ex3_temp_two_landfills$SAMPLE_DATE), paste(data_temp_Bo_without_toprows$LOCATION, data_temp_Bo_without_toprows$DATE))

index4_paste_temperature_Spillhaug <- match(paste(Ex3_temp_two_landfills$LOCATION, Ex3_temp_two_landfills$SAMPLE_DATE), paste(data_temp_Sp_without_toprows$LOCATION, data_temp_Sp_without_toprows$DATE))

#Correlate temperature with main dataset for both Boelstad and Spillhaug:
Ex3_temp_two_landfills$TEMPERATURE <- ifelse(!is.na(index3_paste_temperature_Boelstad), data_temp_Bo_without_toprows$TEMPERATURE[index3_paste_temperature_Boelstad], NA)

Ex3_temp_two_landfills$TEMPERATURE_SPILLHAUG <- ifelse(!is.na(index4_paste_temperature_Spillhaug), data_temp_Sp_without_toprows$TEMPERATURE[index4_paste_temperature_Spillhaug], NA)

#Combine the columns with temperature of Spillhaug and Boelstad:

Ex3_temp_two_landfills$TEMP_COMBINED <- ifelse(is.na(Ex3_temp_two_landfills$TEMPERATURE), Ex3_temp_two_landfills$TEMPERATURE_SPILLHAUG, Ex3_temp_two_landfills$TEMPERATURE)

#Remove excess columns and clean df of unnecessary data. Store df with new name.
Ex5_Spillhaug_Boelstad_combined_temperature <- Ex3_temp_two_landfills %>% select(-TEMPERATURE, -TEMPERATURE_SPILLHAUG)

#Remove "NA"-values:
Ex5_Spillhaug_Boelstad_combined_temperature <- Ex5_Spillhaug_Boelstad_combined_temperature[!is.na(Ex5_Spillhaug_Boelstad_combined_temperature$TEMP_COMBINED), ]
Ex5_Spillhaug_Boelstad_combined_temperature <- Ex5_Spillhaug_Boelstad_combined_temperature[!is.na(Ex5_Spillhaug_Boelstad_combined_temperature$PARAMETER_ENG), ]
Ex5_Spillhaug_Boelstad_combined_temperature <- Ex5_Spillhaug_Boelstad_combined_temperature[!is.na(Ex5_Spillhaug_Boelstad_combined_temperature$MEASURED_VALUE), ]


#Extract relevant sample sites. (Remove e.g.groundwater wells etc.) 
Ex6_relevant_site_codes_Boelstad_and_Spillhaug <- subset(Ex5_Spillhaug_Boelstad_combined_temperature, SITE_CODE %in% c("Bo_Sig1", "Bo_UtRA2", "Bo_Sed3", "Sp_K1", "Sp_K2", "Sp_Llut", "Sp_Sed", "Sp_VM3"))

#Retreive precipitation data using Frost (www.yr.no)

#Personal client_id.
client_id = '0d436346-6476-4cca-b26f-c0aa5453049a'
  
endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")
sources <- 'SN2650, SN2610, SN17741, SN17870' #Locations for precipitation sites closest to Boelstad (SN17870, for the whole period) and Spillhaug (SN2650 until 01/12/2007/SN2610 from 01/12/2007).
elements <- 'sum(precipitation_amount P1D)'
referenceTime <- '1995-01-01/2024-01-01'
url <- paste0(
endpoint, "?",
"sources=", sources,
"&referencetime=", referenceTime,
"&elements=", elements
)

# Issue an HTTP GET request and extract JSON-data
xs <- try(fromJSON(URLencode(url),flatten=T))

#Check if the request worked, print out any errors.
if (class(xs) != 'try-error') 
{
df <- unnest(xs$data, cols = c(observations))
print("Data retrieved from frost.met.no!")
} else {
print("Error: the data retrieval was not successful!")
}

# These additional columns will be kept
columns <- c("sourceId","referenceTime","elementId","value","unit","timeOffset")
df2 <- df[columns]

# Convert the time value to something R understands
df2$referenceTime <- as.Date(df2$referenceTime)
df2$sourceId <- gsub(":0", "", df2$sourceId)

#Change column names of df df2 to match with df (see below).
names(df2)[names(df2) == "sourceId"] <- "STATION"
names(df2)[names(df2) == "referenceTime"] <- "SAMPLE_DATE"


#Remove potentially duplicated values from dataset. In case of duplication of SAMPLE_DATE and STATION, keep lowest offset (6h). To keep close to CET (Central European Time).
df2 <- df2 %>%
  group_by(STATION, SAMPLE_DATE) %>%
  filter(!(timeOffset == "PT18H" & n() > 1)) %>%
  ungroup()


#rm(df)

#Create column with station ID in main df, to import temperature:
test <- subset(Ex5_Spillhaug_Boelstad_combined_temperature, SITE_CODE %in% c("Bo_Sig1", "Bo_UtRA2", "Bo_Sed3", "Sp_K1", "Sp_K2", "Sp_Llut", "Sp_Sed", "Sp_VM3"))

test$SAMPLE_DATE <- as.Date(test$SAMPLE_DATE)

#Implement STATION ID, based on location and sample_date. To prepare for precipitation implementation. Dates outside this interval are excluded due to lack of precipitation measurements:
test$STATION <- ifelse(test$LOCATION == 'Boelstad' & test$SAMPLE_DATE >= as.Date('1995-01-01') & test$SAMPLE_DATE <= as.Date('2009-03-13'), "SN17741",
                       ifelse(test$LOCATION == 'Boelstad' & test$SAMPLE_DATE > as.Date('2009-03-14'), "SN17870",
                              ifelse(test$LOCATION == 'Spillhaug' & test$SAMPLE_DATE >= as.Date('1995-01-01') & test$SAMPLE_DATE <= as.Date('2007-12-01'), "SN2610",
                                     ifelse(test$LOCATION == 'Spillhaug' & test$SAMPLE_DATE > as.Date('2007-12-01'), "SN2650", NA))))



#Remove NA-values prior to merging df.
test <- test[,c(-9,-10)] #Removing CAS-column do avoid losing data when removing NA-values.

#Import precipitation.
merged_data <- left_join(test, df2[, c("STATION", "SAMPLE_DATE", "value")], 
                         by = c("STATION", "SAMPLE_DATE"))

#Change name of column.
colnames(merged_data)[13] <- "PRECIPITATION"

#Inspect NA-values of precipitation column.
na_rows <- merged_data[is.na(merged_data$PRECIPITATION), ]

#Due to lack of measurements or downtime at measurement stations at some dates, precipitation values are gathered manually from the stations SN17870, SN17850 for Boelstad and SN4920 for Spillhaug here.
merged_data$PRECIPITATION <- ifelse(merged_data$LOCATION == "Boelstad" & merged_data$SAMPLE_DATE == as.Date("2013-07-03"), 0.7, merged_data$PRECIPITATION)
merged_data$PRECIPITATION <- ifelse(merged_data$LOCATION == "Boelstad" & merged_data$SAMPLE_DATE == as.Date("2011-04-15"), 0, merged_data$PRECIPITATION)
merged_data$PRECIPITATION <- ifelse(merged_data$LOCATION == "Boelstad" & merged_data$SAMPLE_DATE == as.Date("2013-10-29"), 0.7, merged_data$PRECIPITATION)
merged_data$PRECIPITATION <- ifelse(merged_data$LOCATION == "Boelstad" & merged_data$SAMPLE_DATE == as.Date("2013-12-31"), 1.1, merged_data$PRECIPITATION)
merged_data$PRECIPITATION <- ifelse(merged_data$LOCATION == "Spillhaug" & merged_data$SAMPLE_DATE == as.Date("2018-12-17"), 2.6, merged_data$PRECIPITATION)


#Give new name to dataset to avoid follow-up mistakes.
Ex7_dataset_with_prectipitation <- merged_data[, c(1:2,4,6:8,10:11,13)]

#rm(test) 
#test_baby <- Ex7_dataset_with_prectipitation %>% filter(SITE_CODE %in% c("Sp_Llut", "Sp_VM3"))

#test_baby <- Ex7_dataset_with_prectipitation %>% filter(PARAMETER_ENG %in% c("Perfluorooctanoic acid (PFOA)", "Perfluorooctanesulfonate (PFOS)", "Perfluoroheksan acid (PFHxA)"))
#rm(df2)






#Improve column name
names(Ex7_dataset_with_prectipitation)[8] <- paste("TEMPERATURE_VALUE")
names(Ex7_dataset_with_prectipitation)[9] <- paste("PRECIPITATION_VALUE")
Ex7_dataset_with_prectipitation$TEMPERATURE_UNIT <- "C"
Ex7_dataset_with_prectipitation$PRECIPITATION_UNIT <- "mm"
Ex7_dataset_with_prectipitation <- Ex7_dataset_with_prectipitation[, c(1:3,6,4:5,7,8,10,9,11)]


#Extract sediment-values. Store as df sediment_values_df.
Ex8_sediment_values_df <- Ex7_dataset_with_prectipitation[grepl("Sed", Ex7_dataset_with_prectipitation$SITE_CODE), ]

#Remove sediment-values from df. Store main dataset as new name.
Ex8_without_sed_values <- Ex7_dataset_with_prectipitation[!grepl("Sed", Ex7_dataset_with_prectipitation$SITE_CODE), ]

#Reset rownumbers.
rownames(Ex8_without_sed_values) <- NULL

#Specify numeric TEMPERATURE_VALUE.
Ex8_without_sed_values$TEMPERATURE_VALUE <- as.numeric(Ex8_without_sed_values$TEMPERATURE_VALUE)

#Extract Sp_Llut.
Ex8_Spillhaug_AiredLagoonPriCW <- Ex8_without_sed_values %>% filter(SITE_CODE == "Sp_Llut")


#For leachate into the treatment facility of Spillhaug, a mean of Sp_K1 and Sp_K2 are considered reliable. Sp_K2 and Sp_K1 share some common SAMPLE_DATES and PARAMETER_ENG (but some Sp_K2 are missing in the dataset). Hence, where Sp_K1 and Sp_K2 are both present, a mean of MEASURED_VALUE is implemented. Where only Sp_K1 is present, the row is kept.

Sp_K1_df <- Ex8_without_sed_values %>% filter(SITE_CODE == "Sp_K1")
Sp_K2_df <- Ex8_without_sed_values %>% filter(SITE_CODE == "Sp_K2")

#Remove duplicates
Sp_K1_df <- Sp_K1_df %>% distinct(SAMPLE_DATE, PARAMETER_ENG, .keep_all = TRUE)
Sp_K2_df <- Sp_K2_df %>% distinct(SAMPLE_DATE, PARAMETER_ENG, .keep_all = TRUE)

#Combine dataset based on common SAMPLE_DATE and PARAMETER_ENG
merged_data_Spillhaug <- full_join(Sp_K1_df, Sp_K2_df, by = c("SAMPLE_DATE", "PARAMETER_ENG"), suffix = c("_k1", "_k2"))

#Create a mean value for common SAMPLE_DATE and PARAMETER_ENG. Otherwise, keep Sp_K1 (there are generally more values for Sp_K1 than Sp_K2).
merged_data_Spillhaug <- as.data.frame(merged_data_Spillhaug)

#Create numeric columns and remove NA-values.
merged_data_Spillhaug$MEASURED_VALUE_k1 <- as.numeric(merged_data_Spillhaug$MEASURED_VALUE_k1)
merged_data_Spillhaug$MEASURED_VALUE_k2 <- as.numeric(merged_data_Spillhaug$MEASURED_VALUE_k2)

#Remove NA-values.
merged_data_Spillhaug <- merged_data_Spillhaug %>% filter(!is.na(MEASURED_VALUE_k1))
#merged_data_Spillhaug <- merged_data_Spillhaug %>% filter(!is.na(MEASURED_VALUE_k2))


 Spillhaug_mean_value_SpIn <- merged_data_Spillhaug %>%
  mutate(MEASURED_VALUE = ifelse(!is.na(MEASURED_VALUE_k1) & !is.na(MEASURED_VALUE_k2),
                                 (MEASURED_VALUE_k1 + MEASURED_VALUE_k2) / 2,
                                 ifelse(!is.na(MEASURED_VALUE_k1), MEASURED_VALUE_k1, MEASURED_VALUE_k2))) %>%
  mutate(MEASURED_UNIT = ifelse(!is.na(MEASURED_UNIT_k1), MEASURED_UNIT_k1, MEASURED_UNIT_k2)) %>%
  mutate(TEMPERATURE_VALUE = ifelse(!is.na(TEMPERATURE_VALUE_k1), 
                                    TEMPERATURE_VALUE_k1, TEMPERATURE_VALUE_k2)) %>%
  mutate(PRECIPITATION_VALUE = ifelse(!is.na(PRECIPITATION_VALUE_k1), 
                                      PRECIPITATION_VALUE_k1, PRECIPITATION_VALUE_k2)) %>%
   mutate(LOQ = ifelse(!is.na(LOQ_k1), LOQ_k1, LOQ_k2)) %>%
  mutate(TEMPERATURE_UNIT = "C", PRECIPITATION_UNIT = "mm", SITE_CODE = "Sp_K1K2", LOCATION = "Spillhaug") %>%
  select(LOCATION, SAMPLE_DATE, SITE_CODE, PARAMETER_ENG, MEASURED_VALUE, MEASURED_UNIT, 
         TEMPERATURE_VALUE, TEMPERATURE_UNIT, PRECIPITATION_VALUE, PRECIPITATION_UNIT, LOQ)
  
 #any(is.na(Spillhaug_mean_value_SpIn))

 #Import filtrated values of Spillhaug_mean_value_SpIn into main df. Remove Sp_K1 and Sp_K2. Replace with new values from Spillhaug_mean_value_SpIn:
 temp_df <- Ex8_without_sed_values %>% filter(!SITE_CODE %in% c("Sp_K1", "Sp_K2"))
 
 temp_df$MEASURED_VALUE <- as.numeric(temp_df$MEASURED_VALUE)
 
 #Remove NA-values due to e.g. n.d.
 temp_df <- temp_df %>% filter(!is.na(MEASURED_VALUE))
 
 
 #Store merged df with new name. This is the new main df.
 Ex8_filtratedK1K2_Spillhaug_Boelstad <- temp_df %>%
   full_join(Spillhaug_mean_value_SpIn, by = colnames(temp_df))

 
#Extract "pH"-values.
Ex8_pH <- Ex8_filtratedK1K2_Spillhaug_Boelstad %>% filter(PARAMETER_ENG == "pH")

#Separate data in two groups, based on SITE_CODE.
Boelstad_in_pH_values <- Ex8_pH %>% filter(SITE_CODE == "Bo_Sig1") %>% pull(MEASURED_VALUE)

Boelstad_out_pH_values <- Ex8_pH %>% filter(SITE_CODE == "Bo_UtRA2") %>% pull(MEASURED_VALUE)

#Since not all arguments have the same length, the length needs to be edited.
min_length_Boelstad_pH <- min(length(Boelstad_in_pH_values), length(Boelstad_out_pH_values))
Boelstad_in_pH_values <- Boelstad_in_pH_values[1:min_length_Boelstad_pH]
Boelstad_out_pH_values <- Boelstad_out_pH_values[1:min_length_Boelstad_pH]

#Conduct pairwise t-test for pH in and out of Boelstad.
t_test_result_Boelstad <- t.test(Boelstad_in_pH_values, Boelstad_out_pH_values, paired = TRUE)

#Print results.
print(t_test_result_Boelstad) #There is a significant difference between pH in and out for Boelstad.

#Doing the same paired t-test for Spillhaug:
Spillhaug_in_pH_values <- Ex8_pH %>% filter(SITE_CODE == "Sp_K1K2") %>% pull(MEASURED_VALUE)
Spillhaug_out_pH_values <- Ex8_pH %>% filter(SITE_CODE == "Sp_VM3") %>% pull(MEASURED_VALUE)

#Since not all arguments have the same length, the length needs to be edited.
min_length_Spillhaug_pH <- min(length(Spillhaug_in_pH_values), length(Spillhaug_out_pH_values))
Spillhaug_in_pH_values <- Spillhaug_in_pH_values[1:min_length_Spillhaug_pH]
Spillhaug_out_pH_values <- Spillhaug_out_pH_values[1:min_length_Spillhaug_pH]

#Conduct pairwise t-test
t_test_result_Spillhaug <- t.test(Spillhaug_in_pH_values, Spillhaug_out_pH_values, paired = TRUE)

#Print results.
print(t_test_result_Spillhaug) #Significant difference between pH in and out of treatment facility.


#Create temporary df. Remove MEASURED_UNIT with %, TU, mS/m and °C. Convert µg/l to mg/l and update MEASURED_VALUE.
a2 <- Ex8_filtratedK1K2_Spillhaug_Boelstad

#Change text to universal (UTF-8). This step is repeated here, to be able to extract unusual signs below.
Sys.setlocale("LC_ALL", "en_US.UTF-8")
Sys.getlocale()
  
a2 <- subset(a2, !(MEASURED_UNIT %in% c("%", "TU", "mS/m", "°C")))

#Change wrongly labelled data from "mg/kg TS" to "ug/l":
a2$MEASURED_UNIT <- ifelse (a2$SITE_CODE == "Bo_Sig1" & a2$SAMPLE_DATE == "2023-09-26" & a2$PARAMETER_ENG == "Perfluorooctanoic acid (PFOA)", "ug/l", a2$MEASURED_UNIT)

#Replace "µg/l" with "ug/l" for whole dataset.
a2$MEASURED_UNIT <- gsub("µg/l", "ug/l", a2$MEASURED_UNIT)

index_convert_ug_to_mg <- which(a2$MEASURED_UNIT == "ug/l")

#Convert from "µg/l" to "mg/l" in both MEASURED_VALUE and MEASURED_UNIT.
a2$MEASURED_VALUE[index_convert_ug_to_mg] <- a2$MEASURED_VALUE[index_convert_ug_to_mg]/1000
a2$MEASURED_UNIT[index_convert_ug_to_mg] <- "mg/l"

#Replace "ng/l" with "mg/l"
index_convert_ng_to_mg <- which(a2$MEASURED_UNIT == "ng/l")
a2$MEASURED_VALUE[index_convert_ng_to_mg] <- a2$MEASURED_VALUE[index_convert_ng_to_mg]/1000000
a2$MEASURED_UNIT[index_convert_ng_to_mg] <- "mg/l"

#Replace temporary df with main dataset.
Ex8_filtratedK1K2_Spillhaug_Boelstad <- a2

  
#Create temp df. Create a column for pH by extracting measured_value for pH and mS/m.
a2$pH <- ifelse(a2$MEASURED_UNIT == "pH", a2$MEASURED_VALUE, NA)

#Create a temorary df. Extract site_code, sample_date and pH.
temp_a2_df <- a2[!is.na(a2$pH), c("SITE_CODE", "SAMPLE_DATE", "pH")]

#Combine temp_a2_df with a2, based on SITE_CODE and SAMPLE_DATE. Keep all information from a2 (all.x = TRUE). For all values that lack in a2 originally (and is added from temp_a2_df), store under column name ".y" in result df (meaning new data from temp_a2_df will not be added to existing columns in a2, but in a new column named ".y").
a2 <- merge(a2, temp_a2_df, by = c("SITE_CODE", "SAMPLE_DATE"), all.x = TRUE, suffixes = c("", ".y"))

#Update pH column in a2 to replace NA-values in the column "pH".
a2$pH <- ifelse(is.na(a2$pH), a2$pH.y, a2$pH)

#Remove the redudant pH column
a2$pH.y <- NULL
#rm(temp_a2_df)

#Store a2 (temporary df) as side df with new name Ex8_with_NA_values_for_pH. In case of need later.
Ex9_with_NA_values_of_pH <- a2

#Remove NA-values for pH in main df (removes 1043 rows).
Ex9_without_NA_values_of_pH <- Ex9_with_NA_values_of_pH[!is.na(Ex9_with_NA_values_of_pH$pH), ]


#Remove "pH"-value in main df in MEASUREMENT_UNIT. Removes appr. 100 rows.
Ex9_without_NA_values_of_pH <- subset(Ex9_without_NA_values_of_pH, MEASURED_UNIT != "pH")

#Change column order.
Ex9_without_NA_values_of_pH <- Ex9_without_NA_values_of_pH[,c(3,1,2,4:10,12,11)]


#Split main data into a Boelstad df and a Spillhaug df.
Ex9_without_NA_values_of_pH_Boelstad <- subset(Ex9_without_NA_values_of_pH, LOCATION %in% c("Boelstad"))

Ex9_without_NA_values_of_pH_Spillhaug <- subset(Ex9_without_NA_values_of_pH, LOCATION %in% c("Spillhaug"))


#Create difference-value for Boelstad (Bo_Sig1 - Bo_UtRA) and Spillhaug (Sp_K1 - Sp_VM3).

#Part dataset in two for Boelstad and Spillhaug, based on SITE_CODE. Split dataset of Boelstad, to merge afterwards.
df_Bo_site1 <- Ex9_without_NA_values_of_pH_Boelstad[Ex9_without_NA_values_of_pH_Boelstad$SITE_CODE == "Bo_Sig1",]
df_Bo_site2 <- Ex9_without_NA_values_of_pH_Boelstad[Ex9_without_NA_values_of_pH_Boelstad$SITE_CODE == "Bo_UtRA2",]

#Split dataset for Spillhaug, to merge afterwards.
df_Sp_site1 <- Ex9_without_NA_values_of_pH_Spillhaug[Ex9_without_NA_values_of_pH_Spillhaug$SITE_CODE == "Sp_K1K2",]
df_Sp_site2 <- Ex9_without_NA_values_of_pH_Spillhaug[Ex9_without_NA_values_of_pH_Spillhaug$SITE_CODE == "Sp_VM3",]
df_Sp_site3 <- Ex9_without_NA_values_of_pH_Spillhaug[Ex9_without_NA_values_of_pH_Spillhaug$SITE_CODE == "Sp_Llut",]

#Merging df, based on common SAMPLE_DATE and PARAMETER_ENG. NB! Losing rows, but neccessary, due to comparison.
merged_df_Boelstad <- merge(df_Bo_site1, df_Bo_site2, by = c("SAMPLE_DATE", "PARAMETER_ENG"), suffixes = c("_in", "_out"))

merged_df_Spillhaug_main <- merge(df_Sp_site1, df_Sp_site2, by = c("SAMPLE_DATE", "PARAMETER_ENG"), suffixes = c("_in", "_out"))
merged_df_Spillhaug_firstpart <- merge(df_Sp_site1, df_Sp_site3, by = c("SAMPLE_DATE", "PARAMETER_ENG"), suffixes = c("_in", "_aired"))
merged_df_Spillhaug_lastpart <- merge(df_Sp_site3, df_Sp_site2, by = c("SAMPLE_DATE", "PARAMETER_ENG"), suffixes = c("_aired", "_out"))

#Create difference in MEASURED_VALUE (for both Boelstad and Spillhaug). NB! Note the value in-out, due to less negative numbers.
merged_df_Boelstad$diff_MEASURED_VALUE <- merged_df_Boelstad$MEASURED_VALUE_in - merged_df_Boelstad$MEASURED_VALUE_out

merged_df_Spillhaug_main$diff_MEASURED_VALUE <- merged_df_Spillhaug_main$MEASURED_VALUE_in - merged_df_Spillhaug_main$MEASURED_VALUE_out
merged_df_Spillhaug_firstpart$diff_MEASURED_VALUE <- merged_df_Spillhaug_firstpart$MEASURED_VALUE_in - merged_df_Spillhaug_firstpart$MEASURED_VALUE_aired
merged_df_Spillhaug_lastpart$diff_MEASURED_VALUE <- merged_df_Spillhaug_lastpart$MEASURED_VALUE_aired - merged_df_Spillhaug_lastpart$MEASURED_VALUE_out

#Create difference in pH (for all four datasets above).
merged_df_Boelstad$diff_pH <- merged_df_Boelstad$pH_in - merged_df_Boelstad$pH_out

merged_df_Spillhaug_main$diff_pH <- merged_df_Spillhaug_main$pH_in - merged_df_Spillhaug_main$pH_out
merged_df_Spillhaug_firstpart$diff_pH <- merged_df_Spillhaug_firstpart$pH_in - merged_df_Spillhaug_firstpart$pH_aired
merged_df_Spillhaug_lastpart$diff_pH <- merged_df_Spillhaug_lastpart$pH_aired - merged_df_Spillhaug_lastpart$pH_out

#Remove excess columns in avove datasets. Specify column names and change order.
merged_df_Boelstad$LOCATION_in <- NULL
merged_df_Boelstad$LOCATION_out <- NULL
merged_df_Boelstad$SITE_CODE_in <- NULL
merged_df_Boelstad$SITE_CODE_out <- NULL
merged_df_Boelstad$MEASURED_UNIT_in <- NULL
merged_df_Boelstad$TEMPERATURE_UNIT_in <- NULL
merged_df_Boelstad$TEMPERATURE_VALUE_in <- NULL
merged_df_Boelstad$SITE_CODE_out <- NULL
merged_df_Boelstad$PRECIPITATION_VALUE_in <- NULL
merged_df_Boelstad$PRECIPITATION_UNIT_in <- NULL

colnames(merged_df_Boelstad)[colnames(merged_df_Boelstad) == "PRECIPITATION_VALUE_out"] <- "PRECIPITATION"
colnames(merged_df_Boelstad)[colnames(merged_df_Boelstad) == "PRECIPITATION_UNIT_out"] <- "PRECIPITATION_UNIT"
colnames(merged_df_Boelstad)[colnames(merged_df_Boelstad) == "MEASURED_UNIT_out"] <- "MEASURED_UNIT"
#merged_df_Boelstad <- merged_df_Boelstad[, c(1:5,7:12,6,13)]

merged_df_Spillhaug_main$LOCATION_in <- NULL
merged_df_Spillhaug_main$LOCATION_out <- NULL
merged_df_Spillhaug_main$SITE_CODE_in <- NULL
merged_df_Spillhaug_main$SITE_CODE_out <- NULL
merged_df_Spillhaug_main$MEASURED_UNIT_in <- NULL
merged_df_Spillhaug_main$TEMPERATURE_UNIT_in <- NULL
merged_df_Spillhaug_main$TEMPERATURE_VALUE_in <- NULL
merged_df_Spillhaug_main$SITE_CODE_out <- NULL
merged_df_Spillhaug_main$PRECIPITATION_VALUE_in <- NULL
merged_df_Spillhaug_main$PRECIPITATION_UNIT_in <- NULL

colnames(merged_df_Spillhaug_main)[colnames(merged_df_Spillhaug_main) == "PRECIPITATION_VALUE_out"] <- "PRECIPITATION"
colnames(merged_df_Spillhaug_main)[colnames(merged_df_Spillhaug_main) == "PRECIPITATION_UNIT_out"] <- "PRECIPITATION_UNIT"
colnames(merged_df_Spillhaug_main)[colnames(merged_df_Spillhaug_main) == "MEASURED_UNIT_out"] <- "MEASURED_UNIT"
#merged_df_Spillhaug_main <- merged_df_Spillhaug_main[, c(1:5,7:12,6,13)]

merged_df_Spillhaug_firstpart$LOCATION_in <- NULL
merged_df_Spillhaug_firstpart$LOCATION_aired <- NULL
merged_df_Spillhaug_firstpart$SITE_CODE_in <- NULL
merged_df_Spillhaug_firstpart$SITE_CODE_aired <- NULL
merged_df_Spillhaug_firstpart$MEASURED_UNIT_in <- NULL

merged_df_Spillhaug_firstpart$TEMPERATURE_UNIT_in <- NULL
merged_df_Spillhaug_firstpart$TEMPERATURE_VALUE_in <- NULL
merged_df_Spillhaug_firstpart$SITE_CODE_aired <- NULL
merged_df_Spillhaug_firstpart$PRECIPITATION_VALUE_in <- NULL
merged_df_Spillhaug_firstpart$PRECIPITATION_UNIT_in <- NULL

colnames(merged_df_Spillhaug_firstpart)[colnames(merged_df_Spillhaug_firstpart) == "PRECIPITATION_VALUE_out"] <- "PRECIPITATION"
colnames(merged_df_Spillhaug_firstpart)[colnames(merged_df_Spillhaug_firstpart) == "PRECIPITATION_UNIT_out"] <- "PRECIPITATION_UNIT"
colnames(merged_df_Spillhaug_firstpart)[colnames(merged_df_Spillhaug_firstpart) == "MEASURED_UNIT_out"] <- "MEASURED_UNIT"
#merged_df_Spillhaug_firstpart <- merged_df_Spillhaug_firstpart[, c(1:5,7:12,6,13)]

merged_df_Spillhaug_lastpart$LOCATION_aired <- NULL
merged_df_Spillhaug_lastpart$LOCATION_out <- NULL
merged_df_Spillhaug_lastpart$SITE_CODE_aired <- NULL
merged_df_Spillhaug_lastpart$SITE_CODE_out <- NULL

merged_df_Spillhaug_lastpart$MEASURED_UNIT_aired <- NULL
merged_df_Spillhaug_lastpart$TEMPERATURE_UNIT_aired <- NULL
merged_df_Spillhaug_lastpart$TEMPERATURE_VALUE_aired <- NULL
merged_df_Spillhaug_lastpart$SITE_CODE_aired <- NULL
merged_df_Spillhaug_lastpart$PRECIPITATION_VALUE_aired <- NULL
merged_df_Spillhaug_lastpart$PRECIPITATION_UNIT_aired <- NULL

colnames(merged_df_Spillhaug_lastpart)[colnames(merged_df_Spillhaug_lastpart) == "PRECIPITATION_VALUE_out"] <- "PRECIPITATION"
colnames(merged_df_Spillhaug_lastpart)[colnames(merged_df_Spillhaug_lastpart) == "PRECIPITATION_UNIT_out"] <- "PRECIPITATION_UNIT"
colnames(merged_df_Spillhaug_lastpart)[colnames(merged_df_Spillhaug_lastpart) == "MEASURED_UNIT_out"] <- "MEASURED_UNIT"
#merged_df_Spillhaug_lastpart <- merged_df_Spillhaug_lastpart[, c(1:5,7:12,6,13)]


#Creating a column for percentage removal efficiency for each parameter:
merged_df_Boelstad$REM_EFF_PERCENT <- ((merged_df_Boelstad$diff_MEASURED_VALUE / merged_df_Boelstad$MEASURED_VALUE_in) * 100)

merged_df_Spillhaug_main$REM_EFF_PERCENT <- ((merged_df_Spillhaug_main$diff_MEASURED_VALUE / merged_df_Spillhaug_main$MEASURED_VALUE_in) * 100)
merged_df_Spillhaug_firstpart$REM_EFF_PERCENT <- ((merged_df_Spillhaug_firstpart$diff_MEASURED_VALUE / merged_df_Spillhaug_firstpart$MEASURED_VALUE_in) * 100)
merged_df_Spillhaug_lastpart$REM_EFF_PERCENT <- ((merged_df_Spillhaug_lastpart$diff_MEASURED_VALUE / merged_df_Spillhaug_lastpart$MEASURED_VALUE_aired) * 100)

#Create bar chart. Divide into temperature categories for dfs. NB! Note that we only have TEMPERATURE_VALUE_out, hence there are no measurements for merged_df_Spillhaug_firstpart. There is for merged_df_Spillhaug_firstpart, but since merged_df_Boelstad lacks measurements in the middle of the treatment plant, it is considered not fruitful to include temperature in any dataset exept main datasets for both treatment plants. TEMPERATURE_CATEGORY is based on average measurement of temperature of Boelstad (8,1C) and Spillhaug (7.8C). Hence "winter" is rounded off <8C and "summer" >8C.
merged_df_Boelstad$TEMPERATURE_CATEGORY <- cut(merged_df_Boelstad$TEMPERATURE_VALUE_out, breaks = c(-Inf, 8, Inf), labels = c("Winter", "Summer"))

merged_df_Spillhaug_main$TEMPERATURE_CATEGORY <- cut(merged_df_Spillhaug_main$TEMPERATURE_VALUE_out, breaks = c(-Inf, 8, Inf), labels = c("Winter", "Summer"))


#---------------------------------

#Remove outlier for ammonium-N and iron:
merged_df_Spillhaug_main <- merged_df_Spillhaug_main[!(merged_df_Spillhaug_main$PARAMETER_ENG == "Ammonium-N" & merged_df_Spillhaug_main$MEASURED_VALUE_in == 0.31 & merged_df_Spillhaug_main$MEASURED_VALUE_out == 11.2), ]

merged_df_Boelstad <- merged_df_Boelstad[!(merged_df_Boelstad$PARAMETER_ENG == "Iron" & merged_df_Boelstad$MEASURED_VALUE_in == 196 & merged_df_Boelstad$MEASURED_VALUE_out == 5.29), ]

merged_df_Boelstad <- merged_df_Boelstad[!(merged_df_Boelstad$PARAMETER_ENG == "Iron" & merged_df_Boelstad$MEASURED_VALUE_in == 128 & merged_df_Boelstad$MEASURED_VALUE_out == 2.11), ]

#Create colum for mean removal efficiency (%) and standard deviation for each PARAMETER_ENG. No matter of SAMPLE_DATE or TEMPERATURE_CATEGORY.
Boelstad_clean_data <- merged_df_Boelstad %>%
  group_by(PARAMETER_ENG) %>%
  mutate(MEAN_REM_EFF_PERCENT = mean(REM_EFF_PERCENT, na.rm = TRUE))

Boelstad_clean_data <- merged_df_Boelstad %>%
  group_by(PARAMETER_ENG) %>%
  mutate(SD_VALUE_PERCENT = sd(REM_EFF_PERCENT, na.rm = TRUE))

Boelstad_clean_data <- Boelstad_clean_data[, c(1:16,18,17)]

Spillhaug_clean_data <- merged_df_Spillhaug_main %>%
  group_by(PARAMETER_ENG) %>%
  mutate(MEAN_REM_EFF_PERCENT = mean(REM_EFF_PERCENT, na.rm = TRUE))

Spillhaug_clean_data <- merged_df_Spillhaug_main %>%
  group_by(PARAMETER_ENG) %>%
  mutate(SD_VALUE_PERCENT = sd(REM_EFF_PERCENT, na.rm = TRUE))

Spillhaug_clean_data <- Spillhaug_clean_data[, c(1:16,18,17)]

Spillhaug_clean_data_firstpart <- merged_df_Spillhaug_firstpart %>%
  group_by(PARAMETER_ENG) %>%
  mutate(MEAN_REM_EFF_PERCENT = mean(REM_EFF_PERCENT, na.rm = TRUE))

Spillhaug_clean_data_firstpart <- merged_df_Spillhaug_firstpart %>%
  group_by(PARAMETER_ENG) %>%
  mutate(SD_VALUE_PERCENT = sd(REM_EFF_PERCENT, na.rm = TRUE))

Spillhaug_clean_data_lastpart <- merged_df_Spillhaug_lastpart %>%
  group_by(PARAMETER_ENG) %>%
  mutate(MEAN_REM_EFF_PERCENT = mean(REM_EFF_PERCENT, na.rm = TRUE))

Spillhaug_clean_data_lastpart <- merged_df_Spillhaug_lastpart %>%
  group_by(PARAMETER_ENG) %>%
  mutate(SD_VALUE_PERCENT = sd(REM_EFF_PERCENT, na.rm = TRUE))



#Notice how mnany NaN-values we get for REM_EFF_PERCENT, due to 0 in and 0 out of MEASURED_VALUE for df for Spillhaug.
#The INF-values are removed for Spillhaug, due to measure error assumptions (low values measured, easy to make mistakes).

#------------------------------- T-Test for MEASURED_VALUE Boelstad/Spillhaug -------------------------------
  
#Check if values in/aired/out have a significant difference by using a paired t-test. Conduct pairwise t-test for pH in and out of Boelstad and Spillhaug for MEASURED_VALUE.
t_test_result_Boelstad_clean_data <- t.test(Boelstad_clean_data$MEASURED_VALUE_in, Boelstad_clean_data$MEASURED_VALUE_out, paired = TRUE) #Significant difference between data in/out with p< 0.05.

t_test_result_Spillhaug_clean_data <- t.test(Spillhaug_clean_data$MEASURED_VALUE_in, Spillhaug_clean_data$MEASURED_VALUE_out, paired = TRUE) #Significant difference between data in/out with p< 0.05.
t_test_result_Spillhaug_clean_data_firstpart <- t.test(Spillhaug_clean_data_firstpart$MEASURED_VALUE_in, Spillhaug_clean_data_firstpart$MEASURED_VALUE_aired, paired = TRUE) #NB! There is NOT a significant difference between data in/aired with p< 0.05!
t_test_result_Spillhaug_clean_data_lastpart <- t.test(Spillhaug_clean_data_lastpart$MEASURED_VALUE_aired, Spillhaug_clean_data_lastpart$MEASURED_VALUE_out, paired = TRUE) #Significant difference between data in/out with p< 0.05.

#Print results from t-test.
print(t_test_result_Spillhaug_clean_data_lastpart) #There is a significant difference between MEASURED_VALUE in/out for Boelstad and Spillhaug, but not for part-dataset Spillhaug in/aired.

#Hence, due to lack of statistical significant difference between Spillhaug in/aired, and the fact that Boelstad does not have measures in between aired leachate and wetland, the main focus will be on in/out of both Boelstad and Spillhaug from now on. 



#Remove duplicates in dfs with same SAMPLE_DATE and PARAMETER_ENG.
Boelstad_clean_data <- Boelstad_clean_data %>% distinct(SAMPLE_DATE, PARAMETER_ENG, .keep_all = TRUE)
Spillhaug_clean_data<- Spillhaug_clean_data %>% distinct(SAMPLE_DATE, PARAMETER_ENG, .keep_all = TRUE)

#Clean datasets of "Inf"-values. 
Boelstad_clean_data <- Boelstad_clean_data[(is.finite(Boelstad_clean_data$REM_EFF_PERCENT)), ]
Spillhaug_clean_data <- Spillhaug_clean_data[(is.finite(Spillhaug_clean_data$REM_EFF_PERCENT)), ]

#Create mean-value column per dataset.
Spillhaug_clean_data <- Spillhaug_clean_data %>%
  group_by(PARAMETER_ENG) %>%
  mutate(MEAN_REM_EFF_PERCENT = mean(REM_EFF_PERCENT, na.rm = TRUE))

Boelstad_clean_data <- Boelstad_clean_data %>%
  group_by(PARAMETER_ENG) %>%
  mutate(MEAN_REM_EFF_PERCENT = mean(REM_EFF_PERCENT, na.rm = TRUE))

#Create mean-value and SD column per PARAMETER_ENG and TEMPERATURE_CATEGORY.
Spillhaug_clean_data <- Spillhaug_clean_data %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY) %>%
  mutate(MEAN_REM_EFF_PERCENT_TEMPERATURE = mean(REM_EFF_PERCENT, na.rm = TRUE))

Spillhaug_clean_data <- Spillhaug_clean_data %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY) %>%
  mutate(SD_REM_EFF_PERCENT_TEMPERATURE = sd(REM_EFF_PERCENT, na.rm = TRUE))

Boelstad_clean_data <- Boelstad_clean_data %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY) %>%
  mutate(MEAN_REM_EFF_PERCENT_TEMPERATURE = mean(REM_EFF_PERCENT, na.rm = TRUE))

Boelstad_clean_data <- Boelstad_clean_data %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY) %>%
  mutate(SD_REM_EFF_PERCENT_TEMPERATURE = sd(REM_EFF_PERCENT, na.rm = TRUE))



#Create an extra column with location for both dfs.
Boelstad_clean_data$LOCATION <- "Boelstad"

Spillhaug_clean_data$LOCATION <- "Spillhaug"


#Create an extra df to be able to plot graphs per PARAMETER_ENG. To see if the stressors are present in the leachate. Several non-present stressors will later be removed, based on this plot.
df_check_all_stressors_present_Boelstad_Spillhaug <- bind_rows(Boelstad_clean_data, Spillhaug_clean_data)

#Check for statistical significance between MEASURED_VALUE_in and MEASURED_VALUE_out to see what PARAMETER_ENG should be removed.
filtered_data <- df_check_all_stressors_present_Boelstad_Spillhaug %>%
  group_by(LOCATION, PARAMETER_ENG) %>%
  filter(n() >= 2)

#Create paired t-test for each LOCATION and PARAMETER_ENG.
p_value_results_Boelstad_Spillhaug <- filtered_data %>%
  group_by(LOCATION, PARAMETER_ENG) %>%
  summarise(t_test = list(tidy(t.test(MEASURED_VALUE_in, MEASURED_VALUE_out, paired = TRUE))), .groups = 'drop') %>%
  unnest(t_test)

#Remove PARAMETER_ENG where p > 0.05 for MEASURED_VALUE_in and MEASURED_VALUE_out. NB! Mention in the text which of the PARAMETER_ENG are removed due to NOT PRESENT in leachate.
Significant_values_Boelstad_Spillhaug <- df_check_all_stressors_present_Boelstad_Spillhaug %>%
  filter(!grepl("1,1,2,2-Tetrahydroperfluoro-1-decanol|6:2 Fluorotelomer sulfonic acid|8:2 Fluorotelomer sulfonic acid|Acenaphthylene|Anthracene|Antimony|Benz\\(a\\)antracene|Benzo\\(a\\)pyrene|Benzo\\(bjk\\)fluoranthene|Benzo\\(k\\)fluoranthene|Benzo\\[b\\]fluoranthene|Benzo\\(ghi\\)perylene|Benzo\\(k\\)fluoranthene|Boron|Chrysene|Dibenz\\[a,h\\]anthracene|Ethylbenzene|EtFOSE|Fluoranthene|Etyhlbenzene|Indeno\\[1,2,3-cd\\]pyrene|MeFOSA|MeFOSE|Oil \\(C35-C40\\)|Oil \\(C10-C40\\)|Perfluorobutanesulfonic acid|Perfluorodecanoic acid \\(PFDA\\)|Perfluorododecanoic acid \\(PFDoDA\\)|Perfluoroheksan acid \\(PFHxA\\)|Perfluorohexsan sulfonate \\(PFHxS\\)|Perfluorononanoic acid \\(PFNA\\)|Perfluorooctanesulfonamide \\(PFOSA\\)|Perfluorooctanesulfonate \\(PFOS\\)|Perfluorooctanoic acid \\(PFOA\\)|Perfluorotetradecanoic acid \\(PFTeDA\\)|Perfluorotridecanoic acid \\(PFTrDA\\)|Phenanthrene|Perfluoroundecanoic acid \\(PFUnA\\)|O-Xylene|Pyrene|Sulfluramid|Sodium|Toluene|Σ Nitrate/nitrite|Σ Xylenes|Σ PFAS", PARAMETER_ENG, ignore.case = TRUE))



#Add an extra function to remove "Oil" from df. Cannot use grepl-function, as it removes all oil values.
#temp <- df_check_all_stressors_present_Boelstad_Spillhaug %>% filter(PARAMETER_ENG == "Oil")
#Significant_values_Boelstad_Spillhaug <- Significant_values_Boelstad_Spillhaug %>% filter(PARAMETER_ENG != "Oil")

#Extract insign. values from dataset. Save with new name. ignore.case = TRUE means not case senstive. 
Insignificant_values_and_PFAS_Boelstad_Spillhaug <- df_check_all_stressors_present_Boelstad_Spillhaug %>%
  filter(grepl("1,1,2,2-Tetrahydroperfluoro-1-decanol|6:2 Fluorotelomer sulfonic acid|8:2 Fluorotelomer sulfonic acid|Acenaphthylene|Anthracene|Antimony|Benz\\(a\\)antracene|Benzo\\(a\\)pyrene|Benzo\\(bjk\\)fluoranthene|Benzo\\(k\\)fluoranthene|Benzo\\[b\\]fluoranthene|Benzo\\(ghi\\)perylene|Benzo\\(k\\)fluoranthene|Boron|Chrysene|Dibenz\\[a,h\\]anthracene|Ethylbenzene|EtFOSE|Fluoranthene|Etyhlbenzene|Indeno\\[1,2,3-cd\\]pyrene|MeFOSA|MeFOSE|Oil \\(C35-C40\\)|Oil \\(C10-C40\\)|Perfluorobutanesulfonic acid|Perfluorodecanoic acid \\(PFDA\\)|Perfluorododecanoic acid \\(PFDoDA\\)|Perfluoroheksan acid \\(PFHxA\\)|Perfluorohexsan sulfonate \\(PFHxS\\)|Perfluorononanoic acid \\(PFNA\\)|Perfluorooctanesulfonamide \\(PFOSA\\)|Perfluorooctanesulfonate \\(PFOS\\)|Perfluorooctanoic acid \\(PFOA\\)|Perfluorotetradecanoic acid \\(PFTeDA\\)|Perfluorotridecanoic acid \\(PFTrDA\\)|Phenanthrene|Perfluoroundecanoic acid \\(PFUnA\\)|O-Xylene|Pyrene|Sulfluramid|Sodium|Toluene|Σ Nitrate/nitrite|Σ Xylenes|Σ PFAS", PARAMETER_ENG, ignore.case = TRUE))

#Add column for mean difference MEASURED_VALUE with and without temperature category.
Significant_values_Boelstad_Spillhaug <- Significant_values_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(MEAN_diff_MEASURED_VALUE = mean(diff_MEASURED_VALUE, na.rm = TRUE))

Significant_values_Boelstad_Spillhaug <- Significant_values_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY, LOCATION) %>%
  mutate(MEAN_diff_MEASURED_VALUE_TEMPERATURE = mean(diff_MEASURED_VALUE, na.rm = TRUE))

Significant_values_Boelstad_Spillhaug <- Significant_values_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(SD_diff_MEASURED_VALUE = sd(diff_MEASURED_VALUE, na.rm = TRUE))

Significant_values_Boelstad_Spillhaug <- Significant_values_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY, LOCATION) %>%
  mutate(SD_diff_MEASURED_VALUE_TEMPERATURE = sd(diff_MEASURED_VALUE, na.rm = TRUE))



Insignificant_values_and_PFAS_Boelstad_Spillhaug <- Insignificant_values_and_PFAS_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(MEAN_diff_MEASURED_VALUE = mean(diff_MEASURED_VALUE, na.rm = TRUE))

Insignificant_values_and_PFAS_Boelstad_Spillhaug <- Insignificant_values_and_PFAS_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY, LOCATION) %>%
  mutate(MEAN_diff_MEASURED_VALUE_TEMPERATURE = mean(diff_MEASURED_VALUE, na.rm = TRUE))

Insignificant_values_and_PFAS_Boelstad_Spillhaug <- Insignificant_values_and_PFAS_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(SD_diff_MEASURED_VALUE = sd(diff_MEASURED_VALUE, na.rm = TRUE))

Insignificant_values_and_PFAS_Boelstad_Spillhaug <- Insignificant_values_and_PFAS_Boelstad_Spillhaug %>%
  group_by(PARAMETER_ENG, TEMPERATURE_CATEGORY, LOCATION) %>%
  mutate(SD_diff_MEASURED_VALUE_TEMPERATURE = sd(diff_MEASURED_VALUE, na.rm = TRUE))

#Change column order.
Significant_values_Boelstad_Spillhaug <- Significant_values_Boelstad_Spillhaug[,c(22,1:14,23,24,25,26,15,16,18,19,17,20,21)]

Insignificant_values_and_PFAS_Boelstad_Spillhaug <- Insignificant_values_and_PFAS_Boelstad_Spillhaug[,c(22,1:14,23,24,25,26,15,16,18,19,17,20,21)]


#--------------------------

#Create a generic function to summarize data.
#get_data <- function(data) { data %>% 
#  group_by(LOCATION, PARAMETER_ENG) %>% 
 # reframe(
   #     Stressor = PARAMETER_ENG,
   #     Year = paste(min(year(SAMPLE_DATE)), max(year(SAMPLE_DATE)), sep = "-"),
    #    Mean_Treatment_Removal = sprintf("%.3g", first(MEAN_REM_EFF_PERCENT)),
    #    Mean_Treatment_Removal_Summer = sprintf("%.3g", first(MEAN_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"])),
    #    Mean_Treatment_Removal_Winter = sprintf("%.3g", first(MEAN_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"])),
    #    SD_Mean_Removal_Percent = sprintf("%.3g", first(SD_VALUE_PERCENT)),
    #    SD_Mean_Removal_Summer_Percent = sprintf("%.3g", first(SD_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"])),
    #    SD_Mean_Removal_Winter_Percent = sprintf("%.3g", first(SD_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"])),
#LOQ_in_count = sum(LOQ_in == 1, na.rm = TRUE),
#LOQ_out_count = sum(LOQ_out == 1, na.rm = TRUE),
#Mean_Difference_Measured_Valu = sprintf("%.3g", first(MEAN_diff_MEASURED_VALUE)),
#Mean_Difference_Measured_Value_Summer = sprintf("%.3g", first(MEAN_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"])),
  #      Mean_Difference_Measured_Value_Winter = sprintf("%.3g", first(MEAN_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"])),
       # SD_Mean_Removal_MEASURED_VALUE = sprintf("%.3g", sd(MEAN_diff_MEASURED_VALUE)),
      #  SD_Mean_Removal_MEASURED_VALUE_Summer = sprintf("%.3g", sd(MEAN_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"])),
      #  SD_Mean_Removal_MEASURED_VALUE_Winter = sprintf("%.3g", sd(MEAN_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"])),
      #  Temperature_Category_Summer_Winter = paste("Summer:", sum(TEMPERATURE_CATEGORY == "Summer"), "Winter:", sum(TEMPERATURE_CATEGORY == "Winter")),
    #  ) %>%
    #  distinct(LOCATION, PARAMETER_ENG, .keep_all = TRUE) %>%
    #  arrange(PARAMETER_ENG, LOCATION)
#}

#Combine dataframes and print Word-doc for summary.
#result <- get_data(Significant_values_Boelstad_Spillhaug)
#result2 <- get_data(Insignificant_values_and_PFAS_Boelstad_Spillhaug)

#combined_result <- rbind(result, result2)

#Print document
#doc <- read_docx()
#doc <- body_add_table(doc, value = combined_result, style = "table_template")

#print(doc, target = "resultat.docx")

#Create word document to see table.

#get_data <- function(data) {
 # data %>%
  #  group_by(LOCATION, PARAMETER_ENG) %>%
   # reframe(
   #   Stressor = PARAMETER_ENG,
   #   Year = paste(min(year(SAMPLE_DATE)), max(year(SAMPLE_DATE)), sep = "-"),
   #   Mean_Difference_Measured_Value = sprintf("%.3g (%.3g)", first(MEAN_diff_MEASURED_VALUE), first(SD_diff_MEASURED_VALUE)),
   #   Mean_Difference_Measured_Value_Summer = sprintf("%.3g (%.3g)", first(MEAN_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"]), first(SD_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"])),
#      Mean_Difference_Measured_Value_Winter = sprintf("%.3g (%.3g)", first(MEAN_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"]), first(SD_diff_MEASURED_VALUE_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"])),
 #     Mean_Treatment_Removal = sprintf("%.3g (%.3g)", first(MEAN_REM_EFF_PERCENT), first(SD_VALUE_PERCENT)),
 #     Mean_Treatment_Removal_Summer = sprintf("%.3g (%.3g)", first(MEAN_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"]), first(SD_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Summer"])),
  #    Mean_Treatment_Removal_Winter = sprintf("%.3g (%.3g)", first(MEAN_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"]), first(SD_REM_EFF_PERCENT_TEMPERATURE[TEMPERATURE_CATEGORY == "Winter"])),
  #    LOQ_in_count = sum(LOQ_in == 1),
  #    LOQ_out_count = sum(LOQ_out == 1),
  #    Temperature_Category_Summer_Winter = paste("Summer:", sum(TEMPERATURE_CATEGORY == "Summer"), "Winter:", sum(TEMPERATURE_CATEGORY == "Winter")),
#    ) %>%
#    distinct(LOCATION, PARAMETER_ENG, .keep_all = TRUE) %>%
#    arrange(PARAMETER_ENG, LOCATION)
#}

# Combine dataframes and print Word document for summary with SD in paranthesis.
#result3 <- get_data(Significant_values_Boelstad_Spillhaug)
#result4 <- get_data(Insignificant_values_and_PFAS_Boelstad_Spillhaug)

#combined_result2 <- rbind(result3, result4)

#doc <- read_docx()
#doc2 <- body_add_table(doc, value = combined_result2, style = "table_template")

#print(doc2, target = "resultat2.docx")


#-----------------------------
#Extract relevant stressors
filtered_df1 <- Insignificant_values_and_PFAS_Boelstad_Spillhaug %>%
  filter(PARAMETER_ENG %in% c("Perfluoroheksan acid (PFHxA)",
"Perfluorooctanesulfonate (PFOS)",
"Perfluorooctanoic acid (PFOA)"))

filtered_df2 <- Significant_values_Boelstad_Spillhaug %>% filter(PARAMETER_ENG %in% c("Acenaphthene", "Ammonium-N", "BTEX", "Benzene", "Chemical Oxygen Demand (COD)", "Chloride Ion (Cl)", "Chromium", "Fluorene", "Iron", "Manganese", "NO3-N", "Nickel", "Nitrogen", "Phosphorus", "Suspended matter", "TOC", "Zinc"))

#"Acenaphthene", "Ammonium-N", "Arsenic", "BTEX", "Benzene", "Chromium", "Copper", "Fluorene", "Iron",  "Manganese", "NO3-N", "Naphthalene", "Nitrogen", "Phosphorus", "Suspended Matter", "TOC", "Zinc", "Σ PAH16 (USEPA)"
#Combine the filtered dataframes
combined_df <- bind_rows(filtered_df1, filtered_df2)

#Remove neccessary outliers.
#combined_df <- combined_df %>%
#filter(!(LOCATION == "Spillhaug" & SAMPLE_DATE == "2003-11-06" & MEASURED_VALUE_in == 0.310))

#Transform SAMPLE_DATE to YEAR, to include in lmer-function below.
combined_df$SAMPLE_DATE <- format(as.Date(combined_df$SAMPLE_DATE, "%Y-%m-%d"), "%Y")

#Rename column
combined_df <- combined_df %>% rename (YEAR = SAMPLE_DATE)

#Convert from character to numeric.
combined_df$YEAR <- as.numeric(combined_df$YEAR)


#Create boxplot for all environmental toxins (PARAMETER_ENG) and diff_MEASURED_VALUE:
a <- combined_df

#Remove outliers for specific suspended matter
a <- a %>%
  filter(
    !(LOCATION == "Boelstad" & YEAR == 1995) &
      !(LOCATION == "Boelstad" & YEAR == 1996) &
      !(LOCATION == "Boelstad" & YEAR == 1997))


#a <- a %>%
  # filter(
  #   !(LOCATION == "Boelstad" & YEAR == 1999 & MEASURED_VALUE_in == 388.0) &
  #     !(LOCATION == "Boelstad" & YEAR == 1999 & diff_MEASURED_VALUE == -191.00))

#Recalculate sd for SD_VALUE_PERCENT:

a <- a %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(SD_VALUE_PERCENT = sd(REM_EFF_PERCENT))

#Create overview of stressors in table

evaluate <- Ex9_with_NA_values_of_pH


evaluate <- evaluate %>%
  mutate(YEAR = year(SAMPLE_DATE))

evaluate <- evaluate %>%
  filter(
    !(LOCATION == "Boelstad" & YEAR == 1995) &
      !(LOCATION == "Boelstad" & YEAR == 1996) &
      !(LOCATION == "Boelstad" & YEAR == 1997))

#Extract measured values of stressors and paste in columns.
evaluate <- evaluate %>% 
  #arrange(PARAMETER_ENG & LOCATION) %>%
  mutate(
    MEASURED_VALUE_in = ifelse(SITE_CODE %in% c("Sp_K1K2","Bo_Sig1"), MEASURED_VALUE, NA),
    MEASURED_VALUE_out = ifelse(SITE_CODE %in% c("Sp_VM3","Bo_UtRA2"), MEASURED_VALUE, NA)
  )

#Duplicate MEASURED_VALUE_out, if common SAMPLE_DATE, PARAMETER_ENG and LOCATION:
evaluate <- evaluate %>%
  group_by(PARAMETER_ENG, SAMPLE_DATE, LOCATION) %>%
  mutate(
    MEASURED_VALUE_out = if_else(
      !is.na(MEASURED_VALUE_in) & is.na(MEASURED_VALUE_out),
      first(MEASURED_VALUE_out[!is.na(MEASURED_VALUE_out)]),
      MEASURED_VALUE_out)) %>%
  ungroup()

#Duplicate pH values, if common SAMPLE_DATE, PARAMETER_ENG and LOCATION.
evaluate <- evaluate %>%
  group_by(PARAMETER_ENG, SAMPLE_DATE, LOCATION) %>%
  mutate(
    pH = if_else(
      is.na(pH),first(pH[!is.na(pH)]),
      pH))

#Filter columns for !is.na.

evaluate <- evaluate %>%
  filter(!is.na(MEASURED_VALUE_in) & !is.na(MEASURED_VALUE_out))

evaluate <- evaluate %>%
  select(LOCATION, PARAMETER_ENG, MEASURED_VALUE_in,MEASURED_VALUE_out)

evaluate <- evaluate %>%
  group_by (PARAMETER_ENG, LOCATION) %>%
  mutate(
    mean_VALUE_in = mean(MEASURED_VALUE_in),
    SD_VALUE_in = sd(MEASURED_VALUE_in),
    mean_VALUE_out = mean(MEASURED_VALUE_out),
    SD_VALUE_out = sd(MEASURED_VALUE_out)
  )



#Due to scarcity of data for e.g. chloride, as pH-measurements are missing, an older df is used for this plot:

stressors_plot_full <- Ex9_with_NA_values_of_pH %>% filter(PARAMETER_ENG %in% c( "Acenaphthene", "Ammonium-N", "BTEX", "Benzene", "Chemical Oxygen Demand (COD)", "Chloride Ion (Cl)", "Chromium", "Fluorene", "Iron", "Manganese", "NO3-N", "Nickel", "Nitrogen", "Perfluoroheksan acid (PFHxA)","Perfluorooctanesulfonate (PFOS)","Perfluorooctanoic acid (PFOA)", "Phosphorus", "Suspended Matter", "TOC", "Zinc"))

#Add seasons to colour for summer/winter and remove values prior to treatment facility. 

library(lubridate)
stressors_plot_full$SEASON <- ifelse(month(stressors_plot_full$SAMPLE_DATE) %in% 4:9, "Summer", "Winter")

stressors_plot_full <- stressors_plot_full %>%
  mutate(YEAR = year(SAMPLE_DATE))

stressors_plot_full <- stressors_plot_full %>%
  filter(
    !(LOCATION == "Boelstad" & YEAR == 1995) &
      !(LOCATION == "Boelstad" & YEAR == 1996) &
      !(LOCATION == "Boelstad" & YEAR == 1997))

stressors_plot_full <- stressors_plot_full %>%
filter(
!(LOCATION == "Spillhaug" & YEAR == 1999) &
   !(LOCATION == "Spillhaug" & YEAR == 2000) &
    !(LOCATION == "Spillhaug" & YEAR == 2001) &
     !(LOCATION == "Spillhaug" & YEAR == 2003))



#Extract measured values of stressors and paste in columns.
stressors_plot_full<- stressors_plot_full %>% 
#arrange(PARAMETER_ENG & LOCATION) %>%
  mutate(
    MEASURED_VALUE_in = ifelse(SITE_CODE %in% c("Sp_K1K2","Bo_Sig1"), MEASURED_VALUE, NA),
    MEASURED_VALUE_out = ifelse(SITE_CODE %in% c("Sp_VM3","Bo_UtRA2"), MEASURED_VALUE, NA)
  )

#Duplicate MEASURED_VALUE_out, if common SAMPLE_DATE, PARAMETER_ENG and LOCATION:
stressors_plot_full <- stressors_plot_full %>%
  group_by(PARAMETER_ENG, SAMPLE_DATE, LOCATION) %>%
  mutate(
    MEASURED_VALUE_out = if_else(
      !is.na(MEASURED_VALUE_in) & is.na(MEASURED_VALUE_out),
      first(MEASURED_VALUE_out[!is.na(MEASURED_VALUE_out)]),
      MEASURED_VALUE_out)) %>%
  ungroup()

#Duplicate pH values, if common SAMPLE_DATE, PARAMETER_ENG and LOCATION.
stressors_plot_full <- stressors_plot_full %>%
  group_by(PARAMETER_ENG, SAMPLE_DATE, LOCATION) %>%
  mutate(
    pH = if_else(
      is.na(pH),first(pH[!is.na(pH)]),
      pH))

#Filter columns for !is.na.

stressors_plot_full <- stressors_plot_full %>%
  filter(!is.na(MEASURED_VALUE_in) & !is.na(MEASURED_VALUE_out))

#a_baby <- stressors_plot_full %>%
  #filter(PARAMETER_ENG %in% c("Acenaphthene", "Ammonium-N", "Chloride Ion (Cl)", "Chromium", "Fluorene", "Iron"))

#a_baby <- stressors_plot_full %>%
  #filter(PARAMETER_ENG %in% c("Chromium"))


# ggplot(stressors_plot_full, aes(x = YEAR)) +
#   #geom_line(
#     #aes(y = MEASURED_VALUE_out, color = LOCATION, linetype = "In"), 
#     #linewidth = 0.8, alpha = 0.8)+ #Remove line for in-values for simplicity
#   geom_line(
#     aes(y = MEASURED_VALUE_out, color = LOCATION, linetype = "Out"),
#     linewidth = 1.2, alpha = 1)+
#   geom_ribbon(
#     aes(ymin = MEASURED_VALUE_in, ymax = MEASURED_VALUE_out, fill = LOCATION), 
#     alpha = 0.5) +
#   facet_wrap(~ PARAMETER_ENG, scales = "free", ncol = 2
#   ) + 
#   geom_point(aes(y = MEASURED_VALUE_in, shape = SEASON), size = 2) +
#   labs(
#     #title = "Measured Values of Stressors [mg/l]",
#     x = "Year",
#     y = "Measured Value [mg/l]",
#     color = "Location"
#   ) +
#   #scale_shape_manual(values = c("Summer" = 16, "Winter" = 17)) +
#   #scale_color_brewer(palette = "Set1") +   # Bruker Set1 fra RColorBrewer
#   #scale_fill_brewer(palette = "Set1") 
# scale_fill_manual(values = c("#E69F00", "#56B4E9")) + 
#   scale_linetype_manual(values = c("Out" = "solid")) +
#   scale_shape_manual(values = c("Winter" = 1))+#, "Winter" = 3))+
#   theme_minimal() + 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#     axis.text.y = element_text(size = 10),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     strip.text = element_text(size = 14, face = "plain", color = "black"), #Changes names for PARAMETER_ENG
#     #strip.background = element_rect(fill = "black"),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     plot.title = element_text(hjust = 0.5))

#---------------------------------------

stressor_plot_full <- a
#%>%
  #filter(PARAMETER_ENG %in% c("Ammonium-N", "Chloride Ion (Cl)", "Chemical Oxygen Demand (COD)", "Chromium", "Iron", "Manganese", "Nitrogen", "Phosphorus", "Suspended Matter", "TOC"))


stressors_plot_full$diff_MEASURED_VALUE <- stressors_plot_full$MEASURED_VALUE_in - stressors_plot_full$MEASURED_VALUE_out

#Since log transformation is not defined for negative values, we can:
stressors_plot_full$log_signed_diff <- sign(stressors_plot_full$diff_MEASURED_VALUE) * log(abs(stressors_plot_full$diff_MEASURED_VALUE) + 1)

#Remove Spillhaug data before closure.
stressor_plot_full <- stressor_plot_full %>%
  filter(!(LOCATION %in% "Spillhaug" & YEAR %in% c("2002", "2003")))


#Boxplot all values
boxplot_all_values_full <- ggplot(stressor_plot_full, aes(x = PARAMETER_ENG, y = log_signed_diff)) +
  geom_boxplot(aes(fill = LOCATION), width = 15, alpha = 5, size = 0.5) +
  #facet_wrap(~ PARAMETER_ENG, scales = "free_y", ncol = 2) +
  #scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal() +
  labs(
    title = "Contaminants through Leachate Treatment System",
    y = "Concentration Difference (in-out) [mg/l]"
  )+
  theme(
    text = element_text(size = 14), 
    axis.title.x = element_text(size = 10),
    #axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(0.2, "lines"),
    plot.title = element_text(hjust = 0.5)
  ) +
    guides(fill = guide_legend(title = "Location"))

#------------------------------------ FOR IRON -------------------
library(dplyr)
library(bestNormalize)


#PCA of stressors
a <- a %>%
  filter(!(LOCATION %in% "Spillhaug" & YEAR %in% c("1999", "2000", "2001", "2002", "2003")))

#Store new df for PCA and duplicate for later retrieval
all_pca <- a %>%
  filter(PARAMETER_ENG %in% c("Iron"))
    #"Ammonium-N", "Benzene", "BTEX", "Chromium", "Fluorene", "Iron", "Manganese", "Nickel", "Nitrogen", "Phosphorus", "TOC", "Zinc", "Perfluorooctanesulfonate (PFOS)", "Perfluorooctanoic acid (PFOA)", "Perfluoroheksan acid (PFHxA)"))


all_pca <- all_pca %>%
  mutate(
    diff_MEASURED_VALUE_YJ = yeojohnson(diff_MEASURED_VALUE)$x.t)

all_pca2 <- all_pca %>%
  ungroup() %>%
  select(diff_MEASURED_VALUE_YJ, diff_pH, TEMPERATURE_VALUE_out, YEAR, PRECIPITATION)

#Scale variables
all_pca2 <- all_pca2 %>%
  mutate(across(-diff_MEASURED_VALUE_YJ, ~ scale(.)))

#Create PCA and view summary
pca_result <- prcomp(all_pca2, center = TRUE, scale. = TRUE)

summary(pca_result)

screeplot(pca_result, type = "lines", main = "Screeplot (Base R)")


pca_data <- as.data.frame(pca_result$x) #Extract scores (based on observations and loadings)

#Add values for color coding.
pca_data$PARAMETER_ENG <- all_pca$PARAMETER_ENG
pca_data$LOCATION <- all_pca$LOCATION
pca_data$TEMPERATURE_VALUE_out <- all_pca$TEMPERATURE_VALUE_out
pca_data$diff_MEASURED_VALUE_YJ <- all_pca$diff_MEASURED_VALUE_YJ


#Extract name of loadings and values of loadings. For plotting later.
pca_data_loadings<- as.data.frame(pca_result$rotation)
pca_data_loadings$variable <- rownames(pca_result$rotation)

library(ggplot2)

#Plot
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 4, shape = 21, stroke = 0.3, color = "black", aes(fill = diff_MEASURED_VALUE_YJ))+
  #Add loading plot. Dont inherit data frame (= FALSE).
  geom_segment(data = pca_data_loadings,
               aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", inherit.aes = FALSE) +
  
   geom_text(
     data = pca_data_loadings,
    aes(x = PC1 * 5, y = PC2 * 5, label = variable),
    color = "black",
   inherit.aes = FALSE,
   vjust = -0.5,
   size = 5) + #To display arrows
  
  theme(text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate axis

  
  scale_fill_viridis_c(option = "B") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#------------ Correlation heatmap!
#library(reshape2)

#Extract PC1 and PC2
pca_scores <- as.data.frame(pca_result$x[, 1:2])

#Correlation matrix
#Calculate correlation between PCA :scores and original variables
correlation_with_pca <- cor(pca_scores, all_pca2, method = "spearman")

#Print correlation matrices.
print(correlation_with_pca)

#Convert df to make it suitable for plotting using generic function "melt". Change names of column to obtain readability.
library(reshape2)
correlation_melted <- melt(correlation_with_pca)

print(correlation_melted)



correlation_melted <- correlation_melted %>%
  mutate(
    p_value = map2_dbl(Var1, Var2, ~ cor.test(pca_scores[[.x]], all_pca2[[.y]], method = "spearman", exact = FALSE)$p.value),
    p_values = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01  ~ "**",
      p_value <= 0.05  ~ "*",
      TRUE ~ ""))


#Create heatmap
library(ggplot2)
ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "black") +
  
  geom_text(aes(label = p_values), size = 6, color = "black") +
  
  scale_fill_viridis_c(option = "C", limits = c(-1, 1)) +
  
  scale_x_discrete(labels = c(#"MEASURED_VALUE_in" = "Influent",
                              #"MEASURED_VALUE_out" = "Effluent",
                              #"diff_MEASURED_VALUE" = "Treatment Difference",
                              "diff_pH" = "pH",
                              "TEMPERATURE_VALUE_out" = "Temperature",
                              "YEAR" = "Maturity",
                              "PRECIPITATION" = "Precipitation")) +
  scale_y_discrete(labels = c(#"MEASURED_VALUE_in" = "Influent",
                              #"MEASURED_VALUE_out" = "Effluent",
                              #"diff_MEASURED_VALUE" = "Treatment Difference",
                              "diff_pH" = "pH",
                              "TEMPERATURE_VALUE_out" = "Temperature",
                              "YEAR" = "Maturity",
                              "PRECIPITATION" = "Precipitation")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())



#----------------- FOR CHROMIUM -----------------------------------------



#PCA of stressors
a <- a %>%
  filter(!(LOCATION %in% "Spillhaug" & YEAR %in% c("1999", "2000", "2001", "2002", "2003")))

#Store new df for PCA and duplicate for later retrieval
all_pca <- a %>%
  filter(PARAMETER_ENG %in% c("Chromium"))
#"Ammonium-N", "Benzene", "BTEX", "Chromium", "Fluorene", "Iron", "Manganese", "Nickel", "Nitrogen", "Phosphorus", "TOC", "Zinc", "Perfluorooctanesulfonate (PFOS)", "Perfluorooctanoic acid (PFOA)", "Perfluoroheksan acid (PFHxA)"))


all_pca <- all_pca %>%
  mutate(
    diff_MEASURED_VALUE_log = log(diff_MEASURED_VALUE))

all_pca <- all_pca %>%
  filter(is.finite(diff_MEASURED_VALUE_log))

all_pca2 <- all_pca %>%
  ungroup() %>%
  select(diff_MEASURED_VALUE_log, diff_pH, TEMPERATURE_VALUE_out, YEAR, PRECIPITATION)

#Scale variables
#all_pca2 <- all_pca2 %>%
#  mutate(across(-diff_MEASURED_VALUE_log, ~ scale(.)))

#Create PCA and view summary
pca_result <- prcomp(all_pca2, center = TRUE, scale. = TRUE)

summary(pca_result)

#install.packages("factoextra") #Create loadingplot with variable contribution
library(factoextra)
fviz_pca_var(pca_result,
             col.var = "contrib",
             repel = TRUE) #This plot displays how the variables (arrows) contribute to PC1 and PC2. NB! Always less than 1, since scaled.

screeplot(pca_result, type = "lines", main = "Screeplot (Base R)")

#Create biplot
fviz_pca_biplot(pca_result,
                label = "var", #display variables with names
                col.ind = all_pca2$diff_MEASURED_VALUE_log)



pca_data <- as.data.frame(pca_result$x) #Scores from PCA model.

#Add values for color coding.
pca_data$PARAMETER_ENG <- all_pca$PARAMETER_ENG
pca_data$LOCATION <- all_pca$LOCATION
pca_data$TEMPERATURE_VALUE_out <- all_pca$TEMPERATURE_VALUE_out
pca_data$diff_MEASURED_VALUE_log <- all_pca$diff_MEASURED_VALUE_log
pca_data$diff_MEASURED_VALUE <- all_pca$diff_MEASURED_VALUE
pca_data$YEAR <- all_pca$YEAR


#Extract name of loadings and values of loadings. For plotting later.
pca_data_loadings<- as.data.frame(pca_result$rotation) #Rotation/loadings = how much each variable contribute to PCs.
pca_data_loadings$variable <- rownames(pca_result$rotation)

library(ggplot2)

#Create biplot in ggplot
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 4, shape = 21, stroke = 0.3, color = "black", aes(fill = diff_MEASURED_VALUE_log))+
  #Add loading plot. Dont inherit data frame (= FALSE).
  geom_segment(data = pca_data_loadings,
               aes(x = 0, y = 0, xend = PC1*3.6, yend = PC2*3.6), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "darkred", 
               linewidth = 0.9,
               inherit.aes = FALSE) +
  
  # geom_text(
  #   data = pca_data_loadings,
  #   aes(x = PC1*3.6, y = PC2*3.6, label = variable),
  #   color = "black",
  #   inherit.aes = FALSE,
  #   vjust = -0.5,
  #   size = 5) + #To display arrows
  # 
  theme(text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate axis
  
  
  scale_fill_viridis_c(option = "B") +
  theme_minimal()+
  theme(#panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#------------ Correlation heatmap!
#library(reshape2)

#Extract PC1 and PC2
pca_scores <- as.data.frame(pca_result$x[, 1:2])

#Correlation matrix
#Calculate correlation between PCA :scores and original variables
correlation_with_pca <- cor(pca_scores, all_pca2, method = "spearman")

#Print correlation matrices.
print(correlation_with_pca)



#Convert df to make it suitable for plotting using generic function "melt". Change names of column to obtain readability.
library(reshape2)
library(purrr)

correlation_melted <- melt(correlation_with_pca)

print(correlation_melted)

correlation_melted <- correlation_melted %>%
  mutate(
    p_value = map2_dbl(Var1, Var2, ~ cor.test(pca_scores[[.x]], all_pca2[[.y]], method = "spearman", exact = FALSE)$p.value),
    p_values = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01  ~ "**",
      p_value <= 0.05  ~ "*",
      TRUE ~ ""))


#Create heatmap
library(ggplot2)

ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "black") +
  
  geom_text(aes(label = p_values), size = 6, color = "black") +
  
  scale_fill_viridis_c(option = "C", limits = c(-1, 1)) +
  
  scale_x_discrete(labels = c(#"MEASURED_VALUE_in" = "Influent",
    #"MEASURED_VALUE_out" = "Effluent",
    #"diff_MEASURED_VALUE" = "Treatment Difference",
    "diff_pH" = "pH",
    "TEMPERATURE_VALUE_out" = "Temperature",
    "YEAR" = "Maturity",
    "PRECIPITATION" = "Precipitation")) +
  scale_y_discrete(labels = c(#"MEASURED_VALUE_in" = "Influent",
    #"MEASURED_VALUE_out" = "Effluent",
    #"diff_MEASURED_VALUE" = "Treatment Difference",
    "diff_pH" = "pH",
    "TEMPERATURE_VALUE_out" = "Temperature",
    "YEAR" = "Maturity",
    "PRECIPITATION" = "Precipitation")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())



#------ Also create heatmap for original variables to check for correlation.

#To use Spearman, and to compare with the original lmer-model, we use Yeo Johnson-transformed diff_MEASURED_VALUE for iron and logarithmic for chromium.
library(bestNormalize)
library(purrr)

#For IRON! Yeo-Johnson-transformed.

all_pca <- a %>%
  filter(PARAMETER_ENG %in% c("Iron"))
#"Ammonium-N", "Benzene", "BTEX", "Chromium", "Fluorene", "Iron", "Manganese", "Nickel", "Nitrogen", "Phosphorus", "TOC", "Zinc", "Perfluorooctanesulfonate (PFOS)", "Perfluorooctanoic acid (PFOA)", "Perfluoroheksan acid (PFHxA)"))


all_pca <- all_pca %>%
  mutate(
    diff_MEASURED_VALUE_YJ = yeojohnson(diff_MEASURED_VALUE)$x.t)


all_pca2 <- all_pca %>%
  ungroup() %>%
  select(diff_MEASURED_VALUE_YJ, diff_pH, TEMPERATURE_VALUE_out, YEAR, PRECIPITATION)

#Scale variables
all_pca2 <- all_pca2 %>%
  mutate(across(-diff_MEASURED_VALUE_YJ, ~ scale(.)))


cor_original <- cor(all_pca2, method = "spearman")

cor_original_melted <- melt(cor_original)

#Add p-values
cor_original_melted <- cor_original_melted %>%
  mutate(
    p_value = map2_dbl(Var1, Var2, ~ cor.test(all_pca2[[.x]], all_pca2[[.y]], method = "spearman", exact = FALSE)$p.value),
    p_values = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01  ~ "**",
      p_value <= 0.05  ~ "*",
      TRUE ~ ""))

# Heatmap
cor_original_melted %>%
  filter(Var1 == "diff_MEASURED_VALUE_YJ") %>%
  
ggplot(
  
  aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = p_values), size = 6, color = "black") +
  
  scale_fill_viridis_c(option = "C", limits = c(-1, 1)) +
  
  scale_x_discrete(labels = c(
    "diff_MEASURED_VALUE_YJ" = "Treatment Difference (YJ)",
    "diff_pH" = "pH",
    "TEMPERATURE_VALUE_out" = "Temperature",
    "YEAR" = "Maturity",
    "PRECIPITATION" = "Precipitation"
  )) +
  scale_y_discrete(labels = c(
    "diff_MEASURED_VALUE_YJ" = "Treatment Difference (YJ)",
    "diff_pH" = "pH",
    "TEMPERATURE_VALUE_out" = "Temperature",
    "YEAR" = "Maturity",
    "PRECIPITATION" = "Precipitation"
  )) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid = element_blank())


#For chromium! Log transformed.

all_pca <- a %>%
  filter(PARAMETER_ENG %in% c("Chromium"))
#"Ammonium-N", "Benzene", "BTEX", "Chromium", "Fluorene", "Iron", "Manganese", "Nickel", "Nitrogen", "Phosphorus", "TOC", "Zinc", "Perfluorooctanesulfonate (PFOS)", "Perfluorooctanoic acid (PFOA)", "Perfluoroheksan acid (PFHxA)"))


#Log transform! NB! Creates a total of 7 "-Inf" and "NaN" due to no or negative treatment differences.
all_pca2 <- all_pca %>%
  ungroup() %>%
  select(diff_MEASURED_VALUE, diff_pH, TEMPERATURE_VALUE_out, YEAR, PRECIPITATION)

all_pca2 <- all_pca2 %>%
  mutate(
     diff_MEASURED_VALUE_log = log(diff_MEASURED_VALUE)) %>%
   select(-diff_MEASURED_VALUE)
 
 all_pca2 <- all_pca2 %>%
   filter(is.finite(diff_MEASURED_VALUE_log))


#Scale variables
 all_pca2 <- all_pca2 %>%
   mutate(across(-diff_MEASURED_VALUE_log, ~ scale(.)))


cor_original <- cor(all_pca2, method = "spearman")

cor_original_melted <- melt(cor_original)

#Add p-values
cor_original_melted <- cor_original_melted %>%
  mutate(
    p_value = map2_dbl(Var1, Var2, ~ cor.test(all_pca2[[.x]], all_pca2[[.y]], method = "spearman", exact = FALSE)$p.value),
    p_values = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01  ~ "**",
      p_value <= 0.05  ~ "*",
      TRUE ~ ""))

# Heatmap (NB! Only displays treatment difference)
cor_original_melted %>%
  filter(Var1 == "diff_MEASURED_VALUE_log") %>%

ggplot(
  
  aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = p_values), size = 6, color = "black") +
  
  scale_fill_viridis_c(option = "C", limits = c(-1, 1)) +
  
  scale_x_discrete(labels = c(
    "diff_MEASURED_VALUE_log" = "Treatment Difference (ln)",
    "diff_pH" = "pH",
    "TEMPERATURE_VALUE_out" = "Temperature",
    "YEAR" = "Maturity",
    "PRECIPITATION" = "Precipitation"
  )) +
  scale_y_discrete(labels = c(
    "diff_MEASURED_VALUE_log" = "Treatment Difference (ln)",
    "diff_pH" = "pH",
    "TEMPERATURE_VALUE_out" = "Temperature",
    "YEAR" = "Maturity",
    "PRECIPITATION" = "Precipitation"
  )) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid = element_blank())











# 
# 
# #------------------------- WIDE DATA PCA ---------- NB! Large data gaps if all PARAMETER_ENG is included.
# 
# 
# 
# wide_data <- all_pca %>%
#   select(diff_MEASURED_VALUE, diff_pH, TEMPERATURE_VALUE_out, YEAR, PRECIPITATION, PARAMETER_ENG) %>%
#   pivot_wider(names_from = PARAMETER_ENG,
#               values_from = diff_MEASURED_VALUE)
# 
# #wide_data <- na.omit(wide_data)
# 
# 
# #Rm LOCATION before PCA
# wide_data2 <- wide_data %>%
#   ungroup() %>%
#   select(-LOCATION)
# 
# pca_result <- prcomp(wide_data2, center = TRUE, scale. = TRUE)
# 
# summary(pca_result)
# 
# 
# 
# screeplot(pca_result, type = "lines", main = "Screeplot (Base R)")
# 
# 
# pca_data <- as.data.frame(pca_result$x) #Extract scores (based on observations and loadings)
# 
# pca_data$LOCATION <- wide_data$LOCATION #Add location for colour coding.
# 
# #Extract name of loadings and values of loadings. For plotting later.
# pca_data_loadings<- as.data.frame(pca_result$rotation)
# pca_data_loadings$variable <- rownames(pca_result$rotation)
# 
# library(ggplot2)
# 
# #Plot
# ggplot(pca_data, aes(x = PC1, y = PC2)) +
#   geom_point(size = 3, shape = 21, stroke = 0.3, color = "black", aes(fill = LOCATION))+
# 
#   geom_segment(data = pca_data_loadings,
#                aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
#                arrow = arrow(length = unit(0.3, "cm")), 
#                color = "black", inherit.aes = FALSE) +
#   geom_text(
#     data = pca_data_loadings,
#     aes(x = PC1 * 5, y = PC2 * 5, label = variable),
#     color = "black",
#     inherit.aes = FALSE,
#     vjust = -0.5,
#     size = 5) +
#   
#   scale_fill_viridis_d(option = "D") +
#   
#   theme(text = element_text(size = 16),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1))+
#  # labs(
#     #title = "PCA of Environmental Stressors",
#     #x = "PC 1",
#    # y = "PC 2") +
#   theme_minimal(base_size = 18)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# 
# 
# 
# #------------ Correlation heatmap!
# #library(reshape2)
# 
# #Extract PC1 and PC2
# pca_scores <- pca_result$x[, 1:2]
# 
# #Correlation matrix
# #Calculate correlation between PCA :scores and original variables
# correlation_with_pca <- cor(pca_scores, wide_data2, method = "spearman")
# 
# #Print correlation matrices.
# print(correlation_with_pca)
# 
# #Convert df to make it suitable for plotting using generic function "melt". Change names of column to obtain readability.
# library(reshape2)
# correlation_melted <- melt(correlation_with_pca)
# #colnames(correlation_melted) <- c("PCA_Component", "Original_Variable", "Correlation")
# 
# print(correlation_melted)
# 
# 
# 
# #Create heatmap
# library(ggplot2)
# ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile() +
#   #scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1,1)) +
#   #labs(title = "Correlation Heatmap") +
#   scale_x_discrete(labels = c(#"MEASURED_VALUE_in" = "Influent",
#     #"MEASURED_VALUE_out" = "Effluent",
#     "diff_MEASURED_VALUE" = "Treatment Difference",
#     "diff_pH" = "pH",
#     "TEMPERATURE_VALUE_out" = "Temperature",
#     "YEAR" = "Maturity",
#     "PRECIPITATION" = "Precipitation")) +
#   scale_y_discrete(labels = c(#"MEASURED_VALUE_in" = "Influent",
#     #"MEASURED_VALUE_out" = "Effluent",
#     "diff_MEASURED_VALUE" = "Treatment Difference",
#     "diff_pH" = "pH",
#     "TEMPERATURE_VALUE_out" = "Temperature",
#     "YEAR" = "Maturity",
#     "PRECIPITATION" = "Precipitation")) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#     axis.text.y = element_text(angle = 45, hjust = 1, size = 14),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank())






#):------------------------------------------ For temperature --------------------------------
library(lmerTest)
library(bestNormalize)
library(MuMIn)
library(ggplot2)
library(dplyr)

a <- a %>%
  filter(!(LOCATION %in% "Spillhaug" & YEAR %in% c("1999", "2000", "2001", "2002", "2003")))

a2 <- a

data_mang <- a2[a2$PARAMETER_ENG %in% "Manganese", ]
data_ammo <- a2[a2$PARAMETER_ENG %in% "Ammonium-N", ]

yeojohnson_obj_mang <- yeojohnson(data_mang$diff_MEASURED_VALUE)
yeojohnson_obj_ammo <- yeojohnson(data_ammo$diff_MEASURED_VALUE)

data_mang$transformed_MEASURED_VALUE <- yeojohnson_obj_mang$x.t
data_ammo$transformed_MEASURED_VALUE <- yeojohnson_obj_ammo$x.t

#Duplicate column with temperature
data_mang$TEMPERATURE_VALUE_nonScaled <- data_mang$TEMPERATURE_VALUE_out
data_mang$diff_pH_nonScaled <- data_mang$diff_pH
data_mang$YEAR_nonScaled <- data_mang$YEAR
data_mang$PRECIPITATION_nonScaled <- data_mang$PRECIPITATION 


data_ammo$TEMPERATURE_VALUE_nonScaled <- data_ammo$TEMPERATURE_VALUE_out
data_ammo$diff_pH_nonScaled <- data_ammo$diff_pH
data_ammo$YEAR_nonScaled <- data_ammo$YEAR
data_ammo$PRECIPITATION_nonScaled <- data_ammo$PRECIPITATION


#Scale variables, to make regression possible (due to various scales, e.g. degrees vs. pH)
data_mang$diff_pH <- scale(data_mang$diff_pH)
data_mang$TEMPERATURE_VALUE_out <- scale(data_mang$TEMPERATURE_VALUE_out)
data_mang$PRECIPITATION <- scale(data_mang$PRECIPITATION)
data_mang$YEAR <- scale(data_mang$YEAR)

data_ammo$diff_pH <- scale(data_ammo$diff_pH)
data_ammo$TEMPERATURE_VALUE_out <- scale(data_ammo$TEMPERATURE_VALUE_out)
data_ammo$PRECIPITATION <- scale(data_ammo$PRECIPITATION)
data_ammo$YEAR <- scale(data_ammo$YEAR)


#Lmer-models for environmental stressors.
test_lmer_mang <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_mang)

test_lmer_ammo <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_ammo)

#data_mang$predicted_values <- predict(test_lmer_mang)
#data_ammo$predicted_values <- predict(test_lmer_ammo)


#Invert the predicted values to "original values". NB! This also includes re-standardizing y.
#data_mang$original_values_mang <- predict(yeojohnson_obj_mang, newdata = data_mang$predicted_values, inverse = TRUE)
#data_ammo$original_values_ammo <- predict(yeojohnson_obj_ammo, newdata = data_ammo$predicted_values, inverse = TRUE)


#Extract coefficients
fixed_effects_ammo <- coef(summary(test_lmer_ammo))
fixed_effects_mang <- coef(summary(test_lmer_mang))

#Create an intercept, corrected for Yeo-Johnson. Her er det noe muffens! Man skal ha med standardavviket også.
intercept_adjusted_mang <- fixed_effects_mang["(Intercept)", "Estimate"] #-
# fixed_effects_mang["diff_pH", "Estimate"] * mean(data_mang$diff_pH) -
#   fixed_effects_mang["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_mang$TEMPERATURE_VALUE_out) -
#   fixed_effects_mang["YEAR", "Estimate"] * mean(data_mang$YEAR) -
#   fixed_effects_mang["PRECIPITATION", "Estimate"] * mean(data_mang$PRECIPITATION)

intercept_adjusted_ammo <- fixed_effects_ammo["(Intercept)", "Estimate"] #-
 # fixed_effects_ammo["diff_pH", "Estimate"] * mean(data_ammo$diff_pH) -
 # fixed_effects_ammo["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_ammo$TEMPERATURE_VALUE_out) -
 # fixed_effects_ammo["YEAR", "Estimate"] * mean(data_ammo$YEAR) -
 # fixed_effects_ammo["PRECIPITATION", "Estimate"] * mean(data_ammo$PRECIPITATION)

#Create a slope, adjusted for Yeo Johnson, to convert to "real" values:
#intercept_original_mang <- predict(yeojohnson_obj_mang, newdata = intercept_adjusted_mang, inverse = TRUE)

#intercept_original_ammo <- predict(yeojohnson_obj_ammo, newdata = intercept_adjusted_ammo, inverse = TRUE)
  
  
#Create original slope by multiplying with original SD.
slope_original_mang <- fixed_effects_mang["TEMPERATURE_VALUE_out", "Estimate"] 


#Create scaled line
slope_original_ammo <- fixed_effects_ammo["TEMPERATURE_VALUE_out", "Estimate"] 


#Predicition line with real values for temperature. Creating a slope from change in SD temperature vs. measured value to degrees Celsius vs. measured value.
data_mang$pred_line_mang <-  intercept_adjusted_mang + (slope_original_mang*(data_mang$TEMPERATURE_VALUE_out))

data_ammo$pred_line_ammo <-  intercept_adjusted_ammo + (slope_original_ammo * (data_ammo$TEMPERATURE_VALUE_out))

#For scaled values
#pred_line_scaled_mang <- fixed_effects_mang["(Intercept)", "Estimate"] + (fixed_effects_mang["TEMPERATURE_VALUE_out", "Estimate"]*data_mang$TEMPERATURE_VALUE_out)

#pred_line_scaled_ammo <- fixed_effects_ammo["(Intercept)", "Estimate"] + (fixed_effects_ammo["TEMPERATURE_VALUE_out", "Estimate"]*data_ammo$TEMPERATURE_VALUE_out)

#Create R2.
r_squared_mang <- r.squaredGLMM(test_lmer_mang)[1, "R2c"]
print(r_squared_mang)

r_squared_ammo <- r.squaredGLMM(test_lmer_ammo)[1, "R2c"]
print(r_squared_ammo)


#-------- Confidence interval ----------
#From test_lmer_ammo we gain TEMPERATURE -0.19307 (estimate!)   0.09416 (SE!) 49.01697 (df!)  -2.051 (t value!) 0.0457 (p!).

# #Find critical t-value.
# t_crit_mang <- qt(0.975, df = fixed_effects_mang["TEMPERATURE_VALUE_out", "df"]) #2.009558
# t_crit_ammo <- qt(0.975, df = fixed_effects_ammo["TEMPERATURE_VALUE_out", "df"])  #1.98859
# 
# #Calculate standard error
# SE_mang <- fixed_effects_mang["TEMPERATURE_VALUE_out", "Std. Error"]#*sd(data_mang$TEMPERATURE_VALUE_out) 
# 
# SE_ammo <- fixed_effects_ammo["TEMPERATURE_VALUE_out", "Std. Error"]
# 
# #Potentially do it really simple.
# data_mang$lower_ci <- data_mang$pred_line_mang - t_crit_mang*SE_mang
# data_mang$upper_ci <- data_mang$pred_line_mang + t_crit_mang*SE_mang
# 
# data_ammo$lower_ci <- data_ammo$pred_line_ammo - t_crit_ammo*SE_ammo
# data_ammo$upper_ci <- data_ammo$pred_line_ammo + t_crit_ammo*SE_ammo

#Scaled values
#data_mang$scaled_lower_ci <- pred_line_scaled_mang - t_crit_mang*SE_mang
#data_mang$scaled_upper_ci <- pred_line_scaled_mang + t_crit_mang*SE_mang

#data_ammo$scaled_lower_ci <- pred_line_scaled_ammo - t_crit_mang*SE_ammo
#data_ammo$scaled_upper_ci <- pred_line_scaled_ammo + t_crit_mang*SE_ammo

#Real values vs. prediction with R-values.
ggplot() +
  geom_point(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3) +
  geom_smooth(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  #geom_line(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = pred_line_mang), color = "black", linewidth = 2) +
  #geom_ribbon(data = data_mang, aes(x = TEMPERATURE_VALUE_out, ymin = lower_ci, ymax = upper_ci), fill = "black", alpha = 0.2) +
  
  geom_point(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  #geom_line(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = pred_line_ammo), color = "black", linewidth = 2) +
  #geom_ribbon(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, ymin = lower_ci, ymax = upper_ci), fill = "black", alpha = 0.2) +
  
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(title = "TEMPERATURE",
       x = "Temperature [\u00B0C]",
       y = "Treatment Difference [mg/l]") +
  theme_minimal() +
  theme_minimal(base_size = 16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "none"))

# 
# #Plot all fixed effects with SE:
# intercept_adjusted_mang <- fixed_effects_mang["(Intercept)", "Estimate"] -
#     fixed_effects_mang["diff_pH", "Estimate"] * mean(data_mang$diff_pH) -
#     fixed_effects_mang["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_mang$TEMPERATURE_VALUE_out) -
#     fixed_effects_mang["YEAR", "Estimate"] * mean(data_mang$YEAR) -
#     fixed_effects_mang["PRECIPITATION", "Estimate"] * mean(data_mang$PRECIPITATION)
#   
# intercept_adjusted_ammo <- fixed_effects_ammo["(Intercept)", "Estimate"] -
#     fixed_effects_ammo["diff_pH", "Estimate"] * mean(data_ammo$diff_pH) -
#     fixed_effects_ammo["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_ammo$TEMPERATURE_VALUE_out) -
#     fixed_effects_ammo["YEAR", "Estimate"] * mean(data_ammo$YEAR) -
#     fixed_effects_ammo["PRECIPITATION", "Estimate"] * mean(data_ammo$PRECIPITATION)
#   
# 
# data_mang$pred_line_mang <- intercept_adjusted_mang +
#   fixed_effects_mang["diff_pH", "Estimate"] * data_mang$diff_pH +
#   fixed_effects_mang["TEMPERATURE_VALUE_out", "Estimate"] * data_mang$TEMPERATURE_VALUE_out +
#   fixed_effects_mang["YEAR", "Estimate"] * data_mang$YEAR +
#   fixed_effects_mang["PRECIPITATION", "Estimate"] * data_mang$PRECIPITATION
# 
# data_ammo$pred_line_ammo <- intercept_adjusted_ammo +
#   fixed_effects_ammo["diff_pH", "Estimate"] * data_ammo$diff_pH +
#   fixed_effects_ammo["TEMPERATURE_VALUE_out", "Estimate"] * data_ammo$TEMPERATURE_VALUE_out +
#   fixed_effects_ammo["YEAR", "Estimate"] * data_ammo$YEAR +
#   fixed_effects_ammo["PRECIPITATION", "Estimate"] * data_ammo$PRECIPITATION
# 
# 
# SE_mang <- sqrt(
#   fixed_effects_mang["diff_pH", "Std. Error"]^2 +
#     fixed_effects_mang["TEMPERATURE_VALUE_out", "Std. Error"]^2 +
#     fixed_effects_mang["YEAR", "Std. Error"]^2 +
#     fixed_effects_mang["PRECIPITATION", "Std. Error"]^2
# )
# 
# SE_ammo <- sqrt(
#   fixed_effects_ammo["diff_pH", "Std. Error"]^2 +
#     fixed_effects_ammo["TEMPERATURE_VALUE_out", "Std. Error"]^2 +
#     fixed_effects_ammo["YEAR", "Std. Error"]^2 +
#     fixed_effects_ammo["PRECIPITATION", "Std. Error"]^2
# )
# 
# 
# data_mang$lower_ci <- data_mang$pred_line_mang - t_crit_mang * SE_mang
# data_mang$upper_ci <- data_mang$pred_line_mang + t_crit_mang * SE_mang
# 
# data_ammo$lower_ci <- data_ammo$pred_line_ammo - t_crit_ammo * SE_ammo
# data_ammo$upper_ci <- data_ammo$pred_line_ammo + t_crit_ammo * SE_ammo
# 
# 
# ggplot() +
#   geom_point(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 4) +
#   geom_smooth(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "dashed", linewidth = 1, se = FALSE) +
#   geom_line(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = pred_line_mang), color = "black", linewidth = 2) +
#   geom_ribbon(data = data_mang, aes(x = TEMPERATURE_VALUE_out, ymin = lower_ci, ymax = upper_ci), fill = "black", alpha = 0.2) +
#   
#   geom_point(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 4, shape = 17) +
#   geom_smooth(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "dashed", linewidth = 1, se = FALSE) +
#   geom_line(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = pred_line_ammo), color = "black", linewidth = 2) +
#   geom_ribbon(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, ymin = lower_ci, ymax = upper_ci), fill = "black", alpha = 0.2) +
#   
#   facet_wrap(~ PARAMETER_ENG, scales = "free") +
#   labs(title = "Measured Values vs. Prediction Line",
#        x = "Temperature [\u00B0C]",
#        y = "Treatment Difference") +
#   theme_minimal() +
#   scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#56B4E9"))
# 



# #Plotting of manganese and ammonium-N:
# overview_mang <- ggplot(data_mang, aes(x = TEMPERATURE_VALUE_nonScaled, y = MEASURED_VALUE_in, color = LOCATION)) +
#   geom_line(linewidth = 1.5, linetype = "solid") + # Legg til linjer med tykkelse og stil
#   #geom_point(size = 3, shape = 16) +
#   geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1) +
#   labs(
#     title = "Treatment Difference of Manganese with Temperature",
#     x = "Leachate Temperature",  # Endret etikett for x
#     y = "Treatment Difference [mg/l] (in - out)",    # Endret etikett for y
#     color = "Location"
#   ) +
#   scale_color_manual(values = c("red", "blue")) +  # Spesifiser fargene du vil bruke
#   theme_minimal()
# 
# overview_ammo <- ggplot(data_ammo, aes(x = TEMPERATURE_VALUE_nonScaled, y = diff_MEASURED_VALUE, color = LOCATION)) +
#   geom_line(linewidth = 1.5, linetype = "solid") + # Legg til linjer med tykkelse og stil
#   #geom_point(size = 3, shape = 16) +
#   geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1) +
#   labs(
#     title = "Treatment Difference of Ammonium-N with Temperature",
#     x = "Leachate Temperature",  # Endret etikett for x
#     y = "Treatment Difference [mg/l] (in - out)",    # Endret etikett for y
#     color = "Location"
#   ) +
#   scale_color_manual(values = c("red", "blue")) +  # Spesifiser fargene du vil bruke
#   theme_minimal()
# 
# library(gridExtra)
# grid.arrange(overview_ammo, overview_mang, ncol = 1)


#Plot both with temperature

# Plot manganese
# plot_mang <- ggplot(data_mang, aes(x = TEMPERATURE_VALUE_nonScaled, y = diff_MEASURED_VALUE, color = LOCATION)) +
#   geom_line(size = 1.5, linetype = "solid") +
#   geom_point(size = 3, shape = 16) +
#   labs(
#     title = "Manganese Treatment Difference with Temperature",
#     x = "Leachate Temperature",  
#     y = "Treatment Difference [mg/l] (in - out)",    
#     color = "Location"
#   ) +
#   scale_color_manual(values = c("red", "blue")) +  # Spesifiser fargene
#   theme_minimal()

# Plot ammonium
#plot_ammo <- 
  
# ggplot() + 
#   geom_line(data = data_ammo, aes(x = TEMPERATURE_VALUE_nonScaled, y = MEASURED_VALUE_in), color = "red", linewidth = 1)+
#   geom_line(data = data_ammo, aes(x = TEMPERATURE_VALUE_nonScaled, y = MEASURED_VALUE_out), color = "blue", linewidth = 1)
# + geom_line(size = 1.5, linetype = "solid") +
#   geom_line(size = 1.5, linetype = "solid")+
#   #geom_point(size = 3, shape = 16) +
#   labs(
#     title = "Ammonium Treatment Difference with Temperature",
#     x = "Leachate Temperature",  
#     y = "Treatment Difference [mg/l] (in - out)",    
#     color = "Location"
#   ) +
#   scale_color_manual(values = c("red", "blue")) +
#   theme_minimal()
# 
# grid.arrange(plot_mang, plot_ammo, ncol = 1)

# 
# #Real values vs. prediction with R-values.
# ggplot() +
#   geom_point(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = diff_MEASURED_VALUE, color = factor(LOCATION)), size = 4) +
#   geom_line(data = data_mang, aes(x = TEMPERATURE_VALUE_out, y = pred_line_mang), color = "black", linewidth = 2) +
#   geom_ribbon(data = data_mang, aes(x = TEMPERATURE_VALUE_out, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.2) +
#   geom_point(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = diff_MEASURED_VALUE, color = factor(LOCATION)), size = 4, shape = 17) +
#   geom_line(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, y = pred_line_ammo), color = "black", linewidth = 2) +
#   geom_ribbon(data = data_ammo, aes(x = TEMPERATURE_VALUE_out, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.2) +
#   facet_wrap(~ PARAMETER_ENG, scales = "free", labeller = labeller(PARAMETER_ENG = function(x) {
#     sapply(x, function(param) {
#       if (param == "Manganese") {
#         return(bquote("Manganese" ~ 
#                         R^2 ~ "=" ~. (if(r_squared_mang < 1)
#                           (format(round(r_squared_mang, 3), nsmall = 3)))))
#       } else if (param == "Ammonium-N") {
#         return(bquote("Ammonium-N (R"^2 ~ "=" ~ .(format(round(r_squared_ammo, 3), nsmall = 3))))
#       }
#     })
#   })) +
#   labs(title = "Observed Values vs. Prediction Line",
#        x = "Temperature [\u00B0C]",
#        y = "Difference (in-out) in Treatment of Environmental Stressors [mg/l]") +
#   theme_minimal() +
#   scale_color_manual(values = c("Boelstad" = "blue", "Spillhaug" = "red")) #+
# #ylim(-5, NA)

#--------------------------------- The same, but for PRECIPITATION --------------------
library(dplyr)

#data_mang_ammo <- a[a$PARAMETER_ENG %in% c("Manganese", "Ammonium-N"), ]
data_mang2 <- a2[a2$PARAMETER_ENG %in% "Manganese", ] #YJ
data_chloride <- a2[a2$PARAMETER_ENG %in% "Chloride Ion (Cl)", ] #log
data_ammo2 <- a2[a2$PARAMETER_ENG %in% "Ammonium-N", ] #YJ
#data_chloride2 <- a[a$PARAMETER_ENG %in% "Chloride Ion (Cl)", ]  #For plotting later.

#yeojohnson and log transformation of diff_MEASURED_VALUE.
yeojohnson_obj_mang2 <- yeojohnson(data_mang2$diff_MEASURED_VALUE)

yeojohnson_obj_ammo2 <- yeojohnson(data_ammo2$diff_MEASURED_VALUE)

data_mang2$transformed_MEASURED_VALUE <- yeojohnson_obj_mang2$x.t
data_ammo2$transformed_MEASURED_VALUE <- yeojohnson_obj_ammo2$x.t

data_chloride$log_diff_MEASURED_VALUE <- log(data_chloride$diff_MEASURED_VALUE) #NaNs produced.

#Remove NaNs and "inf"-values for log transformations.
data_chloride <- data_chloride %>% filter(!(log_diff_MEASURED_VALUE == "-Inf") &
                          !(log_diff_MEASURED_VALUE == "NaN"))

#Duplicate column to keep non-scaled values
data_mang2$PRECIPITATION_nonScaled <- data_mang2$PRECIPITATION
data_mang2$diff_pH_nonScaled <- data_mang2$diff_pH
data_mang2$TEMPERATURE_nonScaled <- data_mang2$TEMPERATURE_VALUE_out
data_mang2$YEAR_nonScaled <- data_mang2$YEAR


data_ammo2$PRECIPITATION_nonScaled <- data_ammo2$PRECIPITATION
data_ammo2$diff_pH_nonScaled <- data_ammo2$diff_pH
data_ammo2$TEMPERATURE_nonScaled <- data_ammo2$TEMPERATURE_VALUE_out
data_ammo2$YEAR_nonScaled <- data_ammo2$YEAR

data_chloride$PRECIPITATION_nonScaled <- data_chloride$PRECIPITATION
data_chloride$diff_pH_nonScaled <- data_chloride$diff_pH
data_chloride$TEMPERATURE_nonScaled <- data_chloride$TEMPERATURE_VALUE_out
data_chloride$YEAR_nonScaled <- data_chloride$YEAR

#Scale variables, to make regression possible (due to various scales, e.g. degrees vs. pH)
data_mang2$diff_pH <- scale(data_mang2$diff_pH)
data_mang2$TEMPERATURE_VALUE_out <- scale(data_mang2$TEMPERATURE_VALUE_out)
data_mang2$PRECIPITATION <- scale(data_mang2$PRECIPITATION)
data_mang2$YEAR <- scale(data_mang2$YEAR)

data_ammo2$diff_pH <- scale(data_ammo2$diff_pH)
data_ammo2$TEMPERATURE_VALUE_out <- scale(data_ammo2$TEMPERATURE_VALUE_out)
data_ammo2$PRECIPITATION <- scale(data_ammo2$PRECIPITATION)
data_ammo2$YEAR <- scale(data_ammo2$YEAR)

data_chloride$diff_pH <- scale(data_chloride$diff_pH)
data_chloride$TEMPERATURE_VALUE_out <- scale(data_chloride$TEMPERATURE_VALUE_out)
data_chloride$PRECIPITATION <- scale(data_chloride$PRECIPITATION)
data_chloride$YEAR <- scale(data_chloride$YEAR)


#Lmer-models for environmental stressors.
test_lmer_mang2 <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_mang2)

test_lmer_ammo2 <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_ammo2)

test_lmer_chloride <- lmer(log_diff_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_chloride)


#Extract coefficients
fixed_effects_mang2 <- coef(summary(test_lmer_mang2))
fixed_effects_ammo2 <- coef(summary(test_lmer_ammo2))
fixed_effects_chloride <- coef(summary(test_lmer_chloride))


#Create an intercept, corrected for Yeo-Johnson. Since we scaled prior to running model, we correct with the scaled means! We do not divide by SD here, only correcting by multiplying with scaled mean. This gives the original value of y (intercept).
intercept_adjusted_mang2 <- fixed_effects_mang2["(Intercept)", "Estimate"]
# - fixed_effects_mang["diff_pH", "Estimate"] *mean(data_mang$diff_pH) -
#   fixed_effects_mang["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_mang$TEMPERATURE_VALUE_out) -
#   fixed_effects_mang["YEAR", "Estimate"] * mean(data_mang$YEAR) -
#  fixed_effects_mang["PRECIPITATION", "Estimate"] * mean(data_mang$PRECIPITATION)

intercept_adjusted_ammo2 <- fixed_effects_ammo2["(Intercept)", "Estimate"]
# - fixed_effects_ammo["diff_pH", "Estimate"] * mean(data_ammo$diff_pH) -
#  fixed_effects_ammo["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_ammo$TEMPERATURE_VALUE_out) -
#  fixed_effects_ammo["YEAR", "Estimate"] * mean(data_ammo$YEAR)-
# fixed_effects_ammo["PRECIPITATION", "Estimate"] * mean(data_ammo$PRECIPITATION)

intercept_adjusted_chloride <- fixed_effects_chloride["(Intercept)", "Estimate"] 
# - fixed_effects_chloride["diff_pH", "Estimate"] * mean(data_chloride$diff_pH) -
# fixed_effects_chloride["TEMPERATURE_VALUE_out", "Estimate"] * mean(data_chloride$TEMPERATURE_VALUE_out)-
# fixed_effects_chloride["YEAR", "Estimate"] * mean(data_chloride$YEAR)-
#  fixed_effects_chloride["PRECIPITATION", "Estimate"] * mean(data_chloride$PRECIPITATION)
 


#Create a slope, adjusted for Yeo Johnson and log, to convert to original data. Intercept needs Yeo Johnson transf + log as this is the y value!
#intercept_mang_pred <- predict(yeojohnson_obj_mang, newdata = intercept_adjusted_mang, inverse = TRUE)
#intercept_ammo_pred <- predict(yeojohnson_obj_ammo, newdata = intercept_adjusted_ammo, inverse = TRUE)

#intercept_chloride_pred <- exp(intercept_adjusted_chloride)

#Create slope. NB! Conducted on with scaled SD, to convert back to original data. No Yeo-Johnson transformation is needed for slope, as this is dependent on x-values. However, exp is nesseccary for chloride, as they are log transformed! <3

slope_adjusted_mang2 <- fixed_effects_mang2["PRECIPITATION", "Estimate"]#*sd£(data_mang$PRECIPITATION_nonScaled) exp/YJ.

slope_adjusted_ammo2 <- fixed_effects_ammo2["PRECIPITATION", "Estimate"]#*sd(data_ammo$PRECIPITATION_nonScaled)

slope_adjusted_chloride <- (fixed_effects_chloride["PRECIPITATION", "Estimate"])#*sd#data_chloride$PRECIPITATION_nonScaled

#Create prediction line
data_mang2$pred_line_mang <-  intercept_adjusted_mang2 + (slope_adjusted_mang2*(data_mang2$PRECIPITATION_nonScaled))

data_ammo2$pred_line_ammo <-  intercept_adjusted_ammo2 + (slope_adjusted_ammo2*(data_ammo2$PRECIPITATION_nonScaled))

data_chloride$pred_line_chloride <-  intercept_adjusted_chloride + slope_adjusted_chloride *(data_chloride$PRECIPITATION_nonScaled)


#Create R2.
r_squared_mang2 <- r.squaredGLMM(test_lmer_mang2)[1, "R2c"] #Denne vil være den samme for hele modellen for mangan. Denne vil derfor ikke endres for precipitation eller temperature. Location har mye å si!
print(r_squared_mang2)

r_squared_ammo2 <- r.squaredGLMM(test_lmer_ammo2)[1, "R2c"] #Denne vil være den samme for hele modellen for mangan. Denne vil derfor ikke endres for precipitation eller temperature. Location har mye å si!
print(r_squared_ammo2)

r_squared_chloride <- r.squaredGLMM(test_lmer_chloride)[1, "R2c"] #Location har mye å si!
print(r_squared_chloride)


#-------- Confidence interval ----------
#From test_lmer_ammo we gain PRECIPITATION -0.21910 (estimate!)   0.08248 (SE!) 84.02031 (df!)  -2.657 (t value!) 0.00944 (p!).
# 
# #Find critical t-value.
# t_crit_mang <- qt(0.975, df = fixed_effects_mang["PRECIPITATION", "df"])
# t_crit_ammo <- qt(0.975, df = fixed_effects_ammo["PRECIPITATION", "df"])
# t_crit_chloride <- qt(0.975, df = fixed_effects_chloride["PRECIPITATION", "df"])
# 
# #Convert SE (scaled) to SE (original). Formula: SE (original) = SE (scaled) * SD (original)
# SE_mang <- fixed_effects_mang["PRECIPITATION", "Std. Error"]#*sd(data_mang$PRECIPITATION_nonScaled)
# #SE_mang_original <- predict(yeojohnson_obj_mang, newdata = SE_mang, inverse = TRUE) #Yeo-Johnson
# 
# SE_ammo <- fixed_effects_ammo["PRECIPITATION", "Std. Error"]#*sd(data_ammo$PRECIPITATION_nonScaled) 
# #SE_ammo_original <- predict(yeojohnson_obj_mang, newdata = SE_ammo, inverse = TRUE) #Yeo-Johson
# 
# SE_chloride <- fixed_effects_chloride["PRECIPITATION", "Std. Error"]#*sd(data_chloride$PRECIPITATION_nonScaled) 
# #SE_chloride_original <- exp(SE_chloride) #Log transform
# 
# #Manually calculate confidence interval
# data_mang$lower_ci <- data_mang$pred_line_mang - t_crit_mang*SE_mang
# data_mang$upper_ci <- data_mang$pred_line_mang + t_crit_mang*SE_mang
# 
# data_ammo$lower_ci <- data_ammo$pred_line_ammo - t_crit_ammo*SE_ammo
# data_ammo$upper_ci <- data_ammo$pred_line_ammo + t_crit_ammo*SE_ammo
# 
# data_chloride$lower_ci <- data_chloride$pred_line_chloride - t_crit_chloride*SE_chloride
# data_chloride$upper_ci <- data_chloride$pred_line_chloride + t_crit_chloride*SE_chloride


#Plot PRECIPITATION
ggplot() +
  geom_point(data = data_mang2, aes(x = PRECIPITATION, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3) +
  geom_smooth(data = data_mang2, aes(x = PRECIPITATION, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  geom_point(data = data_chloride, aes(x = PRECIPITATION, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_chloride, aes(x = PRECIPITATION, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  geom_point(data = data_ammo2, aes(x = PRECIPITATION, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 18) +
  geom_smooth(data = data_ammo2, aes(x = PRECIPITATION, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(title = "PRECIPITATION",
       x = "Precipitation [mm/24 h]",
       y = "Treatment Difference [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "none"))


#------------------------------pH -----------------------------

#data
data_acen <- a2[a2$PARAMETER_ENG %in% "Acenaphthene", ] #Yeo Johnson
data_fluor <- a2[a2$PARAMETER_ENG %in% "Fluorene", ] #Yeo Johnson
data_iron <- a2[a2$PARAMETER_ENG %in% "Iron", ] #Yeo Johnson

#yeojohnson and log transformation of diff_MEASURED_VALUE.
yeojohnson_obj_acen <- yeojohnson(data_acen$diff_MEASURED_VALUE)
yeojohnson_obj_fluor <- yeojohnson(data_fluor$diff_MEASURED_VALUE)
yeojohnson_obj_iron <- yeojohnson(data_iron$diff_MEASURED_VALUE)


data_acen$transformed_MEASURED_VALUE <- yeojohnson_obj_acen$x.t
data_fluor$transformed_MEASURED_VALUE <- yeojohnson_obj_fluor$x.t
data_iron$transformed_MEASURED_VALUE <- yeojohnson_obj_iron$x.t



#Duplicate column to keep non-scaled values
data_acen$PRECIPITATION_nonScaled <- data_acen$PRECIPITATION
data_acen$diff_pH_nonScaled <- data_acen$diff_pH
data_acen$TEMPERATURE_nonScaled <- data_acen$TEMPERATURE_VALUE_out
data_acen$YEAR_nonScaled <- data_acen$YEAR

data_fluor$PRECIPITATION_nonScaled <- data_fluor$PRECIPITATION
data_fluor$diff_pH_nonScaled <- data_fluor$diff_pH
data_fluor$TEMPERATURE_nonScaled <- data_fluor$TEMPERATURE_VALUE_out
data_fluor$YEAR_nonScaled <- data_fluor$YEAR

data_iron$PRECIPITATION_nonScaled <- data_iron$PRECIPITATION
data_iron$diff_pH_nonScaled <- data_iron$diff_pH
data_iron$TEMPERATURE_nonScaled <- data_iron$TEMPERATURE_VALUE_out
data_iron$YEAR_nonScaled <- data_iron$YEAR

#Scale variables, to make regression possible (due to various scales, e.g. degrees vs. pH)
data_acen$diff_pH <- scale(data_acen$diff_pH)
data_acen$TEMPERATURE_VALUE_out <- scale(data_acen$TEMPERATURE_VALUE_out)
data_acen$PRECIPITATION <- scale(data_acen$PRECIPITATION)
data_acen$YEAR <- scale(data_acen$YEAR)

data_fluor$diff_pH <- scale(data_fluor$diff_pH)
data_fluor$TEMPERATURE_VALUE_out <- scale(data_fluor$TEMPERATURE_VALUE_out)
data_fluor$PRECIPITATION <- scale(data_fluor$PRECIPITATION)
data_fluor$YEAR <- scale(data_fluor$YEAR)

data_iron$diff_pH <- scale(data_iron$diff_pH)
data_iron$TEMPERATURE_VALUE_out <- scale(data_iron$TEMPERATURE_VALUE_out)
data_iron$PRECIPITATION <- scale(data_iron$PRECIPITATION)
data_iron$YEAR <- scale(data_iron$YEAR)


#Lmer-models for environmental stressors.
test_lmer_acen <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_acen)

test_lmer_fluor <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_fluor)

test_lmer_iron <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_iron)


#Extract coefficients
fixed_effects_acen <- coef(summary(test_lmer_acen))
fixed_effects_fluor <- coef(summary(test_lmer_fluor))
fixed_effects_iron <- coef(summary(test_lmer_iron))

#Create an intercept, corrected for Yeo-Johnson.
intercept_adjusted_acen <- fixed_effects_acen["(Intercept)", "Estimate"]

intercept_adjusted_fluor <- fixed_effects_fluor["(Intercept)", "Estimate"]

intercept_adjusted_iron <- fixed_effects_iron["(Intercept)", "Estimate"]
  
#Create slope.
slope_adjusted_acen <- fixed_effects_acen["diff_pH", "Estimate"]#*sd(data_acen$diff_pH_nonScaled)
slope_adjusted_fluor <- fixed_effects_fluor["diff_pH", "Estimate"]#*sd(data_fluor$diff_pH_nonScaled)
slope_adjusted_iron <- fixed_effects_iron["diff_pH", "Estimate"]#*sd(data_iron$diff_pH_nonScaled)


#Create prediction line
data_acen$pred_line_acen <-  intercept_adjusted_acen + (slope_adjusted_acen*(data_acen$diff_pH))
data_fluor$pred_line_fluor <-  intercept_adjusted_fluor + (slope_adjusted_fluor*(data_fluor$diff_pH))
data_iron$pred_line_iron <-  intercept_adjusted_iron + (slope_adjusted_iron*(data_iron$diff_pH))

library(MuMIn)
#Create R2.
r_squared_acen <- r.squaredGLMM(test_lmer_acen)[1, "R2m"]
print(r_squared_acen)

r_squared_fluor <- r.squaredGLMM(test_lmer_fluor)[1, "R2m"]
print(r_squared_fluor)

r_squared_iron <- r.squaredGLMM(test_lmer_iron)[1, "R2m"]
print(r_squared_iron)


#Plot pH
ggplot() +
  geom_point(data = data_acen, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3) +
  geom_smooth(data = data_acen, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  geom_point(data = data_fluor, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_fluor, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  geom_point(data = data_iron, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 18) +
  geom_smooth(data = data_iron, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(title = "pH",
       x = "pH Difference (in-out)",
       y = "Treatment Difference [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "none"))





#--------------------#YEAR------------------------------------------


#data_mang_ammo <- a[a$PARAMETER_ENG %in% c("Manganese", "Ammonium-N"), ]
data_acen2 <- a2[a2$PARAMETER_ENG %in% "Acenaphthene", ] #Yeo-Johnson
data_chrome2 <- a2[a2$PARAMETER_ENG %in% "Chromium", ] #log
data_nitro <- a2[a2$PARAMETER_ENG %in% "Nitrogen", ] #log

# data_chrome2 <- a[a$PARAMETER_ENG %in% "Chromium", ] #For plotting
# data_nitro2 <- a[a$PARAMETER_ENG %in% "Nitrogen", ]

#yeojohnson and log transformation of diff_MEASURED_VALUE.
yeojohnson_obj_acen2 <- yeojohnson(data_acen2$diff_MEASURED_VALUE)
data_acen2$transformed_MEASURED_VALUE <- yeojohnson_obj_acen2$x.t

data_chrome2$log_diff_MEASURED_VALUE <- log(data_chrome2$diff_MEASURED_VALUE) #NaNs produced.
data_nitro$log_diff_MEASURED_VALUE <- log(data_nitro$diff_MEASURED_VALUE) #NaNs produced.


#Remove NaNs and "inf"-values for log transformations.
data_chrome2 <- data_chrome2 %>% filter(!(log_diff_MEASURED_VALUE == "-Inf") &
                                      !(log_diff_MEASURED_VALUE == "NaN"))

data_nitro <- data_nitro %>% filter(!(log_diff_MEASURED_VALUE == "-Inf") &
                                            !(log_diff_MEASURED_VALUE == "NaN"))


#Duplicate column to keep non-scaled values
data_acen2$PRECIPITATION_nonScaled <- data_acen2$PRECIPITATION
data_acen2$diff_pH_nonScaled <- data_acen2$diff_pH
data_acen2$TEMPERATURE_nonScaled <- data_acen2$TEMPERATURE_VALUE_out
data_acen2$YEAR_nonScaled <- data_acen2$YEAR

data_chrome2$PRECIPITATION_nonScaled <- data_chrome2$PRECIPITATION
data_chrome2$diff_pH_nonScaled <- data_chrome2$diff_pH
data_chrome2$TEMPERATURE_nonScaled <- data_chrome2$TEMPERATURE_VALUE_out
data_chrome2$YEAR_nonScaled <- data_chrome2$YEAR

data_nitro$PRECIPITATION_nonScaled <- data_nitro$PRECIPITATION
data_nitro$diff_pH_nonScaled <- data_nitro$diff_pH
data_nitro$TEMPERATURE_nonScaled <- data_nitro$TEMPERATURE_VALUE_out
data_nitro$YEAR_nonScaled <- data_nitro$YEAR


#Scale variables, to make regression possible (due to various scales, e.g. degrees vs. pH)
data_acen2$diff_pH <- scale(data_acen2$diff_pH)
data_acen2$TEMPERATURE_VALUE_out <- scale(data_acen2$TEMPERATURE_VALUE_out)
data_acen2$PRECIPITATION <- scale(data_acen2$PRECIPITATION)
data_acen2$YEAR <- scale(data_acen2$YEAR)

data_chrome2$diff_pH <- scale(data_chrome2$diff_pH)
data_chrome2$TEMPERATURE_VALUE_out <- scale(data_chrome2$TEMPERATURE_VALUE_out)
data_chrome2$PRECIPITATION <- scale(data_chrome2$PRECIPITATION)
data_chrome2$YEAR <- scale(data_chrome2$YEAR)

data_nitro$diff_pH <- scale(data_nitro$diff_pH)
data_nitro$TEMPERATURE_VALUE_out <- scale(data_nitro$TEMPERATURE_VALUE_out)
data_nitro$PRECIPITATION <- scale(data_nitro$PRECIPITATION)
data_nitro$YEAR <- scale(data_nitro$YEAR)

#Lmer-models for environmental stressors.
test_lmer_acen2 <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_acen2)

test_lmer_chrome2 <- lmer(log_diff_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_chrome2)

test_lmer_nitro <- lmer(log_diff_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (1|LOCATION), data = data_nitro)

#Extract coefficients
fixed_effects_acen2 <- coef(summary(test_lmer_acen2))
fixed_effects_chrome2 <- coef(summary(test_lmer_chrome2))
fixed_effects_nitro <- coef(summary(test_lmer_nitro))

#Create an intercept, corrected for Yeo-Johnson. Since we scaled prior to running model, we correct with the scaled means! We do not divide by SD here, only correcting by multiplying with scaled mean. This gives the original value of y (intercept).
intercept_adjusted_acen2 <- fixed_effects_acen2["(Intercept)", "Estimate"]

intercept_adjusted_chrome2 <- fixed_effects_chrome2["(Intercept)", "Estimate"]

intercept_adjusted_nitro <- fixed_effects_nitro["(Intercept)", "Estimate"]


slope_adjusted_acen2 <- (fixed_effects_acen2["YEAR", "Estimate"])
slope_adjusted_chrome2 <- (fixed_effects_chrome2["YEAR", "Estimate"])
slope_adjusted_nitro <- (fixed_effects_nitro["YEAR", "Estimate"])


#Create prediction line
data_acen2$pred_line_acen <-  intercept_adjusted_acen2 + (slope_adjusted_acen2*(data_acen2$YEAR))
data_chrome2$pred_line_chrome <-  intercept_adjusted_chrome2 + (slope_adjusted_chrome2*(data_chrome2$YEAR)) #Her skal man ha skalerte år? Stemmer det?
data_nitro$pred_line_nitro <-  intercept_adjusted_nitro + (slope_adjusted_nitro*(data_nitro$YEAR))

#Create R2.
r_squared_acen2 <- r.squaredGLMM(test_lmer_acen2)[1, "R2m"] #Denne vil være den samme for hele modellen for mangan. Denne vil derfor ikke endres for precipitation eller temperature. Location har mye å si!
print(r_squared_acen2)

r_squared_chrome2 <- r.squaredGLMM(test_lmer_chrome2)[1, "R2m"] #Denne vil være den samme for hele modellen for mangan. Denne vil derfor ikke endres for precipitation eller temperature. Location har mye å si!
print(r_squared_chrome2)

r_squared_nitro <- r.squaredGLMM(test_lmer_nitro)[1, "R2m"] #Location har mye å si!
print(r_squared_nitro)


#-------- Confidence interval ----------
#From symamry(lmer) we gain YEAR -0.21910 (estimate!)   0.08248 (SE!) 84.02031 (df!)  -2.657 (t value!) 0.00944 (p!).

#Find critical t-value.
# t_crit_acen <- qt(0.975, df = fixed_effects_acen["YEAR", "df"])
# t_crit_chrome <- qt(0.975, df = fixed_effects_chrome["YEAR", "df"])
# t_crit_nitro <- qt(0.975, df = fixed_effects_nitro["YEAR", "df"])

# 
# #Convert SE (scaled) to SE (original). Formula: SE (original) = SE (scaled) * SD (original)
# SE_acen <- fixed_effects_acen["YEAR", "Std. Error"]*sd(data_acen$YEAR_nonScaled)
# SE_acen_original <- predict(yeojohnson_obj_acen, newdata = SE_acen, inverse = TRUE) #Yeo-Johnson
# 
# SE_chrome <- fixed_effects_chrome["YEAR", "Std. Error"]*sd(data_chrome$YEAR_nonScaled) #Log
# SE_chrome_original <- exp(SE_chrome)
# 
# SE_nitro <- fixed_effects_nitro["YEAR", "Std. Error"]*sd(data_nitro$YEAR_nonScaled) #Log transform
# SE_nitro_original <- exp(SE_nitro)

# 
# 
# #Manually calculate confidence interval
# data_acen$lower_ci <- data_acen$pred_line_acen - t_crit_acen*SE_acen_original
# data_acen$upper_ci <- data_acen$pred_line_acen + t_crit_acen*SE_acen_original
# 
# data_chrome$lower_ci <- data_chrome$pred_line_chrome - t_crit_chrome*SE_chrome_original
# data_chrome$upper_ci <- data_chrome$pred_line_chrome + t_crit_chrome*SE_chrome_original
# 
# data_nitro$lower_ci <- data_nitro$pred_line_nitro - t_crit_nitro*SE_nitro_original
# data_nitro$upper_ci <- data_nitro$pred_line_nitro + t_crit_nitro*SE_nitro_original
# 
# data_PAH$lower_ci <- data_PAH$pred_line_PAH - t_crit_PAH*SE_PAH_original
# data_PAH$upper_ci <- data_PAH$pred_line_PAH + t_crit_PAH*SE_PAH_original


#Plot YEAR
year <- ggplot() +
  geom_point(data = data_acen2, aes(x = YEAR, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3) +
  geom_smooth(data = data_acen2, aes(x = YEAR, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  geom_point(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  geom_point(data = data_nitro, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 18) +
  geom_smooth(data = data_nitro, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(title = "YEAR",
       x = "Year",
       y = "Treatment Difference [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "none"))


#pH vs. Iron
ggplot() +
  geom_point(data = data_iron, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 18) +
  geom_smooth(data = data_iron, aes(x = diff_pH, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE) +
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(title = "pH",
       x = "pH Difference, scaled (in-out)",
       y = "Treatment Difference, Yeo-Johnson transformed [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
        theme(legend.position = "none")

#Year vs. Chrome
ggplot() +
  geom_point(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE)+
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(title = "YEAR",
       x = "Year, scaled",
       y = "Treatment Difference. log transformed [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
        theme(legend.position = "none")


#Plot graphs with ribbons

data_iron_plot <- a[a$PARAMETER_ENG %in% "Iron", ]
data_chrome_plot <- a[a$PARAMETER_ENG %in% "Chromium", ]

#In values of pH have decreased (acified with time). This leads to an increase of dissolved iron for both landfills.
#plot(data_iron_plot$pH_in, data_iron_plot$MEASURED_VALUE_in, col = as.factor(data_iron_plot$LOCATION))
#plot(data_iron_plot$pH_out, data_iron_plot$MEASURED_VALUE_out, col = as.factor(data_iron_plot$LOCATION))

#plot(data_iron_plot$diff_pH, data_iron_plot$diff_MEASURED_VALUE, col = as.factor(data_iron_plot$LOCATION))

ggplot() +
  geom_point(data = data_iron_plot, aes(x = diff_pH, y = diff_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_iron_plot, aes(x = diff_pH, y = diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE)+
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment Difference of Iron with pH",
       x = "Difference in pH, nonscaled  (in-out)",
       y = "Treatment Difference, nonscaled [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        #panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  theme(legend.position = "none")

data_chrome_plot <- a[a$PARAMETER_ENG %in% "Chromium", ]

#Measured_value_in of chrome has decreased with time. Out values are relatively stable. Less chrome in landfill.
plot(data_chrome_plot$YEAR, data_chrome_plot$MEASURED_VALUE_in, col = as.factor(data_chrome_plot$LOCATION))
plot(data_chrome_plot$YEAR, data_chrome_plot$MEASURED_VALUE_out, col = as.factor(data_chrome_plot$LOCATION))

plot(data_chrome_plot$YEAR, data_chrome_plot$diff_MEASURED_VALUE, col = as.factor(data_chrome_plot$LOCATION))

ggplot() +
  geom_point(data = data_chrome_plot, aes(x = YEAR, y = diff_MEASURED_VALUE, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_chrome_plot, aes(x = YEAR, y = diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE)+
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment Difference of Iron with pH",
    x = "Difference in year, nonscaled",
    y = "Treatment Difference, nonscaled [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        #panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  theme(legend.position = "none")



ggplot() +
  geom_point(data = data_chrome_plot, aes(x = YEAR, y = MEASURED_VALUE_in, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_chrome_plot, aes(x = YEAR, y = MEASURED_VALUE_in, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE)+
  #geom_line(data = data_ammo, aes(x = PRECIPITATION, y = pred_line_ammo), color = "black")+
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment Difference of Iron with pH",
    x = "Difference in year, nonscaled",
    y = "Treatment Value IN, nonscaled [mg/l]") +
  scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        #panel.spacing = unit(2.3, "lines"), #spacing between panels
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  theme(legend.position = "none")

ggplot() +
   geom_point(data = data_iron_plot, aes(x = pH_in, y = MEASURED_VALUE_in, color = factor(LOCATION)), size = 3.3, shape = 17) +
  geom_smooth(data = data_iron_plot, aes(x = pH_in, y = MEASURED_VALUE_in, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 3.3, se = FALSE)+
   facet_wrap(~ PARAMETER_ENG, scales = "free") +
   labs(#title = "Treatment Difference of Iron with pH",
     x = "pH_in, nonscaled",
     y = "Value IN, nonscaled [mg/l]") +
   scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(), #blank background
#         panel.grid.minor = element_blank(), #blank background
#         #panel.spacing = unit(2.3, "lines"), #spacing between panels
#         plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
#   theme(legend.position = "none")






#------------------------------------


#PLOT FIXED AND RANDOM EFFECTS AND TEST LMER MODELS FOR ACCURACY


library(lme4)
library(MuMIn)
library(performance)
library(partR2)
library(sjPlot)
library(DHARMa)
library(sjmisc)
library(ggeffects)
library(ggplot2)
library(tibble)
library(arm)
library(viridis)
library(dplyr)
library(tidyverse)

#Checking model for iron:


fixef(test_lmer_iron) #Extract fixed effects for lmer model
ranef(test_lmer_iron)
se.fixef(test_lmer_iron) #Extract standard error from lmer model
se.ranef(test_lmer_iron)

confint(test_lmer_iron, method = "profile") #Extract conf. interval from lmer model.

#After  confint(test_lmer_iron, method = "profile")
# 2.5 %       97.5 %
#   .sig01                 0.000000000  1.086444740 #SD for random intercept
# .sigma                 0.715161919  0.995695331 #SD for residuals.
# (Intercept)           -0.746363104  0.516374813
# diff_pH               -0.584965241 -0.006926969
# TEMPERATURE_VALUE_out -0.002244692  0.409289200
# YEAR                  -0.115859887  0.483426810
# PRECIPITATION         -0.281802434  0.120966256

#Check if random variable should be included:

# ranova(test_lmer_iron)
# ranova(test_lmer_mang)
# ranova(test_lmer_chrome2)
# ranova(test_lmer_chloride)
# ranova(test_lmer_ammo)
# ranova(test_lmer_fluor)
# ranova(test_lmer_nitro)



#rand(test_lmer_iron, reduce.terms = TRUE)


r2(test_lmer_iron) #Quick calculation of Rm and Rc.
icc(test_lmer_iron)
#icc(test_lmer_chrome2)


plot_model(test_lmer_iron, type = "re") #How much do location varies from the mean (group mean?). Spillhaug draws the model in a negative direction and Boelstad in a positive.
plot_model(test_lmer_iron, type = "est") #Plot fixed effects with confidence intervals.


check_model(test_lmer_iron) #Wow! Plots everything, all at once.
#check_model(test_lmer_chrome2)

#Since both test_lmer_iron and _chrome have a small random effect, the models may be compared to a lm-model:

lm_iron <- lm(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION , data = data_iron)

lm_chrome <- lm(log_diff_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION, data = data_chrome2)

summary(lm_iron) #Adjusted R squared adjusts for the number of predictors in the regression model, as R2 always increases as you add more predictors to a model.

#lmer_iron_comp <- lmer(transformed_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (PRECIPITATION|LOCATION), data = data_iron) #No nested-in structure was preferred by the anova for any of the fixed effects.

#lmer_chrome2_comp <- lmer(log_diff_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION + (PRECIPITATION|LOCATION), data = data_chrome2) #No nested-in structure was preferred by the anova for any of the fixed effects.

#Compare models using AIC
AIC(lm_iron, test_lmer_iron) #Both AIC prefers the linear model without the random effect as the best fit.
AIC(lm_chrome, test_lmer_chrome2)


#Plot fixed effect with confidence interval.
plot_model(test_lmer_iron, type = "eff", terms = "TEMPERATURE_VALUE_out")
#Add points and R2?

#Extract random intercepts for plots
ranef(test_lmer_iron)

#Convert to df
ranef_iron_df <- as.data.frame(ranef(test_lmer_iron))

#Inspect structure of df.
str(ranef_iron_df)

#PLOT RANDOM EFFECTS AND FIXED EFFECT FOR TEMPERATURE



# Fixed effects og SE for jern (iron)
summary_mod <- summary(test_lmer_iron)
fixef_vals <- fixef(test_lmer_iron)

intercept_fixed <- fixef_vals["(Intercept)"]
slope_fixed_temp <- fixef_vals["TEMPERATURE_VALUE_out"]

se_intercept_fixed <- coef(summary_mod)["(Intercept)", "Std. Error"]
se_slope_fixed_temp <- coef(summary_mod)["TEMPERATURE_VALUE_out", "Std. Error"]

# Temperature values on the x axis.
x_vals <- seq(min(data_iron$TEMPERATURE_VALUE_out), max(data_iron$TEMPERATURE_VALUE_out), length.out = 100) 

#Create lines and SE.
y_pred_fixed <- intercept_fixed + slope_fixed_temp * x_vals
se_pred_fixed_temp <- sqrt(se_intercept_fixed^2 + (x_vals^2)*(se_slope_fixed_temp^2))
ci_upper_fixed_temp <- y_pred_fixed + 1.96 * se_pred_fixed_temp
ci_lower_fixed_temp <- y_pred_fixed - 1.96 * se_pred_fixed_temp

#resid_sd <- sigma(test_lmer_iron) #Standard deviation for the residuals (what the model does not explain).
#0.1169/(0.1169+0.7394) #Explains how much variance LOCATION explains out of the residuals for the model. For iron = 13.6%.

# Random intercepts per LOCATION
rand_ints <- ranef(test_lmer_iron, condVar = TRUE)$LOCATION %>% 
  rownames_to_column("LOCATION") %>% 
  rename(rand_intercept = "(Intercept)") %>%
  mutate(
    rand_intercept_sd = sqrt(attr(ranef(test_lmer_iron, condVar = TRUE)$LOCATION, "postVar")[1, 1, ]))

# Create df with predictions and confidence interval per location with fixed effects NB! Since the line of lLOCATION includes the estimation of fixed effects, we also need to include SE for fixed effects when calculating total Standard Error for LOCATION.

lines_df_random <- rand_ints %>%
  rowwise () %>% #Do the following operation per row.
  
  mutate(
    data_rand = list(tibble(
      x = x_vals,
      y = (intercept_fixed + rand_intercept) + (x_vals*slope_fixed_temp),
      
      se_total = sqrt(
        se_intercept_fixed^2 + (x_vals^2 * se_slope_fixed_temp^2) + rand_intercept_sd^2),
      
      ci_upper = y  + (1.96 * se_total),
      ci_lower = y  - (1.96 * se_total)))) %>%
      unnest(data_rand) %>%
      ungroup() %>%
      select(LOCATION, x, y, ci_upper, ci_lower)

#Create line for fixed effects. NB! No location needed for this, as fixed are measured for both.

lines_df_fixed <- tibble(
  LOCATION = "Fixed",
  x = x_vals,
  y = y_pred_fixed,
  ci_upper = ci_upper_fixed_temp,
  ci_lower = ci_lower_fixed_temp)
      
#Combine lines. NB! Fixed is now named "Fixed" in LOCATION.
lines_total <- bind_rows(lines_df_random, lines_df_fixed)

#Extract residuals for plotting.

data_iron <- data_iron %>%
  ungroup() %>%
  mutate(residuals = residuals(test_lmer_iron))


#--------

# R2-values
r2_vals <- r.squaredGLMM(test_lmer_iron) %>%
  { paste0("Marginal R^2 = ", round(.[1], 3), ", Conditional R^2 = ", round(.[2], 3)) }

# Plot
  ggplot(lines_total,aes(x = x, y = y, color = LOCATION, fill = LOCATION)) +
  
    #geom_ribbon(data = subset(lines_total, LOCATION == "Fixed"), aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.5, color = NA) +

    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.5, color = NA) + #For all 95% confidence intervals.
    
  geom_line(data = subset(lines_total, LOCATION != "Fixed"), linewidth = 2, color = "black") +
  geom_line(data = subset(lines_total, LOCATION == "Fixed"), color = "black", linewidth = 2, linetype = "dashed") +
  
    geom_point(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = LOCATION), alpha = 0.4) + #Display raw data
    
#geom_point(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = residuals, color = LOCATION), alpha = 0.4) + #Display residuals of the model.
    
    
  labs(
    title = "Temperature and Iron at a 95% confidence interval",
    subtitle = r2_vals,
    x = "Temperature (scaled)",
    y = "Fitted model with Residuals",
    color = "LOCATION",
    fill = "LOCATION"
  ) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


#---------------------------------------------

#FOR CHROMIUM


fixef(test_lmer_chrome2) #Extract fixed effects for lmer model
ranef(test_lmer_chrome2)
se.fixef(test_lmer_chrome2) #Extract standard error from lmer model
se.ranef(test_lmer_chrome2)

confint(test_lmer_chrome2, method = "profile") #Extract conf. interval from lmer model.


#After  confint(test_lmer_iron, method = "profile")
#                          2.5 %      97.5 %
#.sig01                 0.0000000  0.51647558
#.sigma                 0.3953835  0.61446308
#(Intercept)           -6.7949626 -6.48692970
#diff_pH               -0.2605010  0.12085192
#TEMPERATURE_VALUE_out -0.2906473  0.03084104
##YEAR                  -0.4800924 -0.08954202
#PRECIPITATION         -0.2105761  0.10256283

#Check if random variable should be included:

#ranova(test_lmer_chrome2) #Less clear than for iron, but pretty similar. Go with easier version, hence without random factor.


r2(test_lmer_chrome2) #Quick calculation of Rm and Rc. Rm = for fixed effects alone. Rc = when random effects are included.
#icc(test_lmer_iron) #Almost similar to Rc-Rm.



plot_model(test_lmer_chrome2, type = "re") #How much do location varies from the mean (group mean?). Spillhaug draws the model in a negative direction and Boelstad in a positive.
plot_model(test_lmer_chrome2, type = "est") #Plot fixed effects with confidence intervals.


check_model(test_lmer_chrome2) #Wow! Plots everything, all at once.

#Since both test_lmer_iron and _chrome have a small random effect, the models may be compared to a lm-model:

lm_chrome <- lm(log_diff_MEASURED_VALUE ~ diff_pH + TEMPERATURE_VALUE_out + YEAR + PRECIPITATION, data = data_chrome2)

#Compare models using AIC
AIC(lm_chrome, test_lmer_chrome2)
#df      AIC
#lm_chrome          6 67.64453
#test_lmer_chrome2  7 84.85203

#Plot fixed effect with confidence interval.
plot_model(test_lmer_chrome2, type = "eff", terms = "YEAR")
#Add points and R2?

#Extract random intercepts for plots
ranef(test_lmer_chrome2)


#PLOT RANDOM EFFECTS AND FIXED EFFECT FOR MATURITY



# Fixed effects og SE for jern (iron)
summary_mod <- summary(test_lmer_chrome2)
fixef_vals <- fixef(test_lmer_chrome2)
intercept_fixed <- fixef_vals["(Intercept)"]
slope_temp <- fixef_vals["YEAR"]

se_intercept <- coef(summary_mod)["(Intercept)", "Std. Error"]
se_slope <- coef(summary_mod)["YEAR", "Std. Error"]

resid_sd <- sigma(test_lmer_chrome2) #Standard Deviation for residuals! What the model doesn't explain.

# Random intercepts per LOCATION
rand_ints <- ranef(test_lmer_chrome2)$LOCATION %>% 
  rownames_to_column("LOCATION") %>% 
  rename(rand_intercept = `(Intercept)`)

# Year on x-axis. This functions smooths the lines, so we get a smooth line.
x_vals <- seq(min(data_chrome2$YEAR), max(data_chrome2$YEAR), length.out = 100) 

# Create df with predictions and confidence interval per location with fixed effects. 

lines_df <- rand_ints %>% #Create plot and SE for random variables.
  group_by(LOCATION) %>% 
  do({
    intercept <- intercept_fixed + .$rand_intercept
    y_pred <- intercept + slope_temp * x_vals
    
    se_pred <- sqrt(se_intercept^2 + (x_vals^2)*(se_slope^2)) #+ resid_sd^2)
    ci_upper <- y_pred + 1.96 * se_pred
    ci_lower <- y_pred - 1.96 * se_pred
    
    data.frame(
      LOCATION = .$LOCATION,
      x = x_vals,
      y = y_pred,
      ci_upper = ci_upper,
      ci_lower = ci_lower
    )
  }) %>% 
  ungroup() %>%  #Create plot and SE for fixed variables.
  bind_rows(
    data.frame(
      LOCATION = "Fixed",
      x = x_vals,
      y = intercept_fixed + slope_temp * x_vals,
      ci_upper = intercept_fixed + slope_temp * x_vals + 1.96 * sqrt(se_intercept^2 + (x_vals^2)*(se_slope^2)), #+ resid_sd^2),
      ci_lower = intercept_fixed + slope_temp * x_vals - 1.96 * sqrt(se_intercept^2 + (x_vals^2)*(se_slope^2)) #+ resid_sd^2)
    )
  )

# R2-values
r2_vals <- r.squaredGLMM(test_lmer_chrome2)
r2_text <- paste0("Marginal R^2 = ", round(r2_vals[1], 3), 
                  ", Conditional R^2 = ", round(r2_vals[2], 3))

# Plot
lines_df %>%
  ggplot(aes(x = x, y = y, color = LOCATION, fill = LOCATION)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.4, color = NA) +
  
  
  #geom_ribbon(data = subset(lines_df, LOCATION == "Fixed"),
  #aes(ymin = ci_lower, ymax = ci_upper),
  #alpha = 0.2, fill = "black", color = NA) +
  
  geom_line(data = subset(lines_df, LOCATION != "Fixed"), linewidth = 2) +
  geom_line(data = subset(lines_df, LOCATION == "Fixed"), color = "black", linewidth = 2, linetype = "dashed") +
  geom_point(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = LOCATION), alpha = 0.4) +
  labs(
    title = "Temperature and Iron at a 95% confidence interval",
    subtitle = r2_text,
    x = "Year (scaled)",
    y = "Treatment Difference (ln)",
    color = "LOCATION",
    fill = "LOCATION"
  ) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")










#-------------- PLOT MEDIANS

test_median <- stressor_plot_full

test_median <- test_median %>%
  filter(!(LOCATION %in% "Spillhaug" & YEAR %in% c("1999", "2000", "2001", "2002", "2003")))

test_median <- test_median %>% group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(ug_median_MEASURED_VALUE_in = median(MEASURED_VALUE_in*1000),
         median_MEASURED_VALUE_out = median(MEASURED_VALUE_out),
         median_diff_MEASURED_VALUE = median(diff_MEASURED_VALUE),
         sd_ug_MEASURED_VALUE_out = sd(MEASURED_VALUE_out*1000),
         
         ug_min_MEASURED_VALUE_out = min(MEASURED_VALUE_out*1000),
         ug_max_MEASURED_VALUE_out = max(MEASURED_VALUE_out*1000),
         
         ug_mean_MEASURED_VALUE_out = mean(MEASURED_VALUE_out*1000),
         ug_mean_MEASURED_VALUE_in = mean(MEASURED_VALUE_in*1000),
         ug_MEASURED_VALUE_out = (MEASURED_VALUE_out *1000),
         mean_MEASURED_VALUE_in = mean(MEASURED_VALUE_in)) %>%
         ungroup()

test_median <- test_median %>% group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(
    ug_median_MEASURED_VALUE_out = median_MEASURED_VALUE_out*1000) %>%
  ungroup()

test_median <- test_median %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(
    log_ug_median_MEASURED_VALUE_in = log(ug_median_MEASURED_VALUE_in),
         log_ug_median_MEASURED_VALUE_out = log(ug_median_MEASURED_VALUE_out),
         log_ug_sd_MEASURED_VALUE_out = log(sd_ug_MEASURED_VALUE_out),
         log_ug_mean_MEASURED_VALUE_out = log(ug_mean_MEASURED_VALUE_out),
    
    log_ug_min_MEASURED_VALUE_out = log(ug_min_MEASURED_VALUE_out),
    log_ug_max_MEASURED_VALUE_out = log(ug_max_MEASURED_VALUE_out)) %>%
           ungroup()

#Add logarithmic values for median and SD
test_median <- test_median %>%
  
  filter(PARAMETER_ENG %in% c("Perfluorooctanesulfonate (PFOS)","Perfluorooctanoic acid (PFOA)",  
"Perfluoroheksan acid (PFHxA)","Ammonium-N", "Iron","Nitrogen",    "Suspended matter",
 "TOC", "Phosphorus","Manganese","Zinc","Chromium","Nickel","Acenaphthene", "Benzene",    "Fluorene")) %>%
  
  group_by(PARAMETER_ENG, LOCATION) %>%
  
  mutate(
    log_median_diff_MEASURED_VALUE = log(median_diff_MEASURED_VALUE),
    log_SD_diff_MEASURED_VALUE = log(SD_diff_MEASURED_VALUE),
    log_MEAN_diff_MEASURED_VALUE = log(MEAN_diff_MEASURED_VALUE)) %>%
  ungroup()



#To avoid - Inf for sd_MEASURED_VALUE_out if sd = 0 (if < LOQ out).
test_median <- test_median %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
    mutate(
      log_ug_sd_MEASURED_VALUE_out = ifelse(is.infinite(log_ug_sd_MEASURED_VALUE_out), NA, log_ug_sd_MEASURED_VALUE_out))


#USE THIS ------------------------#


#Same plot, but for MEASURED_VALUE_out, only (since log treatment difference removes too many values)!
ggplot(data = test_median, aes(x = reorder(PARAMETER_ENG, log_ug_median_MEASURED_VALUE_out), 
                               y = log_ug_median_MEASURED_VALUE_out, fill = LOCATION)) +
  geom_point(position = position_dodge(width = 0.8), size = 5, shape = 21, color = "black") +  # Farger punktene basert på LOCATION
  geom_errorbar(aes(ymin = log_ug_min_MEASURED_VALUE_out,
                    ymax = log_ug_max_MEASURED_VALUE_out),
                position = position_dodge(width = 0.8), width = 0.2, color = "black", linewidth = 1) +
  scale_fill_viridis_d(option = "A", begin = 0, end = 1) + #0 and 1 creates black and white colours.
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.09))) + 
  labs(
    x = NULL,  
    y = NULL,
    title = NULL,  
    fill = "Location"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14))








#Not related to median graph plot from here

#Create overview of mean values.
mean_values_for_summary <- test_median 

#Create mean, min and max values
values_for_summary <- mean_values_for_summary %>%
  group_by(LOCATION, PARAMETER_ENG) %>% 
    summarise(
      mean_in = (mean(MEASURED_VALUE_in)*1000),
    min_value_in = (min(MEASURED_VALUE_in, na.rm = TRUE)*1000),
    max_value_in = (max(MEASURED_VALUE_in, na.rm = TRUE)*1000),
    mean_out = (mean(MEASURED_VALUE_out)*1000),
    min_value_out = (min(MEASURED_VALUE_out, na.rm = TRUE)*1000),
    max_value_out = (max(MEASURED_VALUE_out, na.rm = TRUE)*1000),
    total_samples = n(),
    .groups = 'drop')

#Count LOQ
values_for_summary_LOQ <- mean_values_for_summary %>%
  group_by(LOCATION, PARAMETER_ENG) %>%
  summarise(
    LOQ_in_0 = sum(LOQ_in == 0),
    LOQ_in_1 = sum(LOQ_in == 1),
    LOQ_out_0 = sum(LOQ_out == 0),
    LOQ_out_1 = sum(LOQ_out == 1),
    .groups = 'drop')
    
#Precentage LOQ    
values_for_summary_LOQ$LOQ_in_percentage <- ((values_for_summary_LOQ$LOQ_in_1/(values_for_summary_LOQ$LOQ_in_1+ values_for_summary_LOQ$LOQ_in_0))*100)
    
values_for_summary_LOQ$LOQ_out_percentage <- ((values_for_summary_LOQ$LOQ_out_1/(values_for_summary_LOQ$LOQ_out_1+ values_for_summary_LOQ$LOQ_out_0))*100)

#Add mean removal
rem_percentage <- mean_values_for_summary %>%
  group_by(LOCATION, PARAMETER_ENG) %>%
  summarise(
    REM_MEDIAN_PERCENTAGE = median(REM_EFF_PERCENT),
  .groups = 'drop')



#Same, but with median marked as points and geom line for in-out (ln transformed).
test_median_longer$ln_MEASURED_VALUE_in <- log(test_median_longer$MEASURED_VALUE_in)
test_median_longer$ln_MEASURED_VALUE_out <- log(test_median_longer$MEASURED_VALUE_out)

#Plot 
ggplot(data = test_median_longer, aes(x = PARAMETER_ENG, y = VALUE, color = interaction(log_median_MEASURED_VALUE_comb, LOCATION))) +
  geom_point(position = position_dodge(width = 0.9), size = 4, stroke = 5) +
  geom_errorbar(aes(ymin = VALUE - ln_SD_diff_measured_value, ymax = VALUE + ln_SD_diff_measured_value),
                position = position_dodge(width = 0.9), width = 0.8) +
  # Titles and labels
  xlab("Contaminants") +
  ylab("Median Value (ln) [mg/l]") +
  ggtitle("Difference in Contaminants Through Treatment System") +
  # Scale for color to represent both location and log median
  scale_color_viridis_d(option = "F") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none")



#MEDIAN TREATMENT DIFFERENCE in similar PLOT, to make graph easier to read.
ggplot(data = test_median_longer, aes(x = PARAMETER_ENG, y = median_diff_MEASURED_VALUE, color = interaction(log_median_MEASURED_VALUE_comb, LOCATION))) +
  geom_point(position = position_dodge(width = 0.9), size = 4, stroke = 5) +
  geom_errorbar(aes(ymin = VALUE - ln_SD_diff_measured_value, ymax = VALUE + ln_SD_diff_measured_value),
                position = position_dodge(width = 0.9), width = 0.8) +
  # Titles and labels
  xlab("Contaminants") +
  ylab("Median Value (ln) [mg/l]") +
  ggtitle("Difference in Contaminants Through Treatment System") +
  # Scale for color to represent both location and log median
  scale_color_viridis_d(option = "F") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none")








#Create median removal (%)
test_median_longer <- test_median_longer %>%
  group_by(PARAMETER_ENG, LOCATION) %>%
  mutate(median_REM_PERCENT = median(REM_EFF_PERCENT))




#Additional check: Check what stressors at Spillhaug (influent to aired) creates a significant difference in treatment.
library(purrr)
t_test_per_stressor_Spillhaug_firstpart <- Spillhaug_clean_data_firstpart %>%
  group_by(PARAMETER_ENG) %>%
  filter(n() >= 2) %>% #Remove stressors with only one observation.
  summarize(
    t_test_Spillhaug_firstpart_per_stressor = list(t.test(MEASURED_VALUE_in, MEASURED_VALUE_aired, paired = TRUE)),
    .groups = "drop")

t_test_Spillhaug_firstpart_per_stressor_p_values <- t_test_per_stressor_Spillhaug_firstpart %>%
mutate(p_value_Spillhaug_each_stressor = map_dbl(t_test_Spillhaug_firstpart_per_stressor, ~ .x$p.value))

#Add column to make it easier to read.
t_test_Spillhaug_firstpart_per_stressor_p_values$p_value_hundred <- (t_test_Spillhaug_firstpart_per_stressor_p_values$p_value_Spillhaug_each_stressor)*100

#-------------
#Additional check: Check what stressors at Spillhaug (aired to effluent) creates a significant difference in treatment.
library(purrr)
t_test_per_stressor_Spillhaug_lastpart <- Spillhaug_clean_data_lastpart %>%
  group_by(PARAMETER_ENG) %>%
  filter(n() >= 2) %>% #Remove stressors with only one observation.
  summarize(
    t_test_Spillhaug_lastpart_per_stressor = list(t.test(MEASURED_VALUE_aired, MEASURED_VALUE_out, paired = TRUE)),
    .groups = "drop")

t_test_Spillhaug_lastpart_per_stressor_p_values <- t_test_per_stressor_Spillhaug_lastpart %>%
  mutate(p_value_Spillhaug_each_stressor = map_dbl(t_test_Spillhaug_lastpart_per_stressor, ~ .x$p.value))

#Add column to make it easier to read.
t_test_Spillhaug_lastpart_per_stressor_p_values$p_value_hundred <- (t_test_Spillhaug_lastpart_per_stressor_p_values$p_value_Spillhaug_each_stressor)*100

#Plot chrome data
chrome_data <- a %>% filter(PARAMETER_ENG %in% c("Chromium"))

#------------------ for the chrome vs year plot. which phase is the landfill in?

#Year vs. Chrome
ggplot() +
  geom_point(data = chrome_data, aes(x = YEAR, y = MEASURED_VALUE_out), 
             size = 2.9, shape = 1) +
  geom_point(data = chrome_data, aes(x = YEAR, y = MEASURED_VALUE_in), 
             size = 2.9, shape = 16) +
  geom_smooth(data = chrome_data, aes(x = YEAR, y = MEASURED_VALUE_out), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_smooth(data = chrome_data, aes(x = YEAR, y = MEASURED_VALUE_in), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  #facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment of Chromium",
       x = "Year",
       y = "Chrome [mg/l]") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.3, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "none")

#----------------------

#Plot iron data

iron_data <- a %>% filter(PARAMETER_ENG %in% c("Iron"))

#------------------ for the chrome vs year plot. which phase is the landfill in?

#Year vs. pH for Iron
ggplot() +
  geom_point(data = iron_data, aes(x = YEAR, y = pH_in), 
             size = 2.9, shape = 16) +
  geom_point(data = iron_data, aes(x = YEAR, y = pH_out), 
             size = 2.9, shape = 1) +
  geom_smooth(data = iron_data, aes(x = YEAR, y = pH_out), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_smooth(data = iron_data, aes(x = YEAR, y = pH_in), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  #facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment of Chromium",
    x = "Year",
    y = "pH") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.3, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "none")

#Treatment vs. pH for Iron
#pH vs. Iron
ggplot() +
  geom_point(data = iron_data, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_in), 
             size = 2.9, shape = 16) +
  geom_point(data = iron_data, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_out), 
             size = 2.9, shape = 1) +
  geom_smooth(data = iron_data, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_out), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_smooth(data = iron_data, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_in), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  #facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment of Chromium",
    x = "Temperature Leachate [C]",
    y = "Iron [mg/l]") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.3, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "none")

#Diff temperature vs. iron
ggplot() +
  geom_point(data = iron_data, aes(x = ph_, y = MEASURED_VALUE_in), 
             size = 2.9, shape = 16) +
  geom_point(data = iron_data, aes(x = YEAR, y = MEASURED_VALUE_out), 
             size = 2.9, shape = 1) +
  geom_smooth(data = iron_data, aes(x = YEAR, y = MEASURED_VALUE_out), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_smooth(data = iron_data, aes(x = YEAR, y = MEASURED_VALUE_in), 
              method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  #facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(#title = "Treatment of Chromium",
    x = "Year",
    y = "Iron [mg/l]") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.3, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "none")



#CREATE MEDIAN VALUES (EXTRA)
library(dplyr)

median <- Ex9_with_NA_values_of_pH

#Filter for relevant stressors.
median <- median %>%
  filter(PARAMETER_ENG %in% c("Perfluorooctanesulfonate (PFOS)", "Zinc", "Suspended matter", "Perfluorooctanoic acid (PFOA)", "Perfluoroheksan acid (PFHxA)", "Nitrogen", "Nickel", "Manganese", "Iron", "Fluorene", "Chromium", "Chemical Oxygen Demand (COD)", "BTEX", "Benzene", "Ammonium-N", "Acenaphthene")) %>%
  mutate(
    YEAR = lubridate::year(SAMPLE_DATE)) %>%
  relocate(YEAR, .before = SAMPLE_DATE) %>%
  select(1:6,8)


#Split main data into a Boelstad df and a Spillhaug df.
median_Boelstad <- subset(median, LOCATION %in% c("Boelstad"))

median_Spillhaug <- subset(median, LOCATION %in% c("Spillhaug"))


#Create difference-value for Boelstad (Bo_Sig1 - Bo_UtRA) and Spillhaug (Sp_K1 - Sp_VM3).

#Part dataset in two for Boelstad and Spillhaug, based on SITE_CODE. Split dataset of Boelstad, to merge afterwards.
median_Bo_in <- median_Boelstad[median_Boelstad$SITE_CODE == "Bo_Sig1",]

median_Bo_out <- median_Boelstad[median_Boelstad$SITE_CODE == "Bo_UtRA2",]

#Split dataset for Spillhaug, to merge afterwards.
median_Sp_in <- median_Spillhaug[median_Spillhaug$SITE_CODE == "Sp_K1K2",]

median_Sp_out <- median_Spillhaug[median_Spillhaug$SITE_CODE == "Sp_VM3",]


#Merging df, based on common SAMPLE_DATE and PARAMETER_ENG. NB! Losing rows, but neccessary, due to comparison.
median_Boelstad <- merge(median_Bo_in, median_Bo_out, by = c("SAMPLE_DATE", "PARAMETER_ENG", "LOCATION", "YEAR"), suffixes = c("_in", "_out"))

median_Spillhaug <- merge(median_Sp_in, median_Sp_out, by = c("SAMPLE_DATE", "PARAMETER_ENG", "LOCATION", "YEAR"), suffixes = c("_in", "_out"))

#Merge dfs.
median <- full_join(median_Boelstad, median_Spillhaug, by = c("LOCATION", "YEAR", "PARAMETER_ENG", "SAMPLE_DATE", "SITE_CODE_in", "MEASURED_VALUE_in", "SITE_CODE_out", "MEASURED_VALUE_out", "LOQ_in", "LOQ_out"))

#Filter for relevant years.
median <- median %>%
  filter(!(LOCATION == "Spillhaug" & YEAR %in% c(1999:2003))) %>%
  filter(!(LOCATION == "Boelstad" & YEAR %in% c(1995:1997)))

#Difference measured value
median$diff_MEASURED_VALUE <- (median$MEASURED_VALUE_in)- (median$MEASURED_VALUE_out)

median$REM_EFF_PERCENT <- (median$diff_MEASURED_VALUE/median$MEASURED_VALUE_in)*100


#Create median values per season:
median <- median %>%
  group_by(LOCATION, PARAMETER_ENG, YEAR, TEMPERATURE_CATEGORY) %>%
  mutate(
    MEDIAN_VALUE_SEASON_YEAR = median(REM_EFF_PERCENT)) %>%
  relocate(MEDIAN_VALUE_SEASON_YEAR, .before = TEMPERATURE_CATEGORY) %>% 
  ungroup()

median_values <- median_values %>%
  group_by(LOCATION, PARAMETER_ENG, TEMPERATURE_CATEGORY) %>%
  mutate(
    MEDIAN_VALUE_SEASON = median(REM_EFF_PERCENT)) %>%
  relocate(MEDIAN_VALUE_SEASON, .before = MEDIAN_VALUE_SEASON_YEAR) %>% 
  ungroup()












#-----------------------
#Change text to universal (UTF-8):
Sys.setlocale("LC_ALL", "en_US.UTF-8")
Sys.getlocale()


#CREATE MEDIAN VALUES (EXTRA)
library(dplyr)
library(lubridate)

median <- Ex3_temp_two_landfills

#Filter for relevant stressors.
median <- median %>%
  #filter(parameter_eng %in% c("Perfluorooctanesulfonate (PFOS)", "Zinc", "Suspended matter", "Perfluorooctanoic acid (PFOA)", "Perfluoroheksan acid (PFHxA)", "Nitrogen", "Nickel", "Manganese", "Iron", "Fluorene", "Chromium", "Chemical Oxygen Demand (COD)", "BTEX", "Benzene", "Ammonium-N", "Acenaphthene")) %>%
  mutate(
    YEAR = lubridate::year(DATE)) %>%
  select(1,2,12,7,3,5,6,11)

median$VALUE <- as.numeric(median$VALUE)
median$DATE <- as.Date(median$DATE)


#Change to ug/l for all units.
median$VALUE <- if_else(
  median$UNIT == "mg/l",  (median$VALUE*1000), median$VALUE)

median$UNIT <- if_else(
  median$UNIT == "µg/l",  "ug/l", median$UNIT)

median$UNIT <- if_else(
  median$UNIT == "mg/l",  "ug/l", median$UNIT)

#Convert an error.
median$UNIT <- ifelse(median$LANDFILL == "Boelstad" & median$DATE == "2023-09-26" & median$parameter_eng == "Perfluorooctanoic acid (PFOA)", "ug/l", median$UNIT)

#Filter for relevant contaminants.
median <-median %>%
  filter(UNIT %in% c("ug/l"))

#Filter for relevant SAMPLES_SITES.
median <- median %>%
  filter(SAMPLE_SITES %in% c("K1", "K2", "Sig", "UtRA", "VM3"))


#Split main data into a Boelstad df and a Spillhaug df.
median_Boelstad <- subset(median, LANDFILL %in% c("Boelstad"))

median_Spillhaug <- subset(median, LANDFILL %in% c("Spillhaug"))


#Separate Spillhaug for K1K2

Sp_K1_df <- median %>% filter(SAMPLE_SITES == "K1")
Sp_K2_df <- median %>% filter(SAMPLE_SITES == "K2")

#Remove duplicates
Sp_K1_df <- Sp_K1_df %>% distinct(DATE, parameter_eng, .keep_all = TRUE)
Sp_K2_df <- Sp_K2_df %>% distinct(DATE, parameter_eng, .keep_all = TRUE)

#Combine dataset based on common SAMPLE_DATE and PARAMETER_ENG
merged_data_Spillhaug <- full_join(Sp_K1_df, Sp_K2_df, by = c("DATE", "parameter_eng"), suffix = c("_k1", "_k2"))


#Remove NA-values.
merged_data_Spillhaug <- merged_data_Spillhaug %>% filter(!is.na(VALUE_k1))



#Create a mean of K1 and K2.

merged_data_Spillhaug <- merged_data_Spillhaug %>%
  mutate(VALUE = ifelse(!is.na(VALUE_k1) & !is.na(VALUE_k2),
                                 (VALUE_k1 + VALUE_k2) / 2,
                                 VALUE_k1)) %>%
  mutate(LOQ = ifelse(!is.na(LOQ_k1), LOQ_k1, LOQ_k2)) %>%
  select(1,3,15,16,4,5,7,2) %>%
  rename(
    LANDFILL = LANDFILL_k1,
    YEAR = YEAR_k1,
    UNIT = UNIT_k1, 
    SAMPLE_SITES = SAMPLE_SITES_k1)

#Part dataset in two
median_Bo_in <- median_Boelstad[median_Boelstad$SAMPLE_SITES == "Sig",]

median_Bo_out <- median_Boelstad[median_Boelstad$SAMPLE_SITES == "UtRA",]

#Split dataset for Spillhaug, to merge afterwards.
median_Sp_in <- merged_data_Spillhaug

median_Sp_out <- median_Spillhaug[median_Spillhaug$SAMPLE_SITES == "VM3",]


#Merging df, based on common SAMPLE_DATE and PARAMETER_ENG. NB! Losing rows, but neccessary, due to comparison.
median_Boelstad <- merge(median_Bo_in, median_Bo_out, by = c("DATE", "parameter_eng", "LANDFILL", "YEAR"), suffixes = c("_in", "_out"))

median_Spillhaug <- merge(median_Sp_in, median_Sp_out, by = c("DATE", "parameter_eng", "LANDFILL", "YEAR"), suffixes = c("_in", "_out"))

#Merge dfs.
median <- full_join(median_Boelstad, median_Spillhaug, by = c("LANDFILL", "YEAR", "parameter_eng", "DATE", "SAMPLE_SITES_in", "VALUE_in", "SAMPLE_SITES_out", "VALUE_out", "LOQ_in", "LOQ_out"))


#Filter for relevant years.
median <- median %>%
  filter(!(LANDFILL == "Spillhaug" & YEAR %in% c(1999:2003))) %>%
  filter(!(LANDFILL == "Boelstad" & YEAR %in% c(1995:1997)))

#Difference measured value
median$diff_MEASURED_VALUE <- (median$VALUE_in)- (median$VALUE_out)

median$REM_EFF_PERCENT <- (median$diff_MEASURED_VALUE/median$VALUE_in)*100


#Import temperature category, based on month (based on temperature from previous code).
median$TEMPERATURE_CATEGORY <- ifelse(month(median$DATE) %in% 4:9, "Summer", "Winter")

#Remove a NA-value for BTEX in VALUE_out.
median <- median %>%
  filter(!is.na(VALUE_out))

#Create median values per season. NB! Exclude values where both LOQin and LOQout are 1, as it cant be used for measurements.
median <- median %>%
  group_by(LANDFILL, parameter_eng, YEAR, TEMPERATURE_CATEGORY) %>%
  mutate(
    MEDIAN_VALUE_SEASON_YEAR = median(
      REM_EFF_PERCENT[!(LOQ_in == 1 & LOQ_out == 1 & diff_MEASURED_VALUE == 0)],
      na.rm = TRUE
    )
  ) %>%
  relocate(MEDIAN_VALUE_SEASON_YEAR, .before = TEMPERATURE_CATEGORY) %>%
  ungroup()

median <- median %>%
  group_by(LANDFILL, parameter_eng, TEMPERATURE_CATEGORY) %>%
  mutate(
    MEDIAN_VALUE_SEASON = median(REM_EFF_PERCENT)) %>%
  relocate(MEDIAN_VALUE_SEASON, .before = MEDIAN_VALUE_SEASON_YEAR) %>% 
  ungroup()

#Median overall
median <- median %>%
  group_by(LANDFILL,parameter_eng) %>%
  mutate(MEDIAN_REMOVAL_OVERALL = median(REM_EFF_PERCENT))

#Create summary table.
summary <- median %>%
  group_by(parameter_eng, LANDFILL, TEMPERATURE_CATEGORY) %>%
  summarise(
    n = n_distinct(diff_MEASURED_VALUE),
    .groups = "drop")

#Extract unique median removal overall.
summary2 <- median %>%
  group_by(parameter_eng, LANDFILL) %>%
  summarise(
    MEDIAN_overall = list(unique(MEDIAN_REMOVAL_OVERALL)), .groups = "drop")

  
#Extract removal per season and max/min per season.
summary3 <- median %>%
  group_by(parameter_eng, LANDFILL, TEMPERATURE_CATEGORY) %>%
  summarise(
    MEDIAN_SEASON = list(unique(MEDIAN_VALUE_SEASON)),
    min_REM_EFF_SEASON = min(REM_EFF_PERCENT),
    max_REM_EFF_SEASON = max(REM_EFF_PERCENT),
    .groups = "drop")

summary3 <- summary3 %>%
  relocate(min_REM_EFF_SEASON, .before = LANDFILL) %>%
  relocate(max_REM_EFF_SEASON, .before = LANDFILL) %>%
  relocate(TEMPERATURE_CATEGORY, .before = min_REM_EFF_SEASON)

#Also add influent and effluent mean values, with minimum and maximum VALUE_in and VALUE_out.
summary_influent <- median %>%
  group_by(parameter_eng, LANDFILL) %>%
  summarise(
    mean_VALUE_in_overall = mean(VALUE_in),
    mean_VALUE_out_overall = mean(VALUE_out),
    min_VALUE_in = min(VALUE_in),
    max_VALUE_in = max(VALUE_in),
    min_VALUE_out = min(VALUE_out),
    max_VALUE_out = max(VALUE_out),
    .groups = "drop"
  )

#Calculate % of LOQ_in and LOQ_out per landfill and contaminant.
loq_summary <- median %>%
  group_by(parameter_eng, LANDFILL) %>%
  summarise(
    total_LOQ_in = sum(LOQ_in %in% c(0, 1)),
    LOQ_in_1 = sum(LOQ_in == 1),
    total_LOQ_out = sum(LOQ_out %in% c(0, 1)),
    LOQ_out_1 = sum(LOQ_out == 1),
    .groups = "drop"
  ) %>%
  mutate(
    LOQ_in_pct = round(100 * LOQ_in_1 / total_LOQ_in),
    LOQ_out_pct = round(100 * LOQ_out_1 / total_LOQ_out),
    LOQ_percent_summary = paste0(LOQ_in_pct, "% / ", LOQ_out_pct, "%"))


#Combine everything in a word table.
library(officer)
library(dplyr)

full_summary <- summary %>%
  full_join(summary2, by = c("parameter_eng", "LANDFILL")) %>%
  full_join(summary3, by = c("parameter_eng", "LANDFILL", "TEMPERATURE_CATEGORY")) %>%
  full_join(summary_influent, by = c("parameter_eng", "LANDFILL")) %>%
  full_join(loq_summary %>% select(parameter_eng, LANDFILL, LOQ_percent_summary),
            by = c("parameter_eng", "LANDFILL"))

#Convert to numeric.
full_summary <- full_summary %>%
mutate(across(4:14, as.numeric))

#Change all numbers to non-exponential numbers.
full_summary <- full_summary %>%
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 5))) %>%
  select(1:3,15,5,8,6,7,9,11:12,10,13:14,4)


#Filter so that Boelstad is above and Spillhaug below per parameter_eng.
full_summary <- full_summary %>%
  mutate(
    LANDFILL = factor(LANDFILL, levels = c("Boelstad", "Spillhaug"))
  ) %>%
  arrange(parameter_eng, LANDFILL)

#Round different columns to different sig.fig.
full_summary <- full_summary %>%
  mutate(
    MEDIAN_overall = round(MEDIAN_overall, 0),
    col2 = round(col2, 2),
    col3 = round(col3, 1)
  )

#Export to word

#Create document.
doc <- read_docx()

#Paste table.
doc <- doc %>%
  body_add_table(full_summary, style = "table_template")

# Lagre dokument
print(doc, target = "Landfill_Full_Summary.docx")
















#-------------------------------------- PLOT IRON AND TEMPERATURE AND CHROMIUM AND MATURITY-----------------





#Plot extra for iron and temperature

#Create slope
intercept_iron_plot <- fixed_effects_iron["(Intercept)", "Estimate"]

slope_iron_plot <- fixed_effects_iron["TEMPERATURE_VALUE_out", "Estimate"]

#Create plot
data_iron$predicted_line <- intercept_iron_plot + (slope_iron_plot*(data_iron$TEMPERATURE_VALUE_out))

#Create R2 per lm-line.
data_iron_R2 <- data_iron %>%
  group_by(LOCATION) %>%
  summarise(R2 = summary(lm(transformed_MEASURED_VALUE ~ TEMPERATURE_VALUE_out, data = cur_data()))$r.squared)

#PLOT iron and temperature
ggplot() +
  geom_point(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, fill = factor(LOCATION)), size = 3.3, shape = 21, color = "black") +
  
  geom_smooth(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = transformed_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "dashed", linewidth = 1.5) +
  
  geom_line(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = predicted_line), color = "black", linewidth = 1.5)+
  
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(
    x = "Temperature",
    y = "Treatment Difference [mg/l]") +
  #scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  scale_color_viridis_d(option = "D")+
  scale_fill_viridis_d(option = "D")+
  
  theme_minimal() +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "bottom"))

#Calculate R2 for data_iron$predicted_line
y_obs = data_iron$transformed_MEASURED_VALUE
y_pred = data_iron$predicted_line


#Residual sum of squares (rss):
rss <- sum((y_obs - y_pred)^2)

#Total sum of squares (tss):
tss <- sum((y_obs - mean(y_obs))^2)


R2_iron <- 1 - (rss/tss)




#Plot iron and temperature for influent and effluent with R2
data_iron <- data_iron %>%
  mutate(
    MEASURED_VALUE_in_scaled = scale(MEASURED_VALUE_in),
    MEASURED_VALUE_in_YJ = yeojohnson(MEASURED_VALUE_in)$x.t,
    MEASURED_VALUE_out_scaled = scale(MEASURED_VALUE_out),
    MEASURED_VALUE_out_YJ = yeojohnson(MEASURED_VALUE_out)$x.t)


ggplot() +
  geom_point(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_in_YJ), fill = "#440154", size = 3.3, shape = 21, color = "black") +
  
  geom_point(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_out_YJ), fill = "#FDE725", size = 3.3, shape = 21, color = "black") +
  
  geom_smooth(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_in_YJ), color = "#440154", method = "lm", linetype = "solid", linewidth = 1.5) +
  
  geom_smooth(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = MEASURED_VALUE_out_YJ), color = "#FDE725", method = "lm", linetype = "solid", linewidth = 1.5) +
  
  # geom_line(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = predicted_line), color = "black", linewidth = 1)+
  
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(
    x = "Temperature (scaled)",
    y = "Treatment Difference (Yeo-Johnson) [mg/l]") +
  #scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  #scale_color_viridis_c(option = "C")+
  #scale_fill_viridis_c(option = "C")+
  
  theme_minimal() +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "bottom"))



#Create R2 per lm-line.
data_iron %>%
  ungroup() %>%
  summarise(
    R2_in = summary(lm(MEASURED_VALUE_in ~ TEMPERATURE_nonScaled))$r.squared,
    R2_out = summary(lm(MEASURED_VALUE_out ~ TEMPERATURE_nonScaled))$r.squared)


data_iron %>%
  ungroup() %>%
  summarise(
    R2_in = summary(lm(MEASURED_VALUE_in_YJ ~ TEMPERATURE_VALUE_out))$r.squared,
    R2_out = summary(lm(MEASURED_VALUE_out_YJ ~ TEMPERATURE_VALUE_out))$r.squared)

#------------CHROMIUM---------------




#Plot extra for iron and temperature

#Create slope
intercept_chrom_plot <- fixed_effects_chrome2["(Intercept)", "Estimate"]

slope_chrom_plot <- fixed_effects_chrome2["YEAR", "Estimate"]

#Create plot
data_chrome2$predicted_line <- intercept_chrom_plot + (slope_chrom_plot*(data_chrome2$YEAR))

#Create R2 per lm-lin for transformed
data_chrome2 %>%
  group_by(LOCATION) %>%
  summarise(R2 = summary(lm(log_diff_MEASURED_VALUE ~ YEAR, data = cur_data()))$r.squared)



#R2 for non-scaled
data_chrome2 %>%
  group_by(LOCATION) %>%
  summarise(R2 = summary(lm(diff_MEASURED_VALUE ~ YEAR_nonScaled, data = cur_data()))$r.squared)


#PLOT chrome and maturity

ggplot() +
  geom_point(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, fill = factor(LOCATION)), size = 3.3, shape = 21, color = "black") +
  
geom_smooth(data = data_chrome2, aes(x = YEAR, y = log_diff_MEASURED_VALUE, color = factor(LOCATION)), method = "lm", linetype = "solid", linewidth = 1.5) +
  
   geom_line(data = data_chrome2, aes(x = YEAR, y = predicted_line), color = "black", linewidth = 1.5)+
  
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(
    x = "Year (scaled)",
    y = "Treatment Difference (ln) [mg/l]") +
  #scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  scale_color_viridis_d(option = "D")+
  scale_fill_viridis_d(option = "D")+
  
  theme_minimal() +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "bottom"))

#Calculate R2 for data_iron$predicted_line
y_obs = data_chrome2$log_diff_MEASURED_VALUE
y_pred = data_chrome2$predicted_line


#Residual sum of squares (rss):
rss <- sum((y_obs - y_pred)^2)

#Total sum of squares (tss):
tss <- sum((y_obs - mean(y_obs))^2)


R2_chrome <- 1 - (rss/tss)




#Plot iron and temperature for influent and effluent with R2
data_chrome2 <- data_chrome2 %>%
  mutate(
    MEASURED_VALUE_in_scaled = scale(MEASURED_VALUE_in),
    MEASURED_VALUE_in_log = log(MEASURED_VALUE_in),
    MEASURED_VALUE_out_scaled = scale(MEASURED_VALUE_out),
    MEASURED_VALUE_out_log = log(MEASURED_VALUE_out))


ggplot() +
  geom_point(data = data_chrome2, aes(x = YEAR_nonScaled, y = MEASURED_VALUE_in), fill = "#440154", size = 3.3, shape = 21, color = "black") +
  
  geom_point(data = data_chrome2, aes(x = YEAR_nonScaled, y = MEASURED_VALUE_out), fill = "#FDE725", size = 3.3, shape = 21, color = "black") +
  
  geom_smooth(data = data_chrome2, aes(x = YEAR_nonScaled, y = MEASURED_VALUE_in), color = "#440154", method = "lm", linetype = "solid", linewidth = 1.5) +
  
  geom_smooth(data = data_chrome2, aes(x = YEAR_nonScaled, y = MEASURED_VALUE_out), color = "#FDE725", method = "lm", linetype = "solid", linewidth = 1.5) +
  
  # geom_line(data = data_iron, aes(x = TEMPERATURE_VALUE_out, y = predicted_line), color = "black", linewidth = 1)+
  
  facet_wrap(~ PARAMETER_ENG, scales = "free") +
  labs(
    x = "Year (scaled)",
    y = "Treatment Difference (ln) [mg/l]") +
  #scale_color_manual(values = c("Boelstad" = "#E69F00", "Spillhaug" = "#3B8FB5"))+
  #scale_color_viridis_c(option = "C")+
  #scale_fill_viridis_c(option = "C")+
  
  theme_minimal() +
  theme(panel.grid.major = element_blank(), #blank background
        panel.grid.minor = element_blank(), #blank background
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        theme(legend.position = "bottom"))



#Create R2 per lm-line.
data_chrome2 %>%
  ungroup() %>%
  summarise(
    R2_in = summary(lm(MEASURED_VALUE_in ~ YEAR_nonScaled))$r.squared,
    R2_out = summary(lm(MEASURED_VALUE_out ~ YEAR_nonScaled))$r.squared)


data_chrome2 %>%
  ungroup() %>%
  summarise(
    R2_in = summary(lm(MEASURED_VALUE_in_log ~ YEAR))$r.squared,
    R2_out = summary(lm(MEASURED_VALUE_out_log ~ YEAR))$r.squared)




