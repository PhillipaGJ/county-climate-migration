#Phillipa Gowdy-Jaehnig
#for PA5046 Econometrics for Public Policy
###################### To calculate Net Migration starting in 2005
#started 2026-04-09
#last updated 2026-10-18

#install.packages("censusapi")
#install.packages("here")
library(censusapi)
library(conflicted)
library(exactextractr)
library(ggplot2)
library(haven)
library(here)
library(ncdf4) # package for netcdf manipulation
library(purrr)
library(raster) # package for raster manipulation
library(readr)
library(readxl)
library(sf)
library(stringr)
library(terra)
library(tidycensus)
library(tidyverse)
library(tigris)
library(zoo)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

library(tidyverse)
library(tidycensus)


# Define the 3 years in the range
years_range <- c(2009, 2014, 2019)

###############################################################################
##First, get the ACS County-to_county Migration Flows annual data
##and aggregate them into 5-year cumulative net migration (a rolling sum)
##to match the ACS 5-year data



# List states for loop, then non-contiguous for exclusion
all_states <- unique(fips_codes$state_code)[1:51] # 50 states + DC, 
non_contiguous <- c("02", "15", "60", "66", "69", "72", "78")


# Define late years range for migration data available through the API
years_late <- c(2014, 2019)

# Years prior to 2010 are not available for 5-year migration API download
# due to 5-year migration flows starting in 2006.
# Download the excel file under "Documentation and Analysis" 
# at https://www.census.gov/library/working-papers/2012/demo/benetsky-02.html

# File path for the above downloaded file for 2005-2009 migration counts by county
migration_file_path <- here("data", "raw", "county_to_county_mig_working_paper_tables.xlsx")

# Load in data from the excel file and clean it
migration_2005_2009 <- read_excel(
  path = migration_file_path,
  sheet = "MigrationCounts",
  range = "A1:H3144",
  col_names = TRUE,
  col_types = c("numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric") # Forces fips column to stay as text
)|>
  mutate(
    GEOID = full_fips,
    year = 2009,
    net_migration = Net_Migration,
    gross_migration = Gross_Migration,
  )|>
  select(
    GEOID,
    year,
    net_migration,
    gross_migration
  )
contiguous_migration_2005_2009 <- migration_2005_2009 |>
  filter(!substr(GEOID, 1, 2) %in% non_contiguous)

#Use API to get newer data, then clean it
# Loop through years and states using purrr map_dfr() function
migration_2010_2019 <- map_dfr(years_late, function(yr) {
  message(paste("Fetching data for year:", yr)) # Progress tracker
  
  map_dfr(all_states, function(st) {
    # Wrapped in tryCatch in case a specific year/state combo has no data
    tryCatch({
      get_flows(
        geography = "county",
        state = st,
        year = yr,
        geometry = FALSE
      ) %>% 
        mutate(year = yr)
    }, error = function(e) return(NULL))
  })
})

# Process stats, adding year to group_by to keep the 5 years distinct
migration_2010_2019 <- migration_2010_2019 %>%
  filter(!is.na(moe)) %>%
  group_by(year, FULL1_NAME) %>% 
  summarise(
    GEOID = first(GEOID1),
    in_migration = sum(estimate[variable == "MOVEDIN"], na.rm = TRUE),
    out_migration = sum(estimate[variable == "MOVEDOUT"], na.rm = TRUE),
    net_migration = in_migration - out_migration,
    gross_migration = in_migration + out_migration,
    .groups = "drop"
  )

contiguous_migration_2010_2019 <- migration_2010_2019 %>%
  filter(!substr(GEOID, 1, 2) %in% non_contiguous) %>%
  select(!FULL1_NAME)|>
  select(GEOID,
         year,
         net_migration,
         gross_migration)

migration_stats <- bind_rows(contiguous_migration_2005_2009, contiguous_migration_2010_2019)

# Standardize the old migration data FIPS to match modern datasets
migration_stats <- migration_stats %>%
  mutate(GEOID = case_when(
    # Alaska Fixes
    GEOID == "2195" ~ "02198", # Petersburg
    GEOID == "2270" ~ "02158", # Wade Hampton -> Kusilvak
    
    # South Dakota Fix
    GEOID == "46113" ~ "46102", # Shannon -> Oglala Lakota
    
    # Virginia Fix (Independent City)
    GEOID == "51678" ~ "51163", # Lexington -> Rockbridge
    
    TRUE ~ GEOID
  ))|>
  filter(GEOID != "11001")  #Ensure D.C. is removed

# Write to file for safety
write.csv(migration_stats, 
          here("data", "processed", "acs_5yr_migration_county_2009_2014_2019.csv"), 
          row.names = FALSE)



###############################################################################
##Second, get 5-year climate data for the 3 years

# Create the vectors and tables to use for every imported table

# Name the columns for all the monthly datasets
my_labels <- c("ID", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_cols <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#Since state codes are different between NOAA and Census FIPS, we need to translate:
#Create a manual "crosswalk" or reference table to translate between the 2
noaa_to_fips <- tribble(
  ~noaa_state, ~fips_state, ~state_name,
  "01", "01", "Alabama",
  "02", "04", "Arizona",
  "03", "05", "Arkansas",
  "04", "06", "California",
  "05", "08", "Colorado",
  "06", "09", "Connecticut",
  "07", "10", "Delaware",
  "08", "12", "Florida",
  "09", "13", "Georgia",
  "10", "16", "Idaho",
  "11", "17", "Illinois",
  "12", "18", "Indiana",
  "13", "19", "Iowa",
  "14", "20", "Kansas",
  "15", "21", "Kentucky",
  "16", "22", "Louisiana",
  "17", "23", "Maine",
  "18", "24", "Maryland",
  "19", "25", "Massachusetts",
  "20", "26", "Michigan",
  "21", "27", "Minnesota",
  "22", "28", "Mississippi",
  "23", "29", "Missouri",
  "24", "30", "Montana",
  "25", "31", "Nebraska",
  "26", "32", "Nevada",
  "27", "33", "New Hampshire",
  "28", "34", "New Jersey",
  "29", "35", "New Mexico",
  "30", "36", "New York",
  "31", "37", "North Carolina",
  "32", "38", "North Dakota",
  "33", "39", "Ohio",
  "34", "40", "Oklahoma",
  "35", "41", "Oregon",
  "36", "42", "Pennsylvania",
  "37", "44", "Rhode Island",
  "38", "45", "South Carolina",
  "39", "46", "South Dakota",
  "40", "47", "Tennessee",
  "41", "48", "Texas",
  "42", "49", "Utah",
  "43", "50", "Vermont",
  "44", "51", "Virginia",
  "45", "53", "Washington",
  "46", "54", "West Virginia",
  "47", "55", "Wisconsin",
  "48", "56", "Wyoming",
  "49", "02", "Alaska",
  "50", "02", "Alaska"
)

## Import and Preprocess NOAA data from file saved on computer

# Read in file
df <- read_fwf(here("data", "raw", "climdiv-tmpccy-v1.0.0-20260205"), 
               col_positions = fwf_widths(c(11, rep(7, 12)), col_names = my_labels))

# Extract Year first and filter immediately for efficiency
df_final <- df %>%
  mutate(year = as.numeric(str_sub(ID, 8, 11))) %>%
  filter(year >= 1999 & year <= 2019) %>%
  # Extract state and county ID components
  mutate(
    noaa_state  = str_sub(ID, 1, 2),
    noaa_county = str_sub(ID, 3, 5)
  )

# Clean temperature values (Vectorized approach is faster)
df_final <- df_final %>%
  mutate(across(all_of(month_cols), ~ {
    val <- as.numeric(str_sub(.x, -5))
    if_else(val < -90, NA_real_, val)
  }))

# Calculate annual mean and join FIPS
noaa_tavg_county_annual <- df_final %>%
  mutate(
    annual_avg_temp = rowMeans(select(., all_of(month_cols)), na.rm = TRUE)
  ) %>%
  left_join(noaa_to_fips, by = "noaa_state") %>%
  mutate(
    full_fips = paste0(fips_state, str_pad(noaa_county, 3, pad = "0"))
  ) %>%
  select(full_fips, state_name, year, annual_avg_temp)

glimpse(noaa_tavg_county_annual)


## B. Now let's preprocess the county-level precipitation data to get annual precip totals per county per year

# Read in precipitation file
df_pcpn <- read_fwf(here("data", "raw", "climdiv-pcpncy-v1.0.0-20260205"), 
                    col_positions = fwf_widths(c(11, rep(7, 12)), col_names = my_labels))

# Extract Year and Filter
df_pcpn_final <- df_pcpn %>%
  mutate(year = as.numeric(str_sub(ID, 8, 11))) %>%
  filter(year >= 1999 & year <= 2019) %>%
  mutate(
    noaa_state  = str_sub(ID, 1, 2),
    noaa_county = str_sub(ID, 3, 5)
  )

# Clean precipitation values
# NOAA precipitation is in inches. Missing values are still coded as -9.99 or -99.9
df_pcpn_final <- df_pcpn_final %>%
  mutate(across(all_of(month_cols), ~ {
    val <- as.numeric(str_sub(.x, -5))
    if_else(val < 0, NA_real_, val) # Precip cannot be negative; handle missing codes
  }))

# Calculate annual total precip
noaa_pcpn_county_annual <- df_pcpn_final %>%
  mutate(
    annual_precip = rowSums(pick(all_of(month_cols)), na.rm = TRUE)
  ) %>%
  left_join(noaa_to_fips, by = "noaa_state") %>%
  mutate(
    full_fips = paste0(fips_state, str_pad(noaa_county, 3, pad = "0"))
  ) %>%
  select(full_fips, state_name, year, annual_precip)

glimpse(noaa_pcpn_county_annual)



## C. Now let's join the data and calculate our 5-year changes

county_climate_measures <- left_join(noaa_tavg_county_annual, noaa_pcpn_county_annual, by = c("full_fips", "state_name", "year"))
glimpse(county_climate_measures)

county_climate_measures <- county_climate_measures |>
  arrange(full_fips, year) |> # Sort so that year values are in order
  group_by(full_fips) |>     # Group so that calculation doesn't spill over between counties
  mutate(
    # 5-year rolling average (looks at current + previous 4 years), fills first 4 years in sample witn NA)
    temp_roll_avg_5yr = zoo::rollmean(annual_avg_temp, k = 5, fill = NA, align = "right"),
    
    # Change from 5 years ago (lag looks back 5 rows)
    temp_diff_5yr_ago = annual_avg_temp - dplyr::lag(annual_avg_temp, 5),
    
    # Change in 5-year average
    fiveyr_avg_temp_diff_5yr_ago = temp_roll_avg_5yr - dplyr::lag(temp_roll_avg_5yr, 5),
  ) |>
  ungroup()

# Calculate 5-year average precip and precip changes
county_climate_measures <- county_climate_measures |>
  arrange(full_fips, year) |> # Sort so that year values are in order
  group_by(full_fips) |>     # Group so that calculation doesn't spill over between counties
  mutate(
    # 5-year rolling average (looks at current + previous 4 years), fills first 4 years in sample witn NA)
    precip_roll_avg_5yr = zoo::rollmean(annual_precip, k = 5, fill = NA, align = "right"),
    
    # Change from 5 years ago (lag looks back 5 rows)
    precip_diff_5yr_ago = annual_precip - dplyr::lag(annual_precip, 5),
    
    # Change in 5-year average
    fiveyr_avg_precip_diff_5yr_ago = precip_roll_avg_5yr - dplyr::lag(precip_roll_avg_5yr, 5)
  ) |>
  ungroup()

glimpse(county_climate_measures)

# Now subset data so it only takes the 5-year calculated columns for our 3 years
climate_data <- county_climate_measures |>
  filter(year %in% years_range, !is.na(state_name), state_name != "Alaska", state_name != "Hawaii")|>
  mutate(GEOID = full_fips) |>
  select(GEOID,
         state_name,
         year,
         temp_roll_avg_5yr,
         range_roll_avg_5yr,
         fiveyr_avg_temp_diff_5yr_ago,
         precip_roll_avg_5yr,
         fiveyr_avg_precip_diff_5yr_ago)|>
  filter(GEOID != "24511")
glimpse(climate_data)

check_vals <- ifelse(is.na(climate_data$fiveyr_avg_temp_diff_5yr_ago), "NA",
                     ifelse(climate_data$fiveyr_avg_temp_diff_5yr_ago == 0, "Zero", "Valid"))

plot(as.factor(check_vals), col = "steelblue", main = "Entry Check: Change in Temp")

# Write to file for safety
write.csv(climate_data, 
          here("data", "processed", "noaa_climate_county_2009_2014_2019.csv"), 
          row.names = FALSE)


###############################################################################
##Third, get 5-year demographic data


## Step 1: Get population variables from ACS5
# # Get variable names for primary ACS5 table - Uncomment to use
# vars2010 <- load_variables(2010, "acs5", cache = TRUE)
# print(vars2010)
# vars19 <- load_variables(2019, "acs5", cache = TRUE)
# print(vars19)

#Variable names are different for different years, so we need to do 2 separate pulls

##A. 2015 and 2019 Data pull
# Define the variables to pull using 'Detailed Table' IDs for 2015 & 2019
acs_vars_2019 <- c(
  total_pop          = "B01003_001",
  median_age         = "B01002_001",
  median_hh_inc      = "B19013_001",
  median_rent        = "B25064_001",
  median_home_val    = "B25077_001",
  pop_in_labor_force = "B23025_003",
  num_unemployed     = "B23025_005",
  total_households   = "B25003_001",
  owner_occupied     = "B25003_002",
  renter_occupied    = "B25003_003",
  edu_total          = "B15003_001",
  edu_bachelors      = "B15003_022", 
  moved_in_past_year = "B07003_001"  
)

# Fetch the data using map_dfr() loop from purrr to pull the data for 2015 and 2019
county_data_2014_2019 <- map_dfr(c(2014, 2019), ~ {
  get_acs(
    geography = "county",
    variables = acs_vars_2019,
    year = .x,          # .x represents the current year in the loop
    survey = "acs5",
    output = "wide"
  ) |>
    # Add a year column
    mutate(year = .x)
}) |>
  # Clean up column names (remove 'E' for Estimate and 'M' for Margin of Error)
  select(GEOID, NAME, year, ends_with("E")) |>
  rename_with(~str_remove(., "E$")) |>
  # Calculate rates
  mutate(
    unemployment_rate = num_unemployed / pop_in_labor_force,
    pct_renter        = renter_occupied / total_households,
    pct_bachelors     = edu_bachelors / edu_total,
    NAME              = NAM
  )|>
  select(-NAM,
         -edu_bachelors)
#str(county_data)

##B. Now the 2010 pull
# Define the variables to pull using 'Detailed Table' IDs for 2010
acs_vars_2010 <- c(
  total_pop          = "B01003_001",
  median_age         = "B01002_001",
  median_hh_inc      = "B19013_001",
  median_rent        = "B25064_001",
  median_home_val    = "B25077_001",
  pop_in_labor_force = "B23025_002E",     #2010 report difference
  total_households   = "B25003_001",
  owner_occupied     = "B25003_002",
  renter_occupied    = "B25003_003",
  edu_total          = "B15002_001",
  edu_bachelors_female    = "B15002_032", 
  edu_bachelors_male      = "B15002_015",
  moved_in_past_year = "B07003_001"  
)

# Fetch the data for 2010 using the alternate variable list
county_data_2009_a <- get_acs(
  geography = "county",
  variables = acs_vars_2010,
  year = 2009,          # .x represents the current year in the loop
  survey = "acs5",
  output = "wide"
) 

# Pull the Unemployed variable from Data Profile (Table DP)
county_data_2009_b <- get_acs(geography = "county", variables = c(num_unemployed = "DP03_0005"), 
                              year = 2009, output = "wide")

#Join the unemployed number to the rest of the 2010 data and clean it up
county_data_2009 <- county_data_2009_a |>
  left_join(county_data_2009_b, by = c("GEOID", "NAME"))|>
  # Add a year column
  mutate(year = 2009) |>
  # Clean up column names (remove 'E' for Estimate and 'M' for Margin of Error)
  select(GEOID, NAME, year, ends_with("E")) |>
  rename_with(~str_remove(., "E$")) |>
  # Calculate rates
  mutate(
    unemployment_rate = num_unemployed / pop_in_labor_force,
    pct_renter        = renter_occupied / total_households,
    pct_bachelors     = (edu_bachelors_female + edu_bachelors_male) / edu_total,
    NAME              = NAM
  )|>
  select(-NAM,
         -edu_bachelors_female,
         -edu_bachelors_male)
#str(county_data)

#Join the two
county_data <- bind_rows(county_data_2009, county_data_2014_2019)
glimpse(county_data)

#Select desired variables
county_data_pop <- county_data |>
  select(
    GEOID,
    NAME,
    year,
    total_pop,
    median_age,
    median_hh_inc,
    median_rent,
    median_home_val,
    unemployment_rate,
    pct_renter,
    pct_bachelors,
    moved_in_past_year
  )


## Step 2: Get Rural-Urban Continuum Codes  and "urban influence" from USDA ERS website
# https://www.ers.usda.gov/data-products/rural-urban-continuum-codes
# The RUCCs are only updated every 10 years. We have chosen the 2023 codes over the 2013 codes
# due to greater proximity in time. However, please note that "the threshold for 
# urban area population was raised from at least 2,500 to 5,000 people, reflecting 
# changes to urban area qualification introduced by the U.S. Census Bureau in 2020."
rucc_data <- read_excel("ruralurbancodes2013.xls")
str(rucc_data)
head(rucc_data)

#The table has 3 rows per county, one each for attribute "Population_2020", "RUCC_2023", and "Description"
# We already have the population, so we will make a table with 1 row per county and 
# 2 separate columns for RUCC code and description.
rucc_wide <- rucc_data |>
  # filter(Attribute %in% c("RUCC_2013", "Description")) |>     #Filter out population
  # pivot_wider(                  #Make new columns
  #   names_from = Attribute, 
  #   values_from = Value
  # ) |>
  # recode Description for ease of reading (see)
  mutate(
    rucc_group = case_when(
      RUCC_2013 == 1 ~ "Urban Core",
      RUCC_2013 %in% c(2, 3) ~ "Suburban/Small Metro",
      RUCC_2013 %in% c(4, 6, 8) ~ "Exurban/Adjacent Rural",
      RUCC_2013 %in% c(5, 7, 9) ~ "Remote Rural",
      TRUE ~ "Other"
    )
  )

##RUCC Code Dictionary:  
# 1 "Metro - Counties in metro areas of 1 million population or more" 
# 2 "Metro - Counties in metro areas of 250,000 to 1 million population"
# 3 "Metro - Counties in metro areas of fewer than 250,000 population"
# 4 "Nonmetro - Urban population of 20,000 or more, adjacent to a metro area"
# 5 "Nonmetro - Urban population of 20,000 or more, not adjacent to a metro area"
# 6 "Nonmetro - Urban population of 5,000 to 20,000, adjacent to a metro area" 
# 7 "Nonmetro - Urban population of 5,000 to 20,000, not adjacent to a metro area" 
# 8 "Nonmetro - Urban population of fewer than 5,000, adjacent to a metro area"
# 9 "Nonmetro - Urban population of fewer than 5,000, not adjacent to a metro area"
# 88 (Unknown-Alaska/Hawaii State/not official USDA Rural-Urban Continuum code) - not in this dataset
# 99 "Not Applicable" (Unknown/not official USDA Rural-Urban Continuum code)

# Prep table for joining:  
rucc_final <- rucc_wide |>
  # Rename RUCC columns and FIPS (to easily join to other tables) and ensure FIPS is 5 characters
  rename(rucc_2013_label = Description,
         rucc_2013_code = RUCC_2013,
  ) |>
  mutate(GEOID = str_pad(FIPS, width = 5, side = "left", pad = "0"))|>
  # simplify table for merge by only selecting necessary fields
  select(GEOID,
         rucc_2013_code,
         rucc_group)|>
  add_row(
    GEOID = "02158", 
    rucc_2013_code = 9,       # Historically RUCC 9 (completely rural)
    rucc_group = "Remote Rural"
  ) |>
  add_row(
    GEOID = "46102", 
    rucc_2013_code = 6,       # Historically RUCC 6 (nonmetro adjacent)
    rucc_group = "Exurban/Adjacent Rural"
  )
head(rucc_final)

# Merge county population table with RUCC table
county_demographics <- county_data_pop |>
  left_join(rucc_final, by = "GEOID")

#Filter out any non-contiguous states and replace old FIPS with their new counterparts
county_demographics <- county_demographics |>
  mutate(GEOID = case_when(
    # South Dakota Fix
    GEOID == "46113" ~ "46102", # Shannon -> Oglala Lakota
    # Virginia Fix (Independent City)
    GEOID == "51678" ~ "51163", # Lexington -> Rockbridge
    GEOID == "51515" ~ "51019", # Bedford City to Bedford County
        TRUE ~ GEOID
  ))|>
  filter(!startsWith(GEOID, "72"), #This filter removes non-contiguous states
         !startsWith(GEOID, "15"),
         !startsWith(GEOID, "02"),
         GEOID != "11001")
str(county_demographics)

#Print table for reference
write.csv(county_demographics, 
          here("data", "processed", "acs_county_demographics_5yravg_2009_2014_2019.csv"), 
          row.names = FALSE)




###############################################################################
##Finally, merge the 3 datasets

# #Read in the 3 data frames to merge
# county_demographics <- read.csv(
#   "acs_county_demographics_5yravg_2015-2019.csv")
# 
# climate_data <- read.csv("noaa_climate_county_5yravg_2015-2019.csv")
# climate_data <- climate_data|>
#   mutate(GEOID = full_fips)|>
#   select(!full_fips)|>
#   select(!state_name)
# 
# contiguous_migration <- read.csv("contiguous_county_migration.csv")
# glimpse(contiguous_migration)


## First, check that the GEOID columns are compatible
#Extract unique IDs
conflicts_prefer(base::intersect)
conflicts_prefer(base::union)

ids_climate <- unique(climate_data$GEOID)
ids_migration <- unique(migration_stats$GEOID)
ids_demographics <- unique(county_demographics$GEOID)

#Find specific overlaps
all_three <- intersect(intersect(ids_climate, ids_migration), ids_demographics)

climate_migration_only <- setdiff(intersect(ids_climate, ids_migration), ids_demographics)
climate_demo_only      <- setdiff(intersect(ids_climate, ids_demographics), ids_migration)
migration_demo_only    <- setdiff(intersect(ids_migration, ids_demographics), ids_climate)

#Identify nique to only one dataset
only_climate <- setdiff(ids_climate, union(ids_migration, ids_demographics))
only_migration <- setdiff(ids_migration, union(ids_climate, ids_demographics))
only_demo <- setdiff(ids_demographics, union(ids_climate, ids_migration))

# 3. Create a Summary Table
overlap_summary <- data.frame(
  Category = c("Shared by ALL", 
               "Climate & Migration Only", "Climate & Demog Only", "Migration & Demog Only",
               "Only Climate", "Only Migration", "Only Demographics"),
  Count = c(length(all_three), 
            length(climate_migration_only), length(climate_demo_only), length(migration_demo_only),
            length(only_climate), length(only_migration), length(only_demo))
)

print(overlap_summary)
print(migration_demo_only)
print(only_climate)
print(only_migration)
print(only_demo)

#Check NAs and county numbers:
master_data <- climate_data |>
  inner_join(migration_stats, by = c("GEOID", "year")) |>
  inner_join(county_demographics, by = c("GEOID", "year"))

# Check your final dimensions
nrow(master_data)
glimpse(master_data)
summary(master_data)

##
county_demog_climate_migration <- county_demographics|>
  left_join(climate_data, by = c("GEOID", "year"))

county_demog_climate_migration <- county_demog_climate_migration|>
  left_join(migration_stats, by = c("GEOID", "year"))

county_demog_climate_migration <- county_demog_climate_migration|>
  mutate(rucc_code_comb = case_when(
    rucc_group == "Remote Rural" ~ 0,
    rucc_group == "Exurban/Adjacent Rural" ~ 1,
    rucc_group == "Suburban/Small Metro" ~ 2,
    rucc_group == "Urban Core" ~ 3,
    TRUE ~NA
  ))|>
  select(!rucc_group)|>
  #  select(!rucc_2023_code) |>
  mutate(rucc_code_comb = factor(rucc_code_comb, 
                                 levels = c(0, 1, 2, 3),
                                 labels = c("Remote Rural", "Exurban/Adjacent Rural", "Suburban/Small Metro", "Urban Core")))

# test <- county_demog_climate_migration|>
#    filter(Net_Migration == "")
# print(test)

write.csv(county_demog_climate_migration, 
          here("data", "processed", "pa5046_county_full_data_2009_2014_2019.csv"), 
          row.names = FALSE)


# Save as a .dta file instead of .csv
write_dta(county_demog_climate_migration, 
          here("data", "processed", "county_full_data_2009_2014_2019.dta")
          )

##########################################################################
## Now, do a data check across all individual years to check for coding mistakes
# before we pool all the years of data

#First, a simple summary table of national averages for each statistic for each year
summary_by_year <- county_demog_climate_migration %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
# Visual scan for dissimilar numbers
View(summary_by_year)

#Second, facet grids of plots

#total_pop, median_hh_inc, temp_roll_avg_5yr, net_migration
county_demog_climate_migration %>%
  # Select year and the variables you want to check
  select(year, total_pop, median_hh_inc, temp_roll_avg_5yr, net_migration) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  group_by(year, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_val)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "National Annual Trends (2015-2019) 1",
       subtitle = "Looking for unusual spikes or coding errors")

#median_age, median_rent, median_home_val, unemployment_rate
county_demog_climate_migration %>%
  # Select year and the variables you want to check
  select(year, median_age, median_rent, median_home_val, unemployment_rate) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  group_by(year, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_val)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "National Annual Trends (2015-2019) 2",
       subtitle = "Looking for unusual spikes or coding errors")

#pct_renter, pct_bachelors, fiveyr_avg_temp_diff_5yr_ago, fiveyr_avg_precip_diff_5yr_ago
county_demog_climate_migration %>%
  # Select year and the variables you want to check
  select(year, pct_renter, pct_bachelors, fiveyr_avg_temp_diff_5yr_ago, fiveyr_avg_precip_diff_5yr_ago) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  group_by(year, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_val)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "National Annual Trends (2015-2019) 3",
       subtitle = "Looking for unusual spikes or coding errors")

#precip_roll_avg_5yr, gross_migration, net_migration, rucc_2013_code
county_demog_climate_migration %>%
  # Select year and the variables you want to check
  select(year, precip_roll_avg_5yr, gross_migration, net_migration, rucc_2013_code) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  group_by(year, variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_val)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "National Annual Trends (2015-2019) 4",
       subtitle = "Looking for unusual spikes or coding errors")


################################################################################
## Now pool the county-level data to get single numbers for the time period 2015-2019

# Create the pooled county-level dataset
county_pooled <- county_demog_climate_migration %>%
  # Group by the variables that identify the county and its static traits
  group_by(GEOID) %>%
  # Average all the numeric value columns
  summarise(
    across(
      c(total_pop, median_age, median_hh_inc, median_rent, 
        median_home_val, unemployment_rate, pct_renter, 
        pct_bachelors, moved_in_past_year, temp_roll_avg_5yr, 
        range_roll_avg_5yr, fiveyr_avg_temp_diff_5yr_ago, 
        precip_roll_avg_5yr, fiveyr_avg_precip_diff_5yr_ago, 
        net_migration, gross_migration),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop" # Clean up the grouping metadata
  )

# Check the results
glimpse(county_pooled)


write.csv(county_pooled, 
          here("data", "processed", "pa5046_county_full_data_pooled_2009_2014_2019.csv"), 
          row.names = FALSE)


# Save as a .dta file instead of .csv
write_dta(county_pooled, 
          here("data", "processed", "county_full_data_pooled_2009_2014_2019.dta")
)


################################################################################
## Now to make descriptive statistics tables for 2019 and 2009/2015/2019

#First we define the set of variables we want in our table
vars_to_summarize <- c(
  "total_pop", "median_age", "median_hh_inc", 
  "unemployment_rate", "pct_renter", "pct_bachelors", 
  "temp_roll_avg_5yr", "fiveyr_avg_temp_diff_5yr_ago", 
  "precip_roll_avg_5yr", "fiveyr_avg_precip_diff_5yr_ago", 
  "net_migration", "gross_migration"
)

min_vars_to_summarize <- c(
  "net_migration",
  "temp_roll_avg_5yr", "fiveyr_avg_temp_diff_5yr_ago", 
  "precip_roll_avg_5yr", "fiveyr_avg_precip_diff_5yr_ago",
  "total_pop", "median_age","pct_bachelors" 
)

# Summarize larger set of variables
#1. National Averages for 2019 Only - More Vars
stats_2019 <- county_demog_climate_migration |>
  filter(year == 2019) |>
  summarise(across(all_of(vars_to_summarize), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean 2019")

#2. Pooled National Averages for 2015-2019 - More Vars
stats_pooled <- county_demog_climate_migration |>
  summarise(across(all_of(vars_to_summarize), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean Pooled 2009, 2014, 2019")

# Get the observation count for 2019 and the pooled period
n_2019 <- county_demog_climate_migration |> filter(year == 2019) |> nrow()
n_pooled <- nrow(county_demog_climate_migration)

# Dataframe for observation count N
n_row <- data.frame(
  Variable = "Observations (N)",
  "Mean 2019" = n_2019,
  "Mean Pooled 2009, 2014, 2019" = n_pooled,
  check.names = FALSE               #Allows space in column names
)

# Basic Table with 2019 and Pooled - More Vars
desc_stats_table_more <- stats_2019 |>
  left_join(stats_pooled, by = "Variable")|>
  mutate(across(where(is.numeric), ~ round(.x, 3)))  #rounds every row to 3 decimal places
desc_stats_table_more_with_n <- bind_rows(desc_stats_table_more, n_row)
print(desc_stats_table_more_with_n)


# Summarize smaller set of variables
#1. National Averages for 2019 Only - Fewer Vars
stats_2019_m <- county_demog_climate_migration |>
  filter(year == 2019) |>
  summarise(across(all_of(min_vars_to_summarize), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean 2019")

#2. Pooled National Averages for 2009, 2014, 2019 - Fewer Vars
stats_pooled_m <- county_demog_climate_migration |>
  summarise(across(all_of(min_vars_to_summarize), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean Pooled 2015-19")

# Basic Table with 2019 and Pooled - Fewer Vars
desc_stats_table_min <- stats_2019_m |>
  left_join(stats_pooled_m, by = "Variable")|>
  mutate(across(where(is.numeric), ~ round(.x, 3)))  #rounds every row to 3 decimal places
desc_stats_table_min_with_n <- bind_rows(desc_stats_table_min, n_row)
print(desc_stats_table_min_with_n)

# Now calculate RUCC shares
#1. for 2019
rucc_shares_2019 <- county_demog_climate_migration |>
  filter(year == 2019) |>
  group_by(rucc_code_comb) |>     #Groups by label, not number code
  summarise(n = n()) |>
  mutate(
    share_2019 = n / sum(n),
    Variable = as.character(rucc_code_comb) #makes the row label the text
  ) |>
  select(Variable, share_2019)

#2. for the pooled data
rucc_shares_pooled <- county_demog_climate_migration |>
  filter(year %in% years_range) |>
  group_by(rucc_code_comb) |>
  summarise(n = n_distinct(GEOID)) |>
  mutate(
    share_pooled = n / sum(n),
    Variable = as.character(rucc_code_comb)
  ) |>
  select(Variable, share_pooled)

# Join and print
rucc_table_labeled <- rucc_shares_2019 |>
  left_join(rucc_shares_pooled, by = "Variable") |>
  rename(
    `Mean 2019` = share_2019,
    `Mean Pooled 2009, 2014, 2019` = share_pooled
  )
print(rucc_table_labeled)