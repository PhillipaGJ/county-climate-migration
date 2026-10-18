# county-climate-migration
R pipeline merging Census ACS, NOAA climate, and USDA data into a county-level migration panel

# County Climate and Migration Dataset

R pipeline that builds a county-level panel (about 3,100 contiguous-U.S.
counties, three 5-year periods: 2005-09, 2010-14, 2015-19) combining
migration, climate, and demographics. Built for a graduate econometrics
course (UMN, Spring 2026) to study how temperature relates to migration.

## Data sources
- Census ACS 5-year county-to-county migration flows (tidycensus get_flows());
  2005-09 from Census working paper tables (Benetsky, 2012) from https://www.census.gov/library/working-papers/2012/demo/benetsky-02.html
- ACS 5-year demographics (tidycensus get_acs())
- NOAA nClimDiv county temperature and precipitation (monthly)
- USDA ERS Rural-Urban Continuum Codes (2013)

## Steps
1. Pull and aggregate migration flows
2. Clean NOAA files, compute 5-year rolling averages
3. Pull demographics, harmonize variables across survey years
4. Reconcile county FIPS changes, merge, run consistency checks

## Status
Work in progress; validation checks are ongoing. Confirming that all climate data is being counted for exactly one county and adding additional in-code validation.

## To run
Open the .Rproj, put the raw files in data/raw/, run code/01_build_dataset.R.
