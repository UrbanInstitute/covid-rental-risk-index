# covid-rental-risk-index
 
Date Last Updated: XXX

This repo contains the data, and code neccesary to generate the tract level 
Rental Assistance Priority Indexes which power [this](https://www.urban.org/features/where-prioritize-emergency-rental-assistance-keep-renters-their-homes) interactive Urban
feature. The final data can also be downloaded from our
Data Catalog [here](https://datacatalog.urban.org/dataset/rental-assistance-priority-index).

 The Rental Assistance Priority Index is composed of three subindixes
(Covid Risk, Housing Instability Risk, and Equity) and each of the subindexes
are composed of a few indicators. The indicators and the data sources they are
pulled from are listed below. 

## Housing Instability Risk Subindex 
- Share of people living in poverty: percentage of the population living below the federal poverty level during the past 12 months (2014–18 ACS data, table C17002)

- Share of renter-occupied housing units: percentage of occupied housing units that are renter-occupied (2014–18 ACS data, table B25003)

- Share of severely cost-burdened low-income renters: percentage of households whose annual incomes are less than $35,000 and pay 50 percent or more of their incomes in gross rent (2014–18 ACS data, table B25074)

- Share of severely overcrowded households: percentage of renter-occupied households with more than 1.5 occupants per room (2014–18 ACS data, table B25014)

- Share of unemployed people: percentage of the labor force that is unemployed (2014–18 ACS data, table B12006)

## COVID-19 Impact Subindex 

- Share of adults without health insurance: percentage of noninstitutionalized people ages 19 to 64  who do not have health insurance (2014–18 ACS data, table C27012) 

- Share of low-income jobs lost to COVID-19: Among residents with jobs that pay $40,000 or less, the estimated percentage who have lost their jobs since Feb 2020.  (July 2020 update to Urban’s “Where Low-Income Jobs Are Being Lost to COVID-19”) 


## Equity Subindex

- Share of people of color: percentage of people designated in the dataset as a race or ethnicity other than white, non-Hispanic (2014–18 ACS data, table B03002)

- Share of extremely low–income renter households: percentage of renter-occupied households that earn 30 percent of area median income or less (2012–16 CHAS tabulations, table 8)

- Share of households receiving public assistance: Percentage of households whose income in the past 12 months has included assistance from public benefit programs such as Temporary Assistance for Needy Families (TANF) or Supplemental Nutrition Assistance Program (SNAP) (2014–18 ACS data, table B19057)

- Share of people born outside the United States: percentage of the population born outside the United States (2014–18 ACS data, table B05002)



## Methodology Overview

After each of the indicators are converted to percentages, we take the state indexed z-score of all the indicators. So for a tract in CA with 10% of people born outside the US, we subtract off the mean percent of people born outside the US for all tracts in  CA and divide by the SD of the percent of people born outside of the US for all tracts in CA. We then construct the subindexes by taking weighted averages across our indicators, with the weights shown below. Finally the total index is a weighted average across all the subindexes. Note that we intentionally upweighted the percent of people of color within a tract relative to the other equity metrics as we explicitly wanted this index to take into account the disparate impacts on communities of color.

![Image of Weights](/images/indicator_table.png)

For more information on our methodology, please see our [technical appendix (PDF)](https://www.urban.org/sites/default/files/2020/08/24/where_to_prioritize_emergency_rental_assistance_to_keep_renters_in_their_homes_technical_appendix.pdf).

## Scripts

Below is a short description of the files in the `scripts\` folder:


- `01_generate_index_variables.R`: This reads in the data from our 3 sources (5
  year ACS, HUD CHAS, and Urban's Low Income Job Loss tool), converts all the
  indicators to percentages using the relevant numerator/demoninator, then
  calculates the state indexed z-scores, and weighted subindex and Total index
  values. Note sometimes the Census FTP site may be down and this script will
  error out with an error that looks something like `Error in
  curl::curl_fetch_memory(url, handle = handle) : Could not resolve host:
  api.census.gov`. If this happens, we suggest waiting a few minutes and trying
  again.

- `02_generate_geographies.R`: This generates geojsons of counties, states, and
  CoC's (Continuums of Care) to display on the map. CoC files are
  downlaoded in from HUD's geodatabase. For the 4 CoC's that crossed state
  boundaries, we chose to split up those CoCs into the parts of the CoC that
  fell within each state (ie COC 1 (CA Part) and COC 1 (NV Part)) so that users
  would not be tempted to compare index values across states; Since the index
  values are based on state specific z scores, index and subindex values cannot
  be compared across states.
  
  While we tried to to the CoC -> state spatial intersection in R, this just
  took too much time (>5 hour runtime) possibly due to malformed geometries in
  the HUD geodatabase. So we did the spatial intersection in ArcGIS using the
  same states and CoC shapefiles (took around 5 minutes). The output file of
  this ArcGIS spatial intersection is
  `data/raw-data/coc-geographies/coc_state_join/coc_state_join.shp`. That is the
  file which is read in, and filtered to get rid of intersections that are just
  long thin state borders due to slightly mismatched boundaries. After
  inspection, we manually specify the 4 CoC's that need to be split across
  multiple states and
  append their state to the `COCNAME` column before writing out. 

- `03_create_appendix_plots.R`: Creates correlation matrices of the indicators
  and histograms of the indices for the technical appendix.

- `04_upload_data_to_s3.sh`: Uploads files to an Urban owned S3 bucket. You
  likely will not need to run this script unless you also want the output files
  to be on S3. If you do run this, you will need to change the S3 output
  locations and specify your own AWS credentials

### Questions?

Reach out to anarayanan@urban.org




