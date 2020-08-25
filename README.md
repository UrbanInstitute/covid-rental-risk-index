# covid-housing-risk-index
 

This repo contains the data, and code necesarry to genreate the tract level 
Rental Assistance Prioriy Indexes which power [this] inetractive Urban feature.  The Rental Assistance Priority Index is composed of three subindixes (Covid Risk, Housing Instability Risk, and Equity) and each of the subindexes are composed of a few indicators. The indicators and the data sources they are pulled from are listed below. 

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

After each of the indicators are converted to percentages, we take the state indexed z-score of all the indicators. So for a tract in CA with 10% of people born outside the US, we subtract off the mean percent of people born outside the US for all tracts in  CA and divide by the SD of the percent of people born outside of the US for all tracts in CA. We then construct the subindexes by taking weighted averages across our indicators, with the weights shown below. Finally the total index is a weighted average across all the subindexes. Note that we intentionally upweighted the percent of people of color within a tract relative to the other equity metrics as we exiplicitly wanted this index to take into account the disparate impacts on communities of color.

![Image of Weights](/images/indicator_table.png)

## Scripts

Below is a short description of the files in the `scripts\` folder:


- `01_genereate_index_variables.R`: This reads in the data from our 3 sources (5 year ACS, HUD CHAS, and Urban's Low Income Job Loss tool), converts all the indicators to percentages using the relevant numerator/demoninator, then calculates the state indexed z-scores, and weighted subindex and Total index values. 

- `04_generate_geographies.R`: This generates geojsons of counties, states, and CoC's (Continuums of Care) for Comms to display on the map. CoC files are downlaoded in from HUD's geodatabase. For the 4 CoC's that crossed state boundaries, we chose to split up those CoCs into the parts of the CoC that fell within each state (ie COC 1 (CA Part) and COC 1 (NV Part)) so that users would not be tempted to compare index values across states; Since the index values are based on state specific z scores, index and subindex values cannot be comapred across states. While we tried to to the CoC -> state spatial intersection in R, this just took too much time (>5 hour runtime) possibly due to malformed geometries. So we asked Rob Pitingolo to do the CoC to states spatial intersection in ArcGIS using the same states and CoC shapefiles (took 5 minutes). The output file of his analysis is `data/raw-data/coc-geographies/coc_state_join/coc_state_join.shp`. That is the file which is read in, and filtered (to get rid of intersections that are just long thin state borders due to slightly mismatched boundaries), and then used to identify the 4 CoCs that cross state boundaries. After inspection, we manually specify the 4 CoC's that need to be split across multiple states and append thier state to the `COCNAME` column before writing out/

### Questions?

Reach out to anarayanan@urban.org




