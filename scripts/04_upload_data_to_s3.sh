#!/bin/bash 

## Write out full state adjusted data

# CSV for Data Catalog
aws s3 cp data/intermediate-data/housing_index_state_adj.csv s3://ui-covid-housing-risk-indicators/housing_index_state_adj.csv
# Geojson for data catalog
aws s3 cp data/intermediate-data/housing_index_state_adj.geojson s3://ui-covid-housing-risk-indicators/housing_index_state_adj.geojson
# Geojson for feature
aws s3 cp data/intermediate-data/housing_index_state_adj_feature.geojson s3://ui-covid-housing-risk-indicators/housing_index_state_adj_feature.geojson


# CSV for Data Catalog
aws s3 cp data/intermediate-data/housing_index_state_natl.csv s3://ui-covid-housing-risk-indicators/housing_index_state_natl.csv
# Geojson for data catalog
aws s3 cp data/intermediate-data/housing_index_natl.geojson s3://ui-covid-housing-risk-indicators/housing_index_natl.geojson


