#!/bin/bash 


## Export environment variables in .env into the shell
export $(egrep -v '^#' .env | xargs)

## For Data Catalog

# State adjusted CSV
aws s3 cp   data/intermediate-data/housing_index_state_adj.csv "s3://${covid_rental_risk_bucket}/housing_index_state_adj.csv"
aws s3api put-object-acl --acl public-read --bucket "${covid_rental_risk_bucket}" --key housing_index_state_adj.csv


# State adjusted geoJSON
aws s3 cp data/intermediate-data/housing_index_state_adj.geojson "s3://${covid_rental_risk_bucket}/housing_index_state_adj.geojson"
aws s3api put-object-acl --acl public-read --bucket "${covid_rental_risk_bucket}" --key housing_index_state_adj.geojson


# Data dictionary
aws s3 cp data/intermediate-data/data_dictionary_housing_data_index_state_adj.csv "s3://${covid_rental_risk_bucket}/data_dictionary_housing_data_index_state_adj.csv"
aws s3api put-object-acl --acl public-read --bucket "${covid_rental_risk_bucket}" --key data_dictionary_housing_data_index_state_adj.csv

## For feature

# State adjusted Geojson
aws s3 cp data/intermediate-data/housing_index_state_adj_feature.geojson "s3://${covid_rental_risk_bucket}/housing_index_state_adj_feature.geojson"

# County Geojson
aws s3 cp data/intermediate-data/counties.geojson "s3://${covid_rental_risk_bucket}/counties.geojson"

# CoC Geojson
aws s3 cp data/intermediate-data/coc_geographies_states_split.geojson.geojson "s3://${covid_rental_risk_bucket}/coc_geographies_states_split.geojson.geojson"


## Unused for now

# CSV for Data Catalog
aws s3 cp data/intermediate-data/housing_index_state_natl.csv "s3://${covid_rental_risk_bucket}/housing_index_state_natl.csv"
# Geojson for data catalog
aws s3 cp data/intermediate-data/housing_index_natl.geojson "s3://${covid_rental_risk_bucket}/housing_index_natl.geojson"


