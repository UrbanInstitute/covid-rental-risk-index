#!/bin/bash 


## Export environment variables in .env into the shell
export $(egrep -v '^#' .env | xargs)

## Write out full state adjusted data

# CSV for Data Catalog
aws s3 cp   data/intermediate-data/housing_index_state_adj.csv "s3://${covid_rental_risk_bucket}/housing_index_state_adj.csv"
aws s3api put-object-acl --acl public-read --bucket "${covid_rental_risk_bucket}" --key housing_index_state_adj.csv


# Geojson for data catalog
aws s3 cp data/intermediate-data/housing_index_state_adj.geojson "s3://${covid_rental_risk_bucket}/housing_index_state_adj.geojson"
aws s3api put-object-acl --acl public-read --bucket "${covid_rental_risk_bucket}" --key housing_index_state_adj.geojson


# Geojson for feature
aws s3 cp data/intermediate-data/housing_index_state_adj_feature.geojson "s3://${covid_rental_risk_bucket}/housing_index_state_adj_feature.geojson"


# CSV for Data Catalog
aws s3 cp data/intermediate-data/housing_index_state_natl.csv "s3://${covid_rental_risk_bucket}/housing_index_state_natl.csv"
# Geojson for data catalog
aws s3 cp data/intermediate-data/housing_index_natl.geojson "s3://${covid_rental_risk_bucket}/housing_index_natl.geojson"


