# final Great Plains scripts

0_PrepData.R: This script formats the data provided by EPA. Handles NaNs, checks for outliers, splits into training/testing, performs data augmentation (upsampling), etc.

1.0_Screening.R: This scripts checks if any of the candidate metrics are too homogenous to be considered further in the analysis (ie, if data are all zeros, it will be dropped here)

2.0_PrelimModels.R: This is the main script that generates the random forest model. Depending on what case it is, there will be some nuances (ie the GP_S no GIS model should only allow samples from the South, and no GIS metrics should be included)