################################################################################
# Restore or recreate the final model (No GIS, Unstratified, v5 refinement)
# Generate predictions on new data
# Information on final metrics used
#
################################################################################
library(randomForest)
library(tidymodels)
library(caret)

################################## GET DATA ####################################
#
################################################################################
# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Get input dataset (contains augmented training, testing data)
df_input <- read_csv(paste0(HOME_DIR,"/input/processed/df_model_aug.csv"))

# Explicitly set data type of prediction variable
df_input$Class <- as.factor(df_input$Class)

############################## CREATE NEW METRICS ##############################
df_input <- df_input %>% mutate(
  TotalAbund_0_10 = case_when(TotalAbundance==0~0, 
                              ((TotalAbundance>0) & (TotalAbundance<10)~1),
                              TotalAbundance>=10~2),
  UplandRooted_PA = case_when(UplandRootedPlants_score<3~0, T~1),
  hydrophytes_2 = case_when(hydrophytes_present<2~0, T~1)
) 

# Separate datasets
df_MODEL <- df_input %>% filter(Dataset=="Training")
df_TEST <- df_input %>% filter(Dataset=="Testing")

#################### INFO ON THE FINAL METRICS FOR WILL ########################
#NOTE: feed the list of "current metrics" in the same order that the model was
#originally created in order to replicate the results exactly. Otherwise you may
#receive slightly different results (similar to a different random seed)

list_of_predictors <- c( "BankWidthMean",
                        "SubstrateSorting_score",
                        "DifferencesInVegetation_score",
                        "RifflePoolSeq_score",
                        "SedimentOnPlantsDebris_score", 
                        "UplandRooted_PA",
                        "hydrophytes_2",
                        "TotalAbund_0_10" )

# Additional context on these metrics for Will below...
#
#
# BankWidthMean: Mean of columns that start with 'Bankwidth' (units = meters)
#       Notes: continuous, should be greater than 0, be clear on units?
#
# SubstrateSorting_score: 0.0: Particle sizes in channel are similar to particle
#       sizes in areas close to but not in the channel. Substrate sorting is not
#       readily observed in the stream channel; 1.5: Particle sizes in channel 
#       are moderately similar to sizes close to but not in the channel. Various 
#       sized substrates are present in the stream channel and are represented 
#       by a higher ratio of larger particles (e.g., gravels, cobbles); 3.0: 
#       Particle sizes in channel are noticeably different from particles close 
#       to but not in the channel. There is a clear distribution of various-sized 
#       substrates in the stream channel, with finer particles acucmulating in 
#       pools and larger particles accumuluating in riffles or runs. Midpoint 
#       scores (e.g., 0.25, 0.75, and 1.25) are possible.
#
# DifferencesInVegetation_score:0: No compositional or density differences in 
#       vegetation are present between the streambanks and adjacent uplands; 1: 
#       Vegetation growing along the reach may occur in greater densities or grow
#       more vigorously than vegetation in the adjacent uplands, but there are no
#       dramatic compositional differences between the two; 2: A distinct riparian
#       vegetation corridor exists along part of the reach. Riparian vegetation 
#       is interspersed with upland vegetation along the length of the reach; 3: 
#       Dramatic compositional differences in vegetation are present between the 
#       stream banks and adjacent uplands. A distinct riparian corridor exists 
#       along the entire reach. Riparian, aquatic, or wetland species dominate 
#       the length of the reach.  Midpoint scores (e.g., 0.5, 1.5 and 2.5) are 
#       possible.
#
# RifflePoolSeq_score: 0: There is no structural (e.g., riffle-pool) sequence 
#       exhibited; 1: Distinction between structures (e.g., riffles and pools) 
#       is difficult and infrequent throughout the reach; 2: Distinction between 
#       structures (e.g., riffles and pools) is difficult, but more than one 
#       transition occurs in the reach; 3: Demonstrated by a frequent number of 
#       structural transitions (e.g., riffles followed by pools) along the entire 
#       reach. There is an obvious transition between structures. Midpoint scores 
#       (e.g., 0.5, 1.5, and 2.5) are possible.
#
# SedimentOnPlantsDebris_score: 0.0: No fine sediment is present on plants or 
#       debris; 0.5: Fine sediment is isolated in small amounts along the stream;
#       1.0: Fine sediment found on plants or debris within the stream channel, 
#       although it is not prevalent along the stream. Mostly accumulating in 
#       pools; 1.5: Fine sediment found readily on plants and debris within the 
#       stream channel, on the streambank, and within the floodplain throughout 
#       the length of the stream. Midpoint scores (e.g., 0.25, 0.75, and 1.25) 
#       are possible.
#
# UplandRooted_PA: This is a "presence/absence" metric. It's a refined metric (ie 
#  we developed it later, during model development) based off of the metric 
#  UplandRootedPlants_score, which was an ordinal metric. The definition of 
#  UplandRootedPlants_score is: 
#       0: Rooted upland plants are prevalent within the streambed; 1: Rooted 
#       upland plants are consistently dispersed throughout the streambed; 2: 
#       There are a few rooted upland plants present within the streambed; 3: 
#       Rooted upland plants are absent from the streambed. Midpoint scores 
#       (e.g., 0.5, 1.5, and 2.5) are possible.
#
# hydrophytes_2: This is a "binned" metric - ie we refined the original continuous
#  metric (hydrophytes_present) into 2 groups: less than 2 hydrophytes or 2 or 
#  more hydrophytes. The definition of hydrophytes_present is:
#       Number of hydrophytic plant species (FACW, OBL or SAV) observed within 
#       the study reach channel and 1/2 channel width of the stream on either bank
#
# TotalAbund_0_10: This is a "binned" metric - ie we refined the original continuous
#  metric (TotalAbundance) into 3 bins: equal to 0, 1-10, 10+. The defintion of 
#  TotalAbundance is: 
#       Total abundance of aquatic invertebrates (integer).
#
################################################################################

################################## GET MODEL ###################################
# How to recreate the trained model or just read it in from rbs file
# Generate predictions
################################################################################

set.seed(1111)

training_data <- df_MODEL[, c("Class", list_of_predictors)]

# Regenerate the model OR...
RF <- randomForest(Class~.,
                   data=training_data,
                   ntree=1500,
                   importance=T,
                   proximity=T)

# ...Restore the object
RF <- readRDS("RF_NoGIS_Unstrat_5.rds")


set.seed(1111)
train_results <- df_MODEL %>% 
  ### GET OOB PREDICTIONS
  # Append the predictions (using majority voting) to the training df
  # add_column(RF_Prediction_Majority = RF$predicted) %>% #OOB
  # # Append the associated OOB prediction probabilities to the training df
  # bind_cols(predict(RF, type="prob") %>%
  #             as_tibble() ) %>%
  
  ### GET In-Bag PREDICTIONS (by using 'newdata' argument)
  add_column(RF_Prediction_Majority = 
               predict(RF, 
                       newdata = training_data) #In-Bag prediction
  ) %>%
  bind_cols(predict(RF, 
                    newdata = training_data, #In-Bag probabilities
                    type="prob") %>%  
              as_tibble() ) %>%
  filter(Notes!="Augmented")


### TEST DATA
new_data <- df_TEST[, c("Class", list_of_predictors)]

set.seed(1111)
# Test results
test_results <- df_TEST %>%
  add_column(RF_Prediction_Majority = 
               predict(RF, 
                       newdata = new_data) #prediction
  ) %>%
  bind_cols(predict(RF, 
                    newdata = new_data, #probabilities
                    type="prob") %>%  
              as_tibble() )

# Reclassify testing data with 50% minimum probability
full_results <- rbind(train_results, test_results)
full_results <- full_results %>% mutate(
  RF_Prediction_50=case_when(
    E>=.5~"E",
    I>=.5~"I",
    P>=.5~"P",
    P>E~"ALI",
    E>P~"LTP",
    P==E & I>P~"NMI", 
    P==E & I<=P~"NMI",
    T~"Other"),
  #Identify correct classifications
  EvALI_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
    T~F),
  PvIvE_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I") & RF_Prediction_50 %in% c("I")~T,
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    T~F)
)
# write_csv(full_results, file="full_results.csv")
write_csv(full_results %>%select("ParentGlobalID", 
                                 "SiteCode",
                                 "CollectionDate",
                                 "Dataset",
                                 "Notes",
                                 "Class",
                                 "TotalAbundance",
                                 "UplandRootedPlants_score",
                                 "hydrophytes_present",
                                 list_of_predictors,
                                 "E","I","P",
                                 "RF_Prediction_50"
                                 ), 
          file="GreatPlainsFinalResults.csv")

################################## QC RESULTS ################################## 
# Make sure results are reproducable by taking a random subset of data
# and check that the predictions are the same as they were originally
################################################################################

df_random_sample <- sample_n(df_input, 100)

# Create new predictions using the 'df_newsubset'
set.seed(1111)

originalpred <- full_results %>% 
    filter(ParentGlobalID%in%unique(df_random_sample$ParentGlobalID)) %>%
    select(ParentGlobalID, RF_Prediction_50)

# Create dataframe to store testing results (ie 'newpredictions')
random_results <- df_random_sample %>%
  # Append the predictions (using majority voting) to the testing df
  add_column(RF_Prediction_Majority= predict(RF, 
          newdata=df_random_sample[, c("Class", list_of_predictors)])
  ) %>%
  # Also append the associated probabilities
  bind_cols(
    predict(RF,
            newdata=df_random_sample[, c(list_of_predictors)], 
            type="prob") %>%
      as_tibble()
  ) %>% 
  mutate(
    #Reclassify with 50% minimum probability
    QC_RF_Prediction=case_when(
      E>=.5~"E",
      I>=.5~"I",
      P>=.5~"P",
      P>E~"ALI",
      E>P~"LTP",
      P==E & I>P~"NMI", 
      P==E & I<=P~"NMI",
      T~"Other")
  ) %>% select("ParentGlobalID",
               "SiteCode",
               "CollectionDate",
               "Class",
               "Dataset",
               "QC_RF_Prediction")

random_results2 <- merge(x = random_results, 
                         y = originalpred, 
                         by = "ParentGlobalID", 
                         all.x = TRUE)

# True/False, does the prediction match the original prediction?
random_results2$Check <- random_results2$QC_RF_Prediction ==
    random_results2$QC_RF_Prediction

# write_csv(random_results2, file="QC_predictions.csv")



# #################### Extra, create a confusion matrix ######################
# library(reshape2)
# 
# ## FINAL CONFUSION MATRIX
# results_conf_mat <- full_results %>% group_by(Class, Dataset)
# results_conf_mat$Class <- as.factor(results_conf_mat$Class)
# results_conf_mat$Dataset <- as.factor(results_conf_mat$Dataset)
# results_conf_mat$RF_Prediction_50 <- as.factor(results_conf_mat$RF_Prediction_50)
# results_conf_mat2 <- results_conf_mat %>%
#   group_by( Class, Dataset, RF_Prediction_50)%>%
#   summarise(n=length(SiteCode)) %>%
#   rename(ActualClass="Class")
# results_conf_mat3 <- spread(results_conf_mat2, key = RF_Prediction_50, value = n)
# results_conf_mat3 <- results_conf_mat3 %>% arrange(Dataset)
# melted <- melt(results_conf_mat3)
# cm_pivot_table <- melted %>%
#   pivot_wider(
#     names_from = c(ActualClass, Dataset),
#     values_from = value
#   ) %>% arrange(factor(variable, levels = c('E', 'I', 'ALI', 'P', 'LTP', 'NMI')))
# 
# cm_pivot_table