###############################################################################
# This script performs a Sensitivity Analsysis on the errors for the final GP 
# model chosen (V5 refined)
#
# Note: last input dataset update from Rafi on 25 Jan 2024
###############################################################################
library(randomForest)
library(tidymodels)
library(caret)
library(tidyverse)

# Sets working directly to wherever this file is saved
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Get input dataset 
df_input <- read_csv(paste0(HOME_DIR,"/input/processed/df_model_aug.csv"))

####DROP AUGMENTED
#df_input <- df_input %>% filter(Notes !="Augmented")

# Explicitly set data type of prediction variable
df_input$Class <- as.factor(df_input$Class)

# Create new features needed by final model
df_input <- df_input %>% mutate(
  TotalAbund_0_10 = case_when(TotalAbundance==0~0, 
                              ((TotalAbundance>0) & (TotalAbundance<10)~1),
                              TotalAbundance>=10~2),
  UplandRooted_PA = case_when(UplandRootedPlants_score<3~0, T~1),
  hydrophytes_2 = case_when(hydrophytes_present<2~0, T~1)
) 

# Separate training vs testing datasets
df_TRAIN <- df_input %>% filter(Dataset=="Training")
df_TEST <- df_input %>% filter(Dataset=="Testing")


model_version <- "Final_GP_Model"
chosen_model <- "NoGIS_Unstrat"
chosen_version <- "V5"
parent_path <- paste0(HOME_DIR, "/output/models/", model_version,
                      "/", chosen_model,"/", chosen_version, "_", chosen_model)

output_path <- paste0(parent_path, "/FINAL_RESULTS")
  if (!dir.exists(output_path)){dir.create(output_path)}

################################################################################
# 1. Get RF model
# Re-create it here or read in the final GP model.
################################################################################
list_of_predictors <- c("BankWidthMean",
                        "SubstrateSorting_score",
                        "DifferencesInVegetation_score",
                        "RifflePoolSeq_score",
                        "SedimentOnPlantsDebris_score", 
                        "UplandRooted_PA",
                        "hydrophytes_2",
                        "TotalAbund_0_10")

training_data <- df_TRAIN[, c("Class", list_of_predictors)]

# Regenerate the model .
set.seed(1111)
RF <- randomForest(Class~.,
                   data=training_data,
                   ntree=1500,
                   importance=T,
                   proximity=T,
                   predict.all=TRUE)

## (Alternatively, can read in the old model from this file)
# RF <- readRDS("RF_NoGIS_Unstrat_5.rds")

# Create dataframe to store training results
train_results <- df_TRAIN %>% 
  ## IN-BAG PREDICTIONS
  add_column(RF_Prediction_Majority = 
               predict(RF, 
                       newdata = training_data) #In-Bag prediction
             ) %>%
  bind_cols(predict(RF, 
                    newdata = training_data, #In-Bag probabilities
                    type="prob") %>%  
              as_tibble() )
  # ## OOB PREDICTIONS
  # add_column(RF_Prediction_Majority = RF$predicted) %>%
  # bind_cols(predict(RF, 
  #                   type="prob") %>%  
  #             as_tibble() 
  #           )


# Saving on object in RData format
rdata_name <- paste0(output_path, "/GreatPlainsFinal.rds")
print(paste("Saving:", rdata_name))
saveRDS(RF, file=rdata_name)


# Create new predictions using the 'newdata'
set.seed(1111)
new_data <- df_TEST[, c("Class", list_of_predictors)]

## TEST DATA
test_results <- df_TEST %>%
  add_column(RF_Prediction_Majority = 
               predict(RF, 
                       newdata = new_data) #prediction
  ) %>%
  bind_cols(predict(RF, 
                    newdata = new_data, #probabilities
                    type="prob") %>%  
              as_tibble() )


full_results <- rbind(train_results, test_results)
full_results <- full_results %>% mutate(
  #Reclassify with 50% minimum probability
  RF_Prediction_50=case_when(
    E>=.5~"E",
    I>=.5~"I",
    P>=.5~"P",
    P>E~"ALI",
    E>P~"LTP",
    P==E & I>P~"NMI", 
    P==E & I<=P~"NMI",
    T~"Other"),
  EvALI_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
    T~F),
  PvIvE_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I") & RF_Prediction_50 %in% c("I")~T,
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    T~F),
  PvLTP_correct = case_when(
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    Class %in% c("I","E") & RF_Prediction_50 %in% c("I","E","LTP")~T,
    T~F)
  )

write_csv(full_results, file=paste0(output_path, "/GreatPlainsFinalResults.csv"))


############################## Great Plains Results ############################ 
full_results_GPfinal <- full_results  %>%
  add_column( model="GP final")
full_results_GPfinal$PredClass <- full_results_GPfinal$RF_Prediction_50

subset_cols <- c("ParentGlobalID",
                 "SiteCode",
                 "Class", 
                 "Dataset",
                 "PredClass",
                 "model",
                 "TotalVisits",
                 "Region_detail",
                 "Wet")

df_GPfinal_subset  <- full_results_GPfinal[, subset_cols]


# FINAL GREAT PLAINS CONFUSION MATRIX
results_conf_mat <- full_results_GPfinal %>% group_by(Class, Dataset)
results_conf_mat$Class <- as.factor(results_conf_mat$Class)
results_conf_mat$Dataset <- as.factor(results_conf_mat$Dataset)
results_conf_mat$PredClass <- as.factor(results_conf_mat$PredClass)
results_conf_mat2 <- results_conf_mat %>%
  group_by( Class, Dataset, PredClass)%>%
  summarise(n=length(SiteCode)) %>%
  rename(ActualClass="Class")
results_conf_mat3 <- spread(results_conf_mat2, 
                            key = PredClass, value = n)
results_conf_mat3 <- results_conf_mat3 %>% arrange(Dataset)
melted <- melt(results_conf_mat3)
cm_pivot_table <- melted %>%
  pivot_wider(
    names_from = c(ActualClass, Dataset),
    values_from = value
  ) %>% arrange(factor(variable, levels = c('E', 'I', 'ALI', 'P', 'LTP', 'NMI')))

write_csv(cm_pivot_table, file=paste0(output_path, "/ConfusionMatrix.csv"))
cm_pivot_table

########################### OTHER MODEL PERFORMANCE ############################ 
#Need additional columns
df_main0 <- read_csv("input/raw/df_main_20240125.csv")
df_main <- df_main0 %>% select(c("ParentGlobalID",
                                 "WaterInChannel_score",
                                 "Fish_score_NM",
                                 "BMI_score",
                                 "Algae_score",
                                 "springs_score_NM",
                                 "ChannelDimensions_score",
                                 "hydrophytes_present_any_noflag",
                                 "AlgalCover_Live"#,
                                 #"StreamOrder"
                                ))
full_results_addl <- merge(x = full_results, 
                           y = df_main, 
                           by = "ParentGlobalID", 
                           all.x = TRUE)

################################################################################
#  GP Beta Method
################################################################################
GP_beta <- readRDS(paste0(HOME_DIR,"/input/other/RF_V8_rmGOLDOCH_StratUNC.rds"))
GP_beta_predictors <- c("Sinuosity_score",
                       "SubstrateSorting_score",
                       "UplandRootedPlants_score",
                       "ChannelDimensions_score",
                       "BankWidth_cat",
                       "EPT_bin",
                       "hydrophyte_cat",
                       "PctShad_bin",
                       "Strata_UNC")

full_results_GPbeta0 <- full_results_addl %>% 
    mutate(
        EPT_bin= case_when(EPT_taxa<2~0, T~1),
        BankWidth_cat = case_when(BankWidthMean<20~0, T~1),
        PctShad_bin= case_when(PctShading<0.1~0, T~1),
        hydrophyte_cat = case_when(
          hydrophytes_present_any_noflag<2~0,
          T~1),
        Strata_UNC = case_when(Strata %in% c("GP_U","GP_N", "GP_C")~1, T~0),
    )

set.seed(555)
new_data_gp <- full_results_GPbeta0[, c("Class", GP_beta_predictors)]

# MAKE PREDICTION
df_GPbeta <- full_results_GPbeta0 %>%
  add_column(PredClass=predict(GP_beta, 
                               newdata = new_data_gp)) %>%
  add_column(model="GP beta")

df_GPbeta_subset  <- df_GPbeta[, subset_cols]


################################################################################
#  New Mexico Method
################################################################################

nm_vars <- c(
    "WaterInChannel_score", 
    "Fish_score_NM", 
    "BMI_score",
    "Algae_score", 
    "DifferencesInVegetation_score",
    "UplandRootedPlants_score",
    "Sinuosity_score", 
    "ChannelDimensions_score",
    "RifflePoolSeq_score",
    "SubstrateSorting_score", 
    "HydricSoils_score",
    "SedimentOnPlantsDebris_score",
    "springs_score_NM", 
    "ironox_bfscore_NM"
)


df_NM <- full_results_addl %>% 
  mutate(nm_score_final = full_results_addl %>%
           select(all_of(nm_vars)) %>% rowSums(),
         nm_class1 = case_when(nm_score_final<9~"E",
                               nm_score_final<12~"It",
                               nm_score_final<=19~"I",
                               nm_score_final<=22~"Pt",
                               nm_score_final>22~"P",
                               T~"error"),
         nm_class2 = case_when(nm_class1=="It"~"I",
                               nm_class1=="Pt"~"P",
                               T~nm_class1),
         NM_Prediction = case_when(nm_class2 %in% c("E","error") &
                               (Fish_score_NM+BMI_score)>0 ~ "ALI",
                               T~nm_class2)) %>%
      rename(PredClass="NM_Prediction") %>%
      add_column(model="NM")

df_NM_subset  <- df_NM[, subset_cols]


################################################################################
#  Arid West Method (Rafi sent 5/6/2024)
################################################################################
load("WM/all_refined_rf_mods.Rdata")
aw_model <- all_refined_rf_mods[[1]]

aw_vars <-c("DifferencesInVegetation_score", #Must be one of these: 0, 0.5, 1, 1.5, 2, 2.5, 3
             "AlgalCover_LiveOrDead_NoUpstream", #Binned during data collection: 0 (none detected), 1 (<2%), 2 (2-10%), 3 (10-40%), 4 (>40%)
             "BankWidthMean", #Must be a positive number (zero not allowed)
             "UplandRootedPlants_score", #Must be one of these: 0, 0.5, 1, 1.5, 2, 2.5, 3
             "RifflePoolSeq_score", #Must be one of these: 0, 0.5, 1, 1.5, 2, 2.5, 3
             "Slope", #Must be a number greater than or equal to zero [check with PDT?]
             "hydrophytes_present_0_1_2_3_4_5", #Must be one of these: 0, 1, 2, 3, 4, 5. Not provided by user.
             "perennial_ISAsubregion_abundance_simp2_0_5_10_20") #Must be one of these: 0, 1, 2, 3, 4. Not provided by user.

full_results_aw0 <- full_results_addl %>% mutate(
    hydrophytes_present_0_1_2_3_4_5 = case_when(
        hydrophytes_present<5~hydrophytes_present,
        T~5),
    perennial_ISAsubregion_abundance_simp2_0_5_10_20 = case_when(
        perennial_ISAsubregion_abundance==0~0,
        perennial_ISAsubregion_abundance<5~1,
        perennial_ISAsubregion_abundance<10~2,
        perennial_ISAsubregion_abundance<20~3,
        # perennial_West_abundance_simplified2==0~0,
        # perennial_West_abundance_simplified2<5~1,
        # perennial_West_abundance_simplified2<10~2,
        # perennial_West_abundance_simplified2<20~3,
        T~4)
    #TODO: Rafi used "perennial_West_abundance_simplified2"... I changed
    # to perennial_ISAsubregion_abundance
    )

set.seed(444)
new_data_aw <- full_results_aw0[, c("Class", aw_vars)]

# MAKE PREDICTION 
df_AW <- full_results_aw0 %>%
  add_column(AW_Prediction=predict(aw_model, newdata = new_data_aw)) %>%
    bind_cols(
      predict(aw_model,
        newdata=new_data_aw, #Generate predictions on new data
        type="prob") %>%
    as_tibble()%>%
      mutate( 
        AW_Predicted=case_when(
              E>=.5~"E",
              I>=.5~"I",
              P>=.5~"P",
              P>E~"ALI",
              E>P~"LTP",
              P==E & I>P~"NMI", 
              P==E & I<=P~"NMI",
              T~"Other"))
        ) %>%
    rename(PredClass="AW_Predicted") %>%
    add_column(model="AW")

df_AW_subset <- df_AW[, subset_cols]


################################################################################
#  Western Mountains Method (Rafi sent 5/6/2024)
################################################################################
load("WM/all_refined_rf_mods.Rdata")
wm_model <- all_refined_rf_mods[[3]]

wm_vars <- c("DifferencesInVegetation_score", #Must be one of these: 0, 0.5, 1, 1.5, 2, 2.5, 3
             "BankWidthMean", #Must be a positive number (zero not allowed)
             "RifflePoolSeq_score", #Must be one of these: 0, 0.5, 1, 1.5, 2, 2.5, 3
             "PctShading", #Must be a number between 0 and 100 (inclusive)
             "UplandRootedPlants_score", #Must be one of these: 0, 0.5, 1, 1.5, 2, 2.5, 3
             "Slope", #Must be a number greater than or equal to zero [check with PDT?]
             "SubstrateSorting_score", #Must be one of these: 0, 0.75, 1.5, 2.25, 3
             "EPT_abundance_0_5_10_20", #Must be one of these: 0, 1, 2, 3, 4. Not provided by user.
             "hydrophytes_present_0_1_2_3_4_5", #Must be one of these: 0, 1, 2, 3, 4, 5. Not provided by user.
             "perennial_ISAsubregion_abundance_simp2_0_5_10_20") #Must be one of these: 0, 1, 2, 3, 4. Not provided by user.

full_results_wm0 <- full_results_addl %>% mutate(
    hydrophytes_present_0_1_2_3_4_5 = case_when(
        hydrophytes_present<5~hydrophytes_present,
        T~5),
    perennial_ISAsubregion_abundance_simp2_0_5_10_20 = case_when(
        perennial_ISAsubregion_abundance==0~0,
        perennial_ISAsubregion_abundance<5~1,
        perennial_ISAsubregion_abundance<10~2,
        perennial_ISAsubregion_abundance<20~3,
        # perennial_West_abundance_simplified2==0~0,
        # perennial_West_abundance_simplified2<5~1,
        # perennial_West_abundance_simplified2<10~2,
        # perennial_West_abundance_simplified2<20~3,
        T~4),
    #TODO: Rafi used "perennial_West_abundance_simplified2"... I changed
    # to perennial_ISAsubregion_abundance
    EPT_abundance_0_5_10_20 = case_when(
        EPT_abundance==0~0,
        EPT_abundance<5~1,
        EPT_abundance<10~2,
        EPT_abundance<20~3,
        T~4)
    ) 

set.seed(333)
new_data_wm <- full_results_wm0[, c("Class", wm_vars)]

# MAKE PREDICTION 
df_WM <- full_results_wm0 %>%
  add_column(WM_Prediction=predict(wm_model, newdata = new_data_wm)) %>%
  bind_cols(
    predict(wm_model,
            newdata=new_data_wm, #Generate predictions on new data
            type="prob") %>%
      as_tibble()%>%
      mutate( 
        WM_Predicted=case_when(
          E>=.5~"E",
          I>=.5~"I",
          P>=.5~"P",
          P>E~"ALI",
          E>P~"LTP",
          P==E & I>P~"NMI", 
          P==E & I<=P~"NMI",
          T~"Other"))
  ) %>%
  rename(PredClass="WM_Predicted") %>%
  add_column(model="WM")

df_WM_subset <- df_WM[, subset_cols]


# ################################################################################
# #  NC Method
# TODO: Need StreamOrder data to do this
# ################################################################################
# df_main_so <- read_csv("C:/Users/smgross/Research Triangle Institute/WARTS GP SDAM - General/NESE_final/scripts-final-NESE/input/raw/Copy of nese_drainagearea_streamorder_10252022.csv")
# full_results_addl_so <- merge(x = full_results_addl, 
#                       y = df_main_so, 
#                       by = "ParentGlobalID", 
#                       all.x = TRUE)
# ## Create additional fields needed for NC method
# full_results_nc0 <- full_results_addl %>% mutate(
#   alglive_cover_score = case_when(AlgalCover_Live=="notdetected"~0,
#                                   AlgalCover_Live=="<2%"~1,
#                                   AlgalCover_Live=="2-10%"~2,
#                                   AlgalCover_Live=="10-40%"~3,
#                                   AlgalCover_Live==">40%"~4,
#                                   T~ 0), #Treat missing data as non-detect
#   Algae_score = case_when(alglive_cover_score>=3~1.5, #>10%
#                           alglive_cover_score==2~1, #>2%
#                           alglive_cover_score==1~0.5, #<2%
#                           alglive_cover_score==0~0), #>2%
#   Mollusk_score = case_when(Mollusk_abundance > 10~3,
#                             Mollusk_taxa > 2~3,
#                             Mollusk_abundance > 2~2,
#                             Mollusk_abundance > 0 ~ 1,
#                             Mollusk_abundance ==0~0),
#   Crayfish_score = case_when(Crayfish_abundance > 2~1.5,
#                              Crayfish_abundance==2~1,
#                              Crayfish_abundance==1~0.5,
#                              Crayfish_abundance==0~0),
#   Amphibian_score = case_when(Amphib_abundance > 3~1.5,
#                               Amphib_richness > 2~1.5,
# 
#                               Amphib_abundance> 1~1,
#                               Amphib_richness == 2~1, #Amphibian abundance >1 but <6 or 4? and only 1 amphibian taxon present
# 
#                               Amphib_abundance==1~0.5,
#                               Amphib_abundance==0~0),
# 
#   #NOTE: In beta code these are "OBL_inchannel" and "hydrophytes_inchannel"...
#   WetlandPlantsInStreambed_score = case_when(hydrophytes_obl_present_inchannel>0~1.5,
#                                              hydrophytes_present_inchannel>0~0.75),
#   # WetlandPlantsInStreambed_score = case_when(OBL_inchannel>0~1.5,
#   #                                            hydrophytes_inchannel>0~0.75),
#   SO_score=case_when(StreamOrder>=2~3,T~0)
# )
# 
# 
# full_results_NC_subset <- full_results_NC %>%
#   select(c("ParentGlobalID","NC_Prediction"))
# 
# # Merge AW results with full df
# full_results <- merge(x = full_results, 
#                       y = full_results_NC_subset, 
#                       by = "ParentGlobalID", 
#                       all.x = TRUE)

# full_results_combined <- rbind(df_GPfinal_subset,
#                                df_GPbeta_subset)
# 
# full_results_combined <- rbind(full_results_combined,
#                                df_NM_subset)

full_results_combined <- rbind(df_GPfinal_subset,
                               df_GPbeta_subset,
                               df_NM_subset, 
                               df_AW_subset, 
                               df_WM_subset)

full_results_combined <- full_results_combined %>% mutate(
    EvALI_correct = case_when(
      Class %in% c("E") & PredClass %in% c("E")~T,
      Class %in% c("I","P") & PredClass %in% c("I","P","ALI")~T,
      T~F),
    PvIvE_correct = case_when(
      Class %in% c("E") & PredClass %in% c("E")~T,
      Class %in% c("I") & PredClass %in% c("I")~T,
      Class %in% c("P") & PredClass %in% c("P")~T,
      T~F)
  )

write_csv(full_results_combined, 
          file=paste0(output_path, "/GreatPlainsFinalResults_withOther.csv"))


################################################################################
#  Comparison Plots
################################################################################
PvIvE_precision_df <- full_results_combined %>%
  filter(TotalVisits>2) %>%
  
  group_by(model, 
           SiteCode, 
           PredClass, 
           TotalVisits, 
           Dataset, 
           Region_detail ) %>%
  tally(name="DifferentClassifications") %>%
  group_by(model, 
           SiteCode, 
           Dataset, 
           Region_detail) %>%
  slice_max(DifferentClassifications, n=1, with_ties=F) %>%
  mutate(PvIvE_precision = (DifferentClassifications-1)/(TotalVisits-1))


EvALI_precision_df <- full_results_combined %>%
  filter(TotalVisits>2) %>%
  
  mutate(Classification_EvALI = case_when(
    PredClass=="E"~"E",
    PredClass %in% c("P","I","ALI")~"ALI",
    T~PredClass)) %>%
  group_by(model, 
           SiteCode, 
           Classification_EvALI, 
           TotalVisits, 
           Dataset, 
           Region_detail) %>%
  tally(name="DifferentClassifications") %>%
  group_by(model, 
           SiteCode, 
           Dataset, 
           Region_detail) %>%
  slice_max(DifferentClassifications, n=1, with_ties=F) %>%
  mutate(EvALI_precision = (DifferentClassifications-1)/(TotalVisits-1))


#######################################################################
EVALIprecision_results_grouped <- EvALI_precision_df %>% 
  group_by(model, 
           Dataset, 
           Region_detail) %>%
  summarise(Precision_EvALI=mean(EvALI_precision))

PvIvEprecision_results_grouped <- PvIvE_precision_df %>% 
  group_by(model, Dataset, Region_detail) %>%
  summarise(Precision_PvIvE=mean(PvIvE_precision))

all_results_flat_grouped2 <- full_results_combined %>% 
  group_by(model, Dataset, Region_detail) %>%
  summarise(n_tests=length(SiteCode),
            n_tests_wet=sum(Wet),
            n_tests_dry=sum(!Wet),
            n_correct_PvIvE=sum(PvIvE_correct),
            n_correct_EvALI=sum(EvALI_correct)#,
            # n_correct_PvIwet=sum(Class[P_or_I_wet]==PredClass[P_or_I_wet]),
            # n_correct_IvEdry=sum(Class[I_or_E_dry]==PredClass[I_or_E_dry]),
            # n_wet_P_or_I=sum(P_or_I_wet),
            # n_dry_I_or_E=sum(I_or_E_dry)
  ) %>%
  ungroup() %>%
  mutate(pct_correct_PvIvE = n_correct_PvIvE/n_tests,
         pct_correct_EvALI = n_correct_EvALI/n_tests#,
        # pct_correct_PvIwet = n_correct_PvIwet/n_wet_P_or_I,
        # pct_correct_IvEdry = n_correct_IvEdry/n_dry_I_or_E
  ) %>% 
  as.data.frame()

precision_results_grouped <- merge(EVALIprecision_results_grouped, 
                                   PvIvEprecision_results_grouped, 
                                   by=c("model", 
                                        "Dataset", 
                                        "Region_detail"))

#This df could be used to graph all at the same time...still working on this
all_results_flat_grouped_precision <- merge(all_results_flat_grouped2, 
                                            precision_results_grouped,
                                            by=c("model", 
                                                 "Dataset", 
                                                 "Region_detail"))

#############################################################################
df_dots1 <- all_results_flat_grouped_precision %>%
  # select(-n_correct_PvIvE, -n_correct_EvALI) %>%
  mutate(aaaaaaaaaaaaPrecisionPvIvE=Precision_PvIvE,
         aaaaaaaaaaaaPrecisionEvALI=Precision_EvALI) %>%
  pivot_longer(cols=#starts_with("pct"), 
                 c("pct_correct_PvIvE",
                   "pct_correct_EvALI",
                   # "pct_correct_PvIwet",
                   # "pct_correct_IvEdry",
                  "aaaaaaaaaaaaPrecisionPvIvE",
                  "aaaaaaaaaaaaPrecisionEvALI"
                   ),
               values_to="Accuracy", 
               names_to="Measure") %>% 
  mutate( Measure=str_sub(Measure, start=13))# %>%
  #filter(ModNameDisplay %in% c("NE (NoGIS)", "SE (NoGIS)", "East (NoGIS)",
                # #              "NE (2GIS)", "SE (2GIS)", "East (2GIS)", 
                           #    "NE Beta", "SE Beta", "NC",))
#
# df_dots1$Measure <- factor(df_dots1$Measure,
#                            levels=c("PvIvE","EvALI","PvIwet","IvEdry",
#                                     "PrecisionPvIvE","PrecisionEvALI"))
df_dots1$Measure <- factor(df_dots1$Measure,
  levels=c("PvIvE","EvALI",#"PvIwet","IvEdry",
         "PrecisionPvIvE","PrecisionEvALI"
         ))
df_dots1$model <- factor(df_dots1$model ,
                      levels = rev(c("GP final",
                                     "GP beta",
                                     "NM",
                                     "AW",
                                     "WM")))



df_dots1$Dataset <- factor(df_dots1$Dataset, 
                           levels=c("Testing","Training"))
label_names <- c(
  `PvIvE` = "Accuracy\nPvIvE",
  `EvALI` = "Accuracy\nEvALI"#,
  # `PvIwet` = "Accuracy\nPvIwet",
  # `IvEdry` = "Accuracy\nIvEdry",
 # `PrecisionPvIvE` = "Precision\nPvIvE",
 # `PrecisionEvALI` = "Precision\nEvALI"
)
dotplot1 <- ggplot(data=df_dots1%>%filter(Dataset=="Testing"), 
                   aes(x=model, y=Accuracy))+
  geom_jitter(aes(size=Dataset, color=Region_detail), width=0.04, height=0)+
  scale_color_manual(
    values=c(
      "Unstrat"="black",
      "GP_S"="#dbb002",
      "GP_N"="#2ac93a",
      "GP_C"="#0639c4",
      "GP_U"="#db0247") 
  ) +
  scale_size_manual(values=c(1,2.5))+
  facet_wrap(~Measure, nrow = 1, scales="free_x",
             labeller=as_labeller(label_names))+
  coord_flip()+
  xlab("")+
  # labs(title="Summary of All Model Performance")+
  # scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
  theme_bw()
dotplot1

ggsave(dotplot1, height=3.5, width=10 , units="in", dpi=900,
       filename=paste0(output_path, "/dotplot1.png"))




df_dots2 <- df_dots1
df_dots2$Measure <- factor(df_dots2$Measure,
                           levels=c("PvIvE","EvALI",#"PvIwet","IvEdry",
                                    "PrecisionPvIvE","PrecisionEvALI"
                           ))
df_dots2$model <- factor(df_dots2$model ,
                         levels = rev(c("GP final",
                                        "GP beta",
                                        "NM",
                                        "AW",
                                        "WM")))



df_dots2$Dataset <- factor(df_dots2$Dataset, 
                           levels=c("Testing","Training"))
label_names <- c(
  `PvIvE` = "Accuracy\nPvIvE",
  `EvALI` = "Accuracy\nEvALI",
  # `PvIwet` = "Accuracy\nPvIwet",
  # `IvEdry` = "Accuracy\nIvEdry",
  `PrecisionPvIvE` = "Precision\nPvIvE",
  `PrecisionEvALI` = "Precision\nEvALI"
)
dotplot2 <- ggplot(data=df_dots2%>%filter(Dataset=="Testing"), 
                   aes(x=model, y=Accuracy))+
  geom_jitter(aes(size=Dataset, color=Region_detail), 
              width=0.04, height=0)+
  scale_color_manual(
    values=c(
      "Unstrat"="black",
      "GP_S"="#dbb002",
      "GP_N"="#2ac93a",
      "GP_C"="#0639c4",
      "GP_U"="#db0247") 
  ) +
  scale_size_manual(values=c(1,2.5))+
  facet_wrap(~Measure, nrow = 1, scales="free_x",
             labeller=as_labeller(label_names))+
  coord_flip()+
  xlab("")+
  # labs(title="Summary of All Model Performance")+
  scale_y_continuous(limits=c(0,1), 
                     breaks=c(0,.5,1), 
                     name="Performance")+
  theme_bw()
dotplot2

ggsave(dotplot2, height=3.5, width=10 , units="in", dpi=900,
       filename=paste0(output_path, "/dotplot2.png"))

