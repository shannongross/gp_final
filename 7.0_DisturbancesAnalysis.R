######################## DISTURBANCES ANALYSIS ############################## 
# The following section script filters the GP model results to look for those
# sites marked as disturbed land uses during sampling. Summary plots/tables are
# generated to look at the difference between disturbed (urban/ag) vs non-
# disturbed sites.
##############################################################################
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

model_version <- "DraftFinalModels2"
chosen_model <- "NoGIS_Unstrat"
chosen_version <- "V4"
parent_path <- paste0(HOME_DIR, "/output/models/", model_version,
                      "/", chosen_model,"/", chosen_version, "_", chosen_model)

# Get results dataset 
df_results <- read_csv(paste0(parent_path, "/refined_results.csv"))

# Get landuse/disturbances info
df_lu <- read_csv(paste0(HOME_DIR,"/input/raw/df_main_20240125.csv"))
df_lu <- df_lu[, c( "ParentGlobalID","Weather","PctCloudCover","rain_yesno",
                    "Disturbances","Disturbances_details","Landuse","Landuse_s",
                    "Lanuse_Notes")]

# Join the dataframes
df = merge(x = df_results, y = df_lu, by = "ParentGlobalID",
           all.x = TRUE)

df_disturbed <- df %>% filter(Notes!="Augmented") %>% mutate(
  agricultural= case_when(
    str_detect(Landuse, "agricultural") ~ 1, 
    str_detect(Landuse_s, "agricultural") ~ 1, 
    T~0),
  urban= case_when(
    str_detect(Landuse, "urban") ~ 1, 
    str_detect(Landuse_s, "urban") ~ 1, 
    T~0),
  developed= case_when(
    str_detect(Landuse, "developed") ~ 1, 
    str_detect(Landuse_s, "developed") ~ 1, 
    T~0)
  )

print(paste("number flagged agricultural in", chosen_model, chosen_version, "model:", sum(df_disturbed$agricultural)))
print(paste("number flagged urban in", chosen_model, chosen_version, "model:", sum(df_disturbed$urban)))
df_disturbed$disturbed <- df_disturbed$agricultural + df_disturbed$urban 

df_disturbed <- df_disturbed %>% mutate(SampleWasDisturbed = case_when(disturbed>=1~1, T~0))
df_disturbed$SampleWasDisturbed <- as.numeric(df_disturbed$SampleWasDisturbed)
number_disturbed <- sum(df_disturbed$SampleWasDisturbed)
print(paste("number with at least one flag in", chosen_model, chosen_version, "model:", number_disturbed))

write.csv(df_disturbed, paste0(parent_path,"/df_lu_disturbances.csv"))

summary_disturb <- df_disturbed %>% group_by(SampleWasDisturbed) %>%
  summarise(n_samples=length(Region),
            n_Training=sum(Dataset=="Training"),
            n_Testing=sum(Dataset=="Testing"),
            n_Flagged_Agricultural=sum(agricultural),
            n_Flagged_Urban=sum(urban),
            n_Flagged_Developed=sum(developed),
            n_Flagged_Disturbed=sum(as.numeric(SampleWasDisturbed)),
            n_correct_PvIvE_CorrectClass50=sum(PvIvE_correct),
            pctCorrect_PvIvE_CorrectClass50=n_correct_PvIvE_CorrectClass50/n_samples,
            n_correct_EvALI_CorrectClass50=sum(EvALI_correct),
            pctCorrect_EvALI_CorrectClass50=n_correct_EvALI_CorrectClass50/n_samples
  ) 
summary_disturb <- t(summary_disturb)
print(summary_disturb)

write.csv(summary_disturb, paste0(parent_path,"/summary_disturb.csv"))