################################################################################
# This script takes the various FINAL GREAT PLAINS input spreadsheets and 
# formats them appropriately
# - combine dataframes
# - drop extra columns
# - NaN handling
# - dtype transformation, where needed
# - oversampling
# - split into train/test
# - EDA plots
#
# Note: last input dataset update from Rafi on 16 Nov 2023
################################################################################
library(rlang)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(skimr)
library(readxl)

CURRENT_REGION <- "GP"
CURRENT_REGION_DISPLAY <- "Final Great Plains"

# If TRUE, saves auxillary spreadsheets and plots
save_spreadsheets <- TRUE
save_plots <- TRUE

# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Create output dirs if they do not exist
input_clean_dir <- paste0(HOME_DIR, "/input/processed")
if (!dir.exists(input_clean_dir)){dir.create(input_clean_dir)}

eda_dir <- paste0(HOME_DIR, "/output/eda")
if (!dir.exists(eda_dir)){dir.create(eda_dir)}

# identifying columns
info_list <- c("ParentGlobalID","CollectionDate","Region",
               "Region_detail","SiteCode","Class","Wet")

################################################################################
# Fetch the input datasets
#   - metrics_lookup: list of all metrics and their subtypes 
#   - df_crosswalk: contains SiteCode, actual Class, and region/subregions
#   - df_gis: for each SiteCode, contains the precip, temp, elev, snow 
#   - df_main: all features collected (need to add site visit number)
#   - ai_metrics: aquatic inverts
################################################################################

################################################################################
# METRICS LOOKUP
# 
################################################################################
metrics_lookup <- read_xlsx("input/raw/metrics_dictionary.xlsx",
                            sheet = "DATA_DICT") %>%
  filter(MetricSubtype!="Direct") %>%
  filter(GP_final!="FALSE") %>%
  filter(MetricCandidate_KF=="TRUE")

# select("MetricType","MetricSubtype","Metric","GP_final") 
current_metrics <- metrics_lookup[!duplicated(metrics_lookup), ]

candidate_list <- current_metrics$Metric
print(paste0("There are ", 
             length(candidate_list), 
             " candidate metrics for the ", 
             CURRENT_REGION, " analysis"))

################################################################################
# CROSSWALK
# 286 sites. 72 ephemeral.
################################################################################
df_crosswalk <- read_csv("input/raw/xwalk_df.csv") %>%
# df_crosswalk <- read_csv("input/raw/master_site_class_xwalk_092123.csv") %>%
  filter(Region %in% c(CURRENT_REGION))  %>%
  rename(SiteCode=sitecode)
print(paste("df_crosswalk contains:", 
            length(df_crosswalk$SiteCode),
            "records. It has", 
            length(unique(df_crosswalk$SiteCode)), 
            "unique sites."))

plot_sites_crosswalk <- ggplot(df_crosswalk, aes(Class)) + 
  geom_bar(fill="cornflowerblue") +
  labs(title = "Crosswalk",
       y="",x="",
       caption=paste0("(",CURRENT_REGION_DISPLAY,
                      ")\n Number of Samples: ", dim(df_crosswalk)[1], 
                      "\n Number of Sites: ", 
                      length(unique(df_crosswalk$SiteCode))[1]))+
  geom_text(stat="count",
            aes(label = scales::percent(after_stat(count)/
                                          sum(after_stat(count)))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=after_stat(count)),
            vjust=1.3, color="white", size=2.8) 
print(plot_sites_crosswalk)
if (save_plots==TRUE){ggsave(plot_sites_crosswalk, 
         filename=paste0(eda_dir, "/plot_sites_crosswalk.png"), 
         dpi=300, height=4, width=6)}


################################################################################
# MAIN
# Remove ai from main (use ai_metrics.csv instead)
################################################################################
df_main <- read_csv("input/raw/df_main_20231116.csv") %>%
  filter(SiteCode %in% (unique(df_crosswalk$SiteCode)))%>% 
  group_by(SiteCode, CollectionDate) %>% 
  slice_head(n=1) %>% #Temporary fix while dupes are resolved
  ungroup()

## Check that there are still the correct number of sites
print(paste("df_crosswalk contains:", length(df_crosswalk$SiteCode), "records. It has", length(unique(df_crosswalk$SiteCode)), "unique sites."))
print(paste("df_main contains:", length(df_main$SiteCode), "records. It has", length(unique(df_main$SiteCode)), "unique sites."))
print(paste("This site is in df_main but not in df_crosswalk:", setdiff(df_main$SiteCode, df_crosswalk$SiteCode)))
print(paste("This site is in df_crosswalk but not in df_main:", setdiff(df_crosswalk$SiteCode, df_main$SiteCode)))

# combine datasets
df_main_new <- left_join(df_main, df_crosswalk, by="SiteCode") 
# Check that everything looks good so far
print(df_main_new %>% count(Class))
print(nrow(df_main_new))
skim(df_main_new)


################################################################################
# Get some general information about the current region
################################################################################
# Distribution of sub-regions
print(df_main_new %>% count(Region_detail))
print(df_main_new %>% count(Class))
print(nrow(df_main_new))

print(df_main_new %>% count(Sinuosity_score))
# df_main_new %>% select(ParentGlobalID, Sinuosity_score) %>% 
#   filter(Sinuosity_score %in% c(0.5, 2.5))

# How many sites are there in the current region?
print(paste0("The ", CURRENT_REGION,
            " region has ",
            length(unique(df_main_new$SiteCode)),
            " unique sites, and ",
            length(df_main_new$SiteCode),
            " total samples."))


################################ FIX MISTAKES IN DB ############################
# The original file df_main contains some errors (half and quarter scores where
# there should be none). The file db_fixes.xlsx contains corrected values that 
# will be used  to overwrite then erroneous values
################################################################################
df_fixes <- read_excel("input/raw/db_fixes.xlsx", sheet="Query Aug19")
df_fixes <- df_fixes[,c("ParentGlobalID","SiteCode","Sinuosity_score",
                        "ChannelDimensions_score","RifflePoolSeq_score",
                        "SubstrateSorting_score")]
df_fixes <- df_fixes %>%
  rename( Sinuosity_score.new = Sinuosity_score,
          ChannelDimensions_score.new = ChannelDimensions_score,
          RifflePoolSeq_score.new = RifflePoolSeq_score,
          SubstrateSorting_score.new = SubstrateSorting_score
  )

df_main_new <- df_main_new %>%
  rename( Sinuosity_score.old = Sinuosity_score,
          ChannelDimensions_score.old = ChannelDimensions_score,
          RifflePoolSeq_score.old = RifflePoolSeq_score,
          SubstrateSorting_score.old = SubstrateSorting_score
  )
df_main_new <- left_join(df_main_new, df_fixes)

df_main_new$Sinuosity_score <- ifelse(is.na(
                            df_main_new$Sinuosity_score.old), 
                            df_main_new$Sinuosity_score.new, 
                            df_main_new$Sinuosity_score.old)

df_main_new$ChannelDimensions_score <- ifelse(is.na(
                            df_main_new$ChannelDimensions_score.old), 
                            df_main_new$ChannelDimensions_score.new, 
                            df_main_new$ChannelDimensions_score.old)

df_main_new$RifflePoolSeq_score <- ifelse(is.na(
                            df_main_new$RifflePoolSeq_score.old), 
                            df_main_new$RifflePoolSeq_score.new, 
                            df_main_new$RifflePoolSeq_score.old)

df_main_new$SubstrateSorting_score <- ifelse(is.na(
                            df_main_new$SubstrateSorting_score.old), 
                            df_main_new$SubstrateSorting_score.new, 
                            df_main_new$SubstrateSorting_score.old)

### Drop these three samples per Ken's comment 10/22/23
df_main_new <- df_main_new %>% 
  filter(ParentGlobalID !="36a91acb-04ba-486c-9a19-a948b61e1dcb",
         ParentGlobalID !="efaada26-7ec0-476b-839c-f3624ff3003c",
         ParentGlobalID !="7fff7228-ec8e-4dc6-b34b-840389e6809f")

### Fill in missing Slope per Ken's comment 10/22/23
df_main_new <- df_main_new %>% 
  mutate(Slope=replace(Slope, 
         ParentGlobalID=="4dd8eab4-d92a-478f-9f13-f676d5eb6b94", 1))
# Check that the value has been corrected
df_main_new %>% 
  filter(ParentGlobalID =="4dd8eab4-d92a-478f-9f13-f676d5eb6b94") %>% 
  select(Slope)

### Fill in missing Slope per Ken's comment 10/22/23
df_main_new <- df_main_new %>% 
  mutate(Slope=replace(Slope, 
         ParentGlobalID=="2ed29fbd-ceec-4864-b7b3-1a303594a7ff", 1))
# Check that the value has been corrected
df_main_new %>% 
  filter(ParentGlobalID =="2ed29fbd-ceec-4864-b7b3-1a303594a7ff") %>% 
  select(Slope)

### Fill in missing BankWidthMean per Ken's comment 10/22/23
df_main_new <- df_main_new %>% 
  mutate(BankWidthMean=replace(BankWidthMean, 
         ParentGlobalID=="9d7a69ef-b77e-4904-a7ed-1363f6d4c274", 5.411111))
# Check that the value has been corrected
df_main_new %>% 
  filter(ParentGlobalID =="9d7a69ef-b77e-4904-a7ed-1363f6d4c274") %>% 
  select(BankWidthMean)

### Fill in missing SubstrateSorting_score per Ken's comment 10/22/23
df_main_new <- df_main_new %>% 
  mutate(SubstrateSorting_score=replace(SubstrateSorting_score, 
         ParentGlobalID=="aa8e2eb0-3597-4a92-bc33-6c4554323254", 0))
# Check that the value has been corrected
df_main_new %>% 
  filter(ParentGlobalID =="aa8e2eb0-3597-4a92-bc33-6c4554323254") %>% 
  select(SubstrateSorting_score)

### Fill in missing SubstrateSorting_score per Ken's comment 10/22/23
df_main_new <- df_main_new %>% 
  mutate(SubstrateSorting_score=replace(SubstrateSorting_score, 
         ParentGlobalID=="2375d4ae-15c7-491c-a6ae-b9bb2d4f8907", 0))
# Check that the value has been corrected
df_main_new %>% 
  filter(ParentGlobalID =="2375d4ae-15c7-491c-a6ae-b9bb2d4f8907") %>% 
  select(SubstrateSorting_score)

## Replace ChannelDimensions_score
df_main_new <- df_main_new %>% 
  mutate(ChannelDimensions_score=replace(ChannelDimensions_score, 
             ParentGlobalID=="3d7df5e7-fc17-4f9a-ae4f-ff6de2625f4c", 1.5),
         ChannelDimensions_score=replace(ChannelDimensions_score, 
             ParentGlobalID=="7c5784db-21d3-4348-86a5-9faef5ca0a9b", 1.5),
         ChannelDimensions_score=replace(ChannelDimensions_score, 
             ParentGlobalID=="3f5b8dba-7b29-48b4-b00d-83f6a4c3abad", 1.5))
# Check that the value has been corrected
df_main_new %>% 
  filter(ParentGlobalID =="7c5784db-21d3-4348-86a5-9faef5ca0a9b") %>% 
  select(ChannelDimensions_score)
# Check that value has been corrected
df_main_new %>% select(ParentGlobalID, ChannelDimensions_score) %>%
        filter(ChannelDimensions_score==2.25)

## Replace outlier BankWidthMean 
df_main_new <- df_main_new %>% 
  mutate(BankWidthMean=replace(BankWidthMean, 
         ParentGlobalID=="8655f7b1-4034-4341-8604-329bf03d1828", 17.75))

# Check that value has been corrected
df_main_new %>%
  filter(ParentGlobalID=="8655f7b1-4034-4341-8604-329bf03d1828") %>%  
  select(ParentGlobalID, BankWidthMean)

## Replace incorrect Sinuosity
df_main_new <- df_main_new %>% 
  mutate(Sinuosity_score=replace(Sinuosity_score, 
              ParentGlobalID=="3d7df5e7-fc17-4f9a-ae4f-ff6de2625f4c", 2),
         Sinuosity_score=replace(Sinuosity_score, 
              ParentGlobalID=="3f5b8dba-7b29-48b4-b00d-83f6a4c3abad", 1))
# Check that value has been corrected
df_main_new %>%
  filter((ParentGlobalID=="3d7df5e7-fc17-4f9a-ae4f-ff6de2625f4c") | 
         (ParentGlobalID=="3f5b8dba-7b29-48b4-b00d-83f6a4c3abad")) %>%  
  select(ParentGlobalID, Sinuosity_score)

## ChannelDimensions_score_NM should be the same as ChannelDimensions_score
df_main_new <- df_main_new %>% select(-c("ChannelDimensions_score_NM"))
df_main_new <- df_main_new %>% 
    mutate(ChannelDimensions_score_NM=ChannelDimensions_score)

## Replace missing WaterInChannel_score (wet)
df_main_new <- df_main_new %>% 
  mutate(WaterInChannel_score=replace(WaterInChannel_score, 
         ParentGlobalID=="c4a6c40d-c765-4742-8e65-6e767dd0f831", 5)) %>% 
  mutate(Wet=replace(Wet, 
          ParentGlobalID=="c4a6c40d-c765-4742-8e65-6e767dd0f831", TRUE))
# Check that value has been corrected
df_main_new %>%
  filter(ParentGlobalID=="c4a6c40d-c765-4742-8e65-6e767dd0f831") %>%  
  select(ParentGlobalID, WaterInChannel_score, Wet)
############################## CREATE MISSING METRICS ##########################
# The original file df_main is missing some metrics. Create them here.
#
################################################################################
# df_main_new <- df_main_new %>% 
#     mutate(
      # BMI_score = case_when(
      #     TotalAbundance >= 10 & Richness>=3 & TolRelAbund<0.9  ~3,
      #     Richness >= 5 & TolRelAbund<0.9~3,  
      #     TotalAbundance>=4 & TolRelAbund<0.9 ~2,   
      #     TotalAbundance>0~1, 
      #     T~0), 
      # #BMI_score_alt1: Same as original but with alt tolerance metric
      # BMI_score_alt1 = case_when(
      #     TotalAbundance >= 10 & Richness>=3 & TolRelAbundAlt<0.9  ~3,  
      #     Richness >= 5 & TolRelAbundAlt<0.9~3,  
      #     TotalAbundance>=4 & TolRelAbundAlt<0.9 ~2,   
      #     TotalAbundance>0~1, 
      #     T~0), 
      # #BMI_score_alt2: Brian Topping's alternative approach
      # BMI_score_alt2 = case_when(
      #     #Strong
      #     TotalAbundance >= 10 & NonTolTaxa>=3  ~3,  
      #     NonTolTaxa >= 5 ~3,  
      #     #Medium
      #     TotalAbundance>=4 & NonTolTaxa >=2  ~2,   
      #     #Weak
      #     NonTolTaxa > 0~1,
      #     NonTolTaxa ==0 & TolTaxa >0 ~1, 
      #     #Absent
      #     T~0), 
      # #BMI_score_alt3: Brian Topping's alternative approach, alt tolerance
      # BMI_score_alt3 = case_when(
      #     #Strong
      #     TotalAbundance >= 10 & NonTolTaxaAlt>=3  ~3,  
      #     NonTolTaxa >= 5 ~3,  
      #     #Medium
      #     TotalAbundance>=4 & NonTolTaxaAlt >=2  ~2,   
      #     #Weak
      #     NonTolTaxaAlt > 0~1,
      #     NonTolTaxaAlt ==0 & TolTaxaAlt >0 ~1, 
      #     #Absent
      #     T~0), 
      #BMI_score_alt4: Original, but ignores tolerance entirely
      # BMI_score_alt4 =  case_when(
      #     TotalAbundance >= 10 & Richness>=3  ~3,
      #     Richness >= 5~3,  
      #     TotalAbundance>=4 ~2,   
      #     TotalAbundance>0~1, 
      #     T~0)
      # )


# Create Strata column
df_main_new <- df_main_new %>% mutate( Strata = Region_detail)

################################################################################
# Additional Data Cleaning
#
################################################################################

# Double check that these site codes are not present (site codes got updated)
# They were moved from the GP to the WM/AW but sampled as part of the GP effor
qc_badsites <- df_main_new %>% filter(SiteCode %in% c("NMCB33",
                                                   "NMCB34",
                                                   "NMCB35",
                                                   "MTNB29",
                                                   "NMSB118"))
print(paste("There are no sites present that shouldn't be there:",
            nrow(qc_badsites)==0))

# Keep only candidate metric columns
print(paste("Number of candidate indicators:", length(candidate_list)))
df_input0 <- df_main_new %>% select(all_of(c(info_list, candidate_list)))

# Put "Unknown" classes in a separate file
df_unknowns <- df_input0 %>% filter(Class=="U")
if (save_spreadsheets==TRUE){write.csv(df_unknowns, 
                              paste0(input_clean_dir,"/df_unknowns.csv"))}

# Remove "Unknowns" from further consideration
df_input <- df_input0 %>% filter(Class!="U") 
print(nrow(df_input))

# Specify data types explicitly
df_input$Class <- as.factor(df_input$Class)
df_input$Region <- as.factor(df_input$Region)
df_input$Region_detail <- as.factor(df_input$Region_detail)
#double check if/why i need to coerce that
df_input$HydricSoils_score <- as.numeric(df_input$HydricSoils_score)

# Cleanup date column # NOTE: had to manually change dtype in the input csv of 
# this column in order for R to understand it correctly
df_input$CollectionDate <- as.Date(df_input$CollectionDate, format="%m/%d/%Y")
print(class(df_input$CollectionDate))
df_input$month <- strftime(df_input$CollectionDate, "%b")
df_input$month <- factor(df_input$month,
                         levels=c("Jan", "Feb", "Mar",
                                  "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep",
                                  "Oct", "Nov", "Dec"))

## Are there any NaNs?
skim(df_input)

## STORE NAN ROWS FOR INVESTIGATION 
df_NaNs <- df_input[rowSums(is.na(df_input)) > 0,]
if (save_spreadsheets==TRUE){write.csv(df_NaNs, 
                                       paste0(input_clean_dir,"/df_NaNs.csv"))}


## STORE ANY SUSPICIOUS VALUES
df_suspicous <- df_input %>% 
    filter(
          (Sinuosity_score %in% c(0.5, 1.5, 2.5, 3.5)) |
          (ChannelDimensions_score_NM %in% c(0.75, 1.53, 1.75, 2.25)) |
          (ChannelDimensions_score_NM >10000) |
          (BankWidthMean > 1000) |
          (RifflePoolSeq_score %in% c(0.25, 0.75)) 
          # (SubstrateSorting_score %in% c(0.25, 0.5, 1.25, 1.75, 2.25, 2.75, 3.5))
        ) %>%
    select(ParentGlobalID, 
          Sinuosity_score,
          ChannelDimensions_score_NM, 
          BankWidthMean,
          RifflePoolSeq_score
          # SubstrateSorting_score
          )
df_suspicous
## STORE SUSPICOUS ROWS FOR INVESTIGATION 
if (save_spreadsheets==TRUE){write.csv(df_suspicous, 
                                 paste0(input_clean_dir,"/df_suspicous.csv"))}



## DROP ANY ROW W A NaN 
# print(paste("Number of rows:", nrow(df_input)))
# df_input <- df_input %>% na.omit()
# df_input <- df_input %>% drop_na()
# print(paste("Number of rows after dropping nans:",nrow(df_input)))
# 


## Make a column w total number of times a site was visited
set.seed(1)
df_input <- df_input %>% group_by(SiteCode) %>%
  mutate(TotalVisits = n()) %>%
  ungroup()

## add revisted flag
df_input <- df_input %>%
  mutate(revist = case_when(TotalVisits>1~TRUE, T~FALSE))

##Add state column
df_input$State <- substr(df_input$SiteCode,1,2)


################################ TRAIN/TEST SPLIT ##############################
# 
################################################################################
# Get a list of all unique siteCodes 
site_info <- unique(df_input %>% select(Class, SiteCode, Region, Region_detail))
set.seed(123)

## Split into training vs testing based on site codes,
# but distribute them evenly by class and ?subregion? 
testing_sites <- site_info %>% 
  filter(Region=="GP") %>% 
  # group_by(Class) %>%
  group_by(Class, Region_detail) %>% #SMG: confirm this balance compared to previous
  slice_sample(prop = 0.2) #SMG: double check why I used this

testing_sites_list <- unique(testing_sites$SiteCode)
training_sites_list <- setdiff(site_info$SiteCode, testing_sites_list) 
print(paste("Out of the original",
            length(unique(site_info$SiteCode)),
            CURRENT_REGION,
            "sites,", length(testing_sites_list), 
            "sites have been put in the testing set and",
            length(training_sites_list),
            "sites are in the training set."))

# Create train, test dataframes
df_test <- df_input %>% filter(SiteCode %in% testing_sites_list)
df_train <- df_input %>% filter(SiteCode %in% training_sites_list)

## Check that number samples in training + testing adds up to original
print(paste("Training sites:", dim(df_train)[1]))
print(df_train %>% count(Class))
print(df_train %>% count(Region))
print(paste("Testing sites:", dim(df_test)[1]))
print(df_test %>% count(Class))
print(df_test %>% count(Region))
dim(df_train)[1] + dim(df_test)[1] == dim(df_input)[1]

## Are there any sites in training that are also in testing? Shouldn't be.
any(df_train$SiteCode %in% df_test$SiteCode)
any(df_test$SiteCode %in% df_train$SiteCode)

#Add qualifier 
df_test$Dataset <- "Testing"
df_train$Dataset <- "Training"
df_train$Notes <- "Train"
df_test$Notes <- "Test"

df_model <- rbind(df_train, df_test)
df_model$Notes <- "Original"


################################# DATA AUGMENTATION ############################
# Set aside some test sites to be used to check model quality. All samples from
# the test sites are withheld from model development. Want test sites that
# are balanced by strata, class, etc.
################################################################################
## Prep training data
set.seed(321)

# If site was only visited 1x, repeat row 6x
df_1visit <- df_train %>%
    filter(TotalVisits==1) %>%
    slice(rep(1:n(), each = 6)) %>%
    group_by(ParentGlobalID)%>%
    mutate(Augment = 1:n(),
           Notes = case_when(Augment==1 ~"Original", T~"Augmented")) %>%
    select(-Augment) %>%
    ungroup()
print(paste("There were", nrow(df_train %>%filter(TotalVisits==1)),
            "samples from training sites visited once. After upsampling, there are",
            nrow(df_1visit), "samples for sites visited once."))

# If site was only visited 2x, repeat row 3x
df_2visit <- df_train %>%
    filter(TotalVisits==2) %>%
    slice(rep(1:n(), each = 3)) %>%
    group_by(ParentGlobalID) %>%
    mutate(Augment = 1:n(),
           Notes = case_when(Augment==1 ~"Original", T~"Augmented")) %>%
    select(-Augment) %>%
    ungroup()
print(paste("There were", nrow(df_train %>%filter(TotalVisits==2)),
        "samples from training sites visited twice After upsampling, there are",
        nrow(df_2visit), "samples for sites visited twice"))

#  If site was only visited 3x, repeat row twice
df_3visit <- df_train %>%
    filter(TotalVisits==3) %>%
    slice(rep(1:n(), each = 2)) %>%
    group_by(ParentGlobalID)%>%
    mutate(Augment = 1:n(),
           Notes = case_when(Augment==1 ~"Original", T~"Augmented")) %>%
    select(-Augment) %>%
    ungroup()

# Leave sites visited 4x, 5x, 6x as-is
df_456visit <- df_train %>%
    filter(TotalVisits==4 | TotalVisits==5 | TotalVisits==6) %>%
    mutate(Notes = "Original")

# Create augmented dataframe
df_aug_train <- rbind(df_1visit, 
                      df_2visit, 
                      df_3visit,
                      df_456visit
) %>% ungroup()
df_model_aug <- rbind(df_aug_train, df_test)
if (save_spreadsheets==TRUE){
                       write.csv(df_model, 
                       paste0(input_clean_dir,"/df_model.csv"))
                       
                       write.csv(df_model_aug, 
                       paste0(input_clean_dir,"/df_model_aug.csv"))
                       }

print(paste("Number of rows df_model:", nrow(df_model)))
print(paste("Number of rows df_model_aug:", nrow(df_model_aug)))
##################################  PLOTS  #####################################
# Study distributions
#
################################################################################

################### Plot: Check distribution across Dates ###################### 
plot_all_samples_by_month <- ggplot(df_model, aes(month)) + 
      geom_bar(fill="cornflowerblue") +
      labs(title = paste0("Distribution of all samples by month"),
           subtitle = "(Before train-test split or oversampling)", y="", x="",
      caption=paste0("(",CURRENT_REGION_DISPLAY,
                     ")\n Number of Samples: ", dim(df_model)[1], 
                     "\n Number of Sites: ", 
                     length(unique(df_model$SiteCode))[1]))+
      geom_text(stat="count",
                aes(label = scales::percent(..count../sum(..count..))),
                vjust=-0.3, color="black", size=2.7) +
      geom_text(stat="count", aes(label=..count..),
                vjust=1.3, color="white", size=2.8) + 
      theme(axis.text.x = element_text(angle = 90))
print(plot_all_samples_by_month)
if (save_plots==TRUE){ggsave(plot_all_samples_by_month, 
      filename=paste0(eda_dir, "/data_prep_samples_by_month.png"), 
      dpi=300, height=4, width=6)}
################################################################################


################ Plot: Check distribution of Samples by Class ################## 
plot_all_samples_by_class<- ggplot(df_model, aes(Class)) + 
      geom_bar(fill="cornflowerblue") +
      labs(title = "Distribution of all Samples by Class",
           subtitle = "(Before train-test split or oversampling)", y="",x="",
           caption=paste0("(",CURRENT_REGION_DISPLAY,
                          ")\n Number of Samples: ", dim(df_model)[1], 
                          "\n Number of Sites: ", 
                          length(unique(df_model$SiteCode))[1]))+
      geom_text(stat="count",
                aes(label = scales::percent(..count../sum(..count..))),
                vjust=-0.3, color="black", size=2.7) +
      geom_text(stat="count", aes(label=..count..),
                vjust=1.3, color="white", size=2.8) 
print(plot_all_samples_by_class)
if (save_plots==TRUE){ggsave(plot_all_samples_by_class, 
      filename=paste0(eda_dir, "/data_prep_samples_by_class.png"), 
      dpi=300, height=5, width=5)}
################################################################################


################ Plot: Check distribution of Sites by Class #################### 
plot_all_sites_by_class <- ggplot(site_info, aes(Class)) + 
      geom_bar(fill="cornflowerblue") +
      labs(title = "Distribution of Sites by Class",
           subtitle = "(Before train-test split or oversampling)", 
           y="",x="",
           caption=paste0("(",CURRENT_REGION_DISPLAY,
                          ")\n Number of Samples: ", dim(df_model)[1], 
                          "\n Number of Sites: ", 
                          length(unique(df_model$SiteCode))[1]))+
      geom_text(stat="count",
                aes(label = scales::percent(..count../sum(..count..))),
                vjust=-0.3, color="black", size=2.7) +
      geom_text(stat="count", aes(label=..count..),
                vjust=1.3, color="white", size=2.8) 
print(plot_all_sites_by_class)
if (save_plots==TRUE){ggsave(plot_all_sites_by_class, 
     filename=paste0(eda_dir, "/data_prep_sites_by_class.png"), 
     dpi=300, height=5, width=5)}
################################################################################


############################### MAP OF SITES ###################################
# # install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
# #        "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
# 
# library("ggplot2")
# theme_set(theme_bw())
# library("sf")
# library("rnaturalearth")
# library("rnaturalearthdata")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
# 
# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = df_crosswalk, aes(x = long, y = lat), size = 2, 
#              # shape = 23, 
#              fill = "black") +
#   coord_sf(xlim = c(min(df_crosswalk$long)-3, max(df_crosswalk$long)+3),
#            ylim = c(min(df_crosswalk$lat)-3, max(df_crosswalk$lat)+3), 
#            expand = FALSE)
################################################################################


###############  Plot: Check distribution of Samples across Region #############
plot_all_samples_by_region_class <- ggplot(df_model,
      aes(x=Class, fill=Region_detail)) + geom_bar(stat='count') +
      labs(title = paste0(
      "Distribution of all Samples by Subregion, Class"),
      subtitle = "(Before train-test split or oversampling)", y="",x="",
      caption=paste0("(",CURRENT_REGION_DISPLAY,
                     ")\n Number of Samples: ", dim(df_model)[1], 
                     "\n Number of Sites: ", 
                     length(unique(df_model$SiteCode))[1])) + 
      scale_fill_manual(values=c("#bf5266", 
                                  "#c78df7", 
                                  "#f77b4a",
                                  "#4fd7db"))
      # geom_text(stat="count", aes(label=after_stat(..count..)),
      #       vjust=-5, color="black", size=2.8)
      print(plot_all_samples_by_region_class)
if (save_plots==TRUE){ggsave(plot_all_samples_by_region_class, 
      filename=paste0(eda_dir, "/plot_all_samples_by_region_class.png"), 
      dpi=300, height=5, width=6)}

plot_all_samples_by_class_region <- ggplot(df_model, 
      aes(x=Region_detail, fill=Class)) + 
      geom_bar(stat='count') +
      labs(title = paste0(
      "Distribution of all Samples by Region, Class"),
      subtitle = "(Before train-test split or oversampling)", 
      y="Number of Samples",x="",
      caption=paste0("(",CURRENT_REGION_DISPLAY,
                     ")\n Number of Samples: ", dim(df_model)[1], 
                     "\n Number of Sites: ", 
                     length(unique(df_model$SiteCode))[1]))
print(plot_all_samples_by_class_region)
if (save_plots==TRUE){ggsave(plot_all_samples_by_class_region, 
      filename=paste0(eda_dir, "/data_prep_samples_by_class_region.png"), 
      dpi=300, height=5, width=5)}

# plot_all_sites_by_class_subregion <- ggplot(site_info,
#   aes(x=Region_detail, fill=Class)) +
#   geom_bar(stat='count') +
#   labs(title = "Distribution of all Sites by Subregion and Class",
#     subtitle = "(Before train-test split or oversampling)", 
#     y="Number of Sites",x="",
#     caption=paste0("(",CURRENT_REGION_DISPLAY,") Number of sites: ", 
#   dim(site_info)[1]))
# print(plot_all_sites_by_class_subregion)
# ggsave(plot_all_sites_by_class_subregion, filename=paste0(eda_dir,
#      "/plot_all_sites_by_class_subregion.png"), dpi=300, height=5, width=5)


# # Plot: Check distribution of Sites across Subregion
# plot_all_sites_by_subregion <- ggplot(site_info, aes(Region_detail)) + 
#   geom_bar(fill="cornflowerblue") +
#   labs(title = "Distribution of SITES by Subregion",
#        subtitle = "(Before train-test split or oversampling)", y="",x="",
#        caption=paste0("(",CURRENT_REGION_DISPLAY,") Number of sites: ", 
#     dim(site_info)[1])) +
#   geom_text(stat="count",
#             aes(label = scales::percent(..count../sum(..count..))),
#             vjust=-0.3, color="black", size=2.7) +
#   geom_text(stat="count", aes(label=..count..),
#             vjust=1.3, color="white", size=2.8) 
# print(plot_all_sites_by_subregion)
# if (save_plots==TRUE){ggsave(plot_all_sites_by_subregion, 
#   filename=paste0(eda_dir, "/data_prep_sites_by_subregion.png"), 
#   dpi=300, height=5, width=5)}



# ##############################################################################
# if (save_plots==TRUE){ggsave(paste0(eda_dir, "/general_distributions.png"), 
#       arrangeGrob(plot_all_samples_by_class, 
#                   plot_all_sites_by_subregion, 
#                   plot_all_sites_by_class_subregion, 
#                   nrow = 1, top= (CURRENT_REGION_DISPLAY)),
#       dpi=300, height=4, width=12)}


########### Plot: Distribution of site visits before oversampling ############## 
plot_no_site_visits_notitle <- ggplot(df_model, aes(TotalVisits)) +
      geom_bar(stat = "count") +
      labs(#title = paste0(CURRENT_REGION_DISPLAY,
                      #  ": Count of Total Site Visits"),
      subtitle = "(Full dataset, before oversampling)", 
      y="Count",x="",
      caption=paste0("(",CURRENT_REGION_DISPLAY, ") Number of samples: ",
                     dim(df_model)[1])) +
      geom_text(stat="count", aes(label=..count..),
            vjust=-0.5, color="black", size=3) +
      # ylim(0,370) + 
      scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
      theme(legend.position = "none")
print(plot_no_site_visits_notitle)

plot_no_site_visits_title <- ggplot(df_model, aes(TotalVisits)) +
      geom_bar(stat = "count") +
      labs(title = paste0("Count of Total Site Visits"),
      subtitle = "(Full dataset, before oversampling)", 
      y="Count",x="",
      caption=paste0("(",CURRENT_REGION_DISPLAY,
                   ")\n Number of Samples: ", dim(df_model)[1], 
                   "\n Number of Sites: ", 
                   length(unique(df_model$SiteCode))[1]))+
      geom_text(stat="count", aes(label=..count..),
                vjust=-0.5, color="black", size=3) +
      # ylim(0,370) + 
      scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
      theme(legend.position = "none")
      print(plot_no_site_visits_title)

if (save_plots==TRUE){ggsave(plot_no_site_visits_title, 
      filename=paste0(eda_dir, "/plot_no_site_visits_title.png"), 
      dpi=300, height=5, width=7)}


plot_no_site_visits_class <- ggplot(df_model, aes(TotalVisits, fill=Class)) +
      geom_bar(stat = "count") +
      labs(title = paste0(CURRENT_REGION_DISPLAY,
                  ": Count of Total Site Visits"),
      subtitle = "(Full dataset, before oversampling)", 
      y="Count", x="Number of site visits",
      caption=paste0("Number of samples: ", dim(df_model)[1])) +
      # geom_text(stat="count", aes(label=..count..),
      #           vjust=1.3, color="white", size=4) + 
      ylim(0,370) + scale_x_continuous(breaks=c(1,2,3,4,5,6))
      print(plot_no_site_visits_class)
if (save_plots==TRUE){ggsave(plot_no_site_visits_class, 
    filename=paste0(eda_dir, "/plot_no_site_visits_class.png"), 
    dpi=300, height=5, width=7)}


this_class <- "E"
plot_original_visits_E <- ggplot(df_model %>% filter(Class==this_class), 
      aes(TotalVisits, fill=Class)) +
      geom_bar(stat = "count") +
      labs(title = paste0(this_class," Class Number of Site Visits"),
      # subtitle = "(Full dataset, before oversampling)", 
      y="", #Count", 
      x="", #"Number of site visits",
      caption=paste0("(",CURRENT_REGION_DISPLAY, ") Number of samples: ",
                        dim(df_model %>% filter(Class==this_class))[1])) +
      geom_text(stat="count", aes(label=..count..),
              vjust=-0.5, color="black", size=3) +
      # ylim(0,370) + 
      scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
      theme(legend.position = "none")
      print(plot_original_visits_E)


this_class <- "I"
this_color <- "green2"
plot_original_visits_I <- ggplot(df_model %>% filter(Class==this_class), 
      aes(TotalVisits)) +
      geom_bar(stat = "count", fill=this_color) +
      labs(title = paste0(this_class,
      " Class Number of Site Visits"),
      # subtitle = "(Full dataset, before oversampling)", 
      y="", #Count", 
      x="", #"Number of site visits",
      caption=paste0("(",CURRENT_REGION_DISPLAY, ") Number of samples: ",
      dim(df_model %>% filter(Class==this_class))[1])) +
      geom_text(stat="count", aes(label=..count..),
      vjust=-0.5, color="black", size=3) +
      # ylim(0,370) + 
      scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
      theme(legend.position = "none")
      print(plot_original_visits_I)

this_class <- "P"
this_color <- "cornflowerblue"
plot_original_visits_P <- ggplot(df_model %>% filter(Class==this_class), 
      aes(TotalVisits)) +
      geom_bar(stat = "count", fill=this_color) +
      labs(title = paste0(this_class,
      " Class Number of Site Visits"),
      # subtitle = "(Full dataset, before oversampling)", 
      y="", #Count", 
      x="", #"Number of site visits",
      caption=paste0("(",CURRENT_REGION_DISPLAY, ") Number of samples: ",
      dim(df_model %>% filter(Class==this_class))[1])) +
      geom_text(stat="count", aes(label=..count..),
      vjust=-0.5, color="black", size=3) +
      # ylim(0,370) + 
      scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
      theme(legend.position = "none")
      print(plot_original_visits_P)

################################################################################
if (save_plots==TRUE){ggsave(paste0(eda_dir, "/original_distrib_visits.png"), 
      arrangeGrob(plot_no_site_visits_notitle,
                  plot_original_visits_E, 
                  plot_original_visits_I, 
                  plot_original_visits_P,
                  nrow = 1, top= (CURRENT_REGION_DISPLAY)),
      dpi=300, height=4, width=16)}

################################################################################
plot_train_dis <- ggplot(df_train, aes(TotalVisits)) +
      geom_bar(stat = "count") +
      labs(#title = paste0(CURRENT_REGION_DISPLAY,
      #        ": Count of Total Site Visits AUG TRAIN"),
      subtitle = "(Training dataset, before oversampling)", 
      y="Count",x="",
      caption=paste0("Original number of training samples: ", 
      dim(df_train)[1])) +
      geom_text(stat="count", aes(label=..count..),
      vjust=1.3, color="white", size=4) +
      # ylim(0,420) + 
      scale_x_continuous(breaks=c(1,2,3,4,5,6))
      print(plot_train_dis)

plot_train_dis_class <- ggplot(df_train, aes(TotalVisits, fill=Class)) +
    geom_bar(stat = "count") +
    labs(#title = paste0(CURRENT_REGION_DISPLAY,
    #  ": Count of Total Site Visits AUG"),
    subtitle = "(Training dataset, before oversampling)", 
    y="",x="",
    caption=paste0("Original number of training samples: ", 
    dim(df_train)[1])) +
    # geom_text(stat="count", aes(label=..count..),
    #           vjust=-.3, color="white", size=4) +
    # ylim(0,420) + 
    scale_x_continuous(breaks=c(1,2,3,4,5,6))
    print(plot_train_dis_class)


plot_aug_dist <- ggplot(df_aug_train, aes(TotalVisits)) +
    geom_bar(stat = "count") +
    labs(#title = paste0(CURRENT_REGION_DISPLAY,
    #  ": Count of Total Site Visits AUG TRAIN"),
    subtitle = "(Training dataset, after oversampling)", 
    y="",x="",
    caption=paste0("Upsampled number of training samples: ", 
    dim(df_aug_train)[1])) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) +
    # ylim(0,420) + 
    scale_x_continuous(breaks=c(1,2,3,4,5,6))
    print(plot_aug_dist)
if (save_plots==TRUE){ggsave(plot_aug_dist, 
    filename=paste0(eda_dir, "/plot_aug_dist.png"), 
    dpi=300, height=5, width=7)}


plot_aug_dist_class <- ggplot(df_aug_train, aes(TotalVisits, fill=Class)) +
    geom_bar(stat = "count") +
    labs(#title = paste0(CURRENT_REGION_DISPLAY,
    #  ": Count of Total Site Visits AUG"),
    subtitle = "(Training dataset, after oversampling)", 
    y="",x="",
    caption=paste0("Upsampled number of training samples: ", 
    dim(df_aug_train)[1])) +
    # geom_text(stat="count", aes(label=..count..),
    #           vjust=-.3, color="white", size=4) +
    # ylim(0,420) + 
    scale_x_continuous(breaks=c(1,2,3,4,5,6))
    print(plot_aug_dist_class)
if (save_plots==TRUE){ggsave(plot_aug_dist_class, 
    filename=paste0(eda_dir, "/plot_aug_dist_class.png"), 
    dpi=300, height=5, width=7)}


if (save_plots==TRUE){ggsave(paste0(eda_dir, "/upsampled_training.png"), 
    arrangeGrob(plot_train_dis,
                plot_aug_dist, 
                plot_aug_dist_class, 
                nrow = 1
                # top= (CURRENT_REGION_DISPLAY)
                ),
    dpi=300, height=4, width=12)}

if (save_plots==TRUE){ggsave(paste0(eda_dir, "/upsampled_training_color.png"), 
     arrangeGrob(plot_train_dis_class,
                 plot_aug_dist_class, 
                 nrow = 1
                 # top= (CURRENT_REGION_DISPLAY)
     ),
     dpi=300, height=4, width=12)}


if (save_plots==TRUE){ggsave(paste0(eda_dir, "/upsampled_training_grey.png"), 
       arrangeGrob(plot_train_dis,
                   plot_aug_dist, 
                   nrow = 1
                   # top= (CURRENT_REGION_DISPLAY)
     ),
     dpi=300, height=4, width=12)}

############################## PLOT UPSAMPLED DATA ############################# 
# Plot: Distribution of site visits
plot_no_site_visits <- ggplot(df_train, aes(TotalVisits)) + 
    geom_bar(fill="cornflowerblue") +
    labs(#title = "Number of Times Site Was Visited",
    subtitle = "(Before oversampling)", 
    y="", x="",
    caption=paste0("Original number of training samples: ", dim(df_train)[1])) +
    geom_text(stat="count",
    aes(label = scales::percent(..count../sum(..count..))),
    vjust=-0.3, color="black", size=3) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) +
    ylim(0,820)
print(plot_no_site_visits)
if (save_plots==TRUE){ggsave(plot_no_site_visits, 
    filename=paste0(eda_dir, "/plot_no_site_visits.png"), 
    dpi=300, height=5, width=5)}

# Plot: Distribution of site visits AUG
plot_no_site_visits_aug <- ggplot(df_aug_train, aes(TotalVisits)) + 
    geom_bar(fill="cornflowerblue") +
    labs(#title = "Number of Times Site Was Visited",
    subtitle = "(After oversampling)",
    y="",x="",
    caption=paste0("Upsampled number of training samples: ", 
    dim(df_aug_train)[1])) +
    geom_text(stat="count",
    aes(label = scales::percent(..count../sum(..count..))),
    vjust=-0.3, color="black", size=3) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) +
    ylim(0,820)
print(plot_no_site_visits_aug)
if (save_plots==TRUE){ggsave(plot_no_site_visits_aug, 
    filename=paste0(eda_dir, "/plot_no_site_visits_aug.png"), 
    dpi=300, height=5, width=5)}

# Plot: Distribution of site visits
plot_no_site_visits_test <- ggplot(df_test, aes(TotalVisits)) + 
    geom_bar(fill="cornflowerblue") +
    labs(#title = "Number of Times Site Was Visited",
    subtitle = "(Test set - withheld from method development)", 
    y="",x="",
    caption=paste0("Number of testing samples: ", dim(df_test)[1])) +
    geom_text(stat="count",
    aes(label = scales::percent(..count../sum(..count..))),
    vjust=-0.3, color="black", size=3) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) +
    ylim(0,820)
print(plot_no_site_visits_test)
if (save_plots==TRUE){ggsave(plot_no_site_visits_test, 
    filename=paste0(eda_dir, "/plot_no_site_visits_test.png"), 
    dpi=300, height=5, width=5)}


################################################################################
if (save_plots==TRUE){ggsave(paste0(eda_dir, "/upsampling.png"), 
    arrangeGrob(plot_no_site_visits, 
                plot_no_site_visits_aug, 
                plot_no_site_visits_test, 
                nrow = 1, top= (CURRENT_REGION_DISPLAY)),
    dpi=300, height=4, width=12)}
################################################################################

plot_train_eip <- ggplot(df_train, aes(Class)) + 
    geom_bar(fill="cornflowerblue") +
    labs(#title = "Number of Times Site Was Visited",
    subtitle = "(Training, before oversampling)", 
    y="",x="",
    caption=paste0("Number of samples: ", dim(df_train)[1])) +
    geom_text(stat="count",
    aes(label = scales::percent(..count../sum(..count..))),
    vjust=-0.3, color="black", size=3) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) + ylim(0,500)
print(plot_train_eip)

plot_train_aug_eip <- ggplot(df_aug_train, aes(Class)) + 
    geom_bar(fill="cornflowerblue") +
    labs(#title = "Number of Times Site Was Visited",
    subtitle = "(Training, after oversampling)", 
    y="",x="",
    caption=paste0("Number of samples: ", dim(df_aug_train)[1])) +
    geom_text(stat="count",
    aes(label = scales::percent(..count../sum(..count..))),
    vjust=-0.3, color="black", size=3) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) + ylim(0,500)
print(plot_train_aug_eip)


plot_test_eip <- ggplot(df_test, aes(Class)) + 
    geom_bar(fill="cornflowerblue") +
    labs(#title = "Number of Times Site Was Visited",
    subtitle = "(Testing, withheld)", 
    y="",x="",
    caption=paste0("Number of samples: ", dim(df_test)[1])) +
    geom_text(stat="count",
    aes(label = scales::percent(..count../sum(..count..))),
    vjust=-0.3, color="black", size=3) +
    geom_text(stat="count", aes(label=..count..),
    vjust=1.3, color="white", size=4) + ylim(0,500)
print(plot_test_eip)


if (save_plots==TRUE){ggsave(paste0(eda_dir, "/before_after_augment_eip.png"), 
     arrangeGrob(plot_train_eip, 
                 plot_train_aug_eip, 
                 plot_test_eip, 
                 nrow = 1, top= "Distribution of Classes"),
     dpi=300, height=4, width=12)}

################################################################################
# same figures, but with region colored
plot_train_dist_reg <- ggplot(df_train, aes(Class, fill=Region_detail)) + 
    geom_bar() +
    labs(subtitle = "Training data (before oversampling)", y="",x="",
    caption=paste0("Number of samples: ", dim(df_train)[1])) +
    ylim(c(0,500))  +
    theme(legend.position="none")
print(plot_train_dist_reg)

plot_test_dist_reg <- ggplot(df_test, aes(Class, fill=Region_detail)) + 
    geom_bar() +
    labs(subtitle = "Testing data (withheld from method development)", y="",x="",
    caption=paste0("Number of samples: ", dim(df_test)[1])) +
    ylim(c(0,500))  +
    theme(legend.position="none")
    #  scale_fill_manual(values=c("springgreen2","dodgerblue1"), name="")
print(plot_test_dist_reg)

plot_train_aug_dist_reg <- ggplot(df_aug_train, aes(Class,fill=Region_detail)) + 
    geom_bar() +
    labs(subtitle = "Training data (after oversampling)", y="",x="",
    caption=paste0("Number of samples: ", dim(df_aug_train)[1])) +
    ylim(c(0,500))  +
    theme(legend.position="none")
    # scale_fill_manual(values=c("springgreen2","dodgerblue1"), name="",
    #                   label=c("NE","SE"))
print(plot_train_aug_dist_reg)

if (save_plots==TRUE){ggsave(paste0(eda_dir, "/before_after_augment_reg.png"), 
      arrangeGrob(plot_train_dist_reg, 
                  plot_train_aug_dist_reg, 
                  plot_test_dist_reg, 
                  nrow = 1, top= "Distribution of Classes"
                  ),
      dpi=300, height=4, width=12)}


########################### EDA PLOTS OF ALL METRICS ########################### 
for (metric in setdiff(c(candidate_list), "Strata")) {
  # metric <- "ChannelDimensions_score_NM"
  print(metric)
  
  
 
  #boxplot
  metric_box <- ggplot(df_model, aes(x = Class, 
                       y = eval(parse(text = metric)), fill = Class)) + 
    geom_boxplot() +
    # geom_jitter(position=position_jitter(0.2)) +
    stat_summary(fun = "median", geom = "point", shape = 8,
                 size = 3, color = "red") +
    labs(#title = "SubstrateSorting_score",
         #subtitle = "(0 = Not Present, 1.5 = Present)",
          y="",
          caption=paste0("(",CURRENT_REGION_DISPLAY,
                        ")\n Number of Samples: ", dim(df_model)[1], 
                        "\n Number of Sites: ", 
                        length(unique(df_model$SiteCode))[1]))

  # Histogram overlaid with kernel density curve
  metric_hist <- df_model %>% ggplot(aes(x=eval(parse(text = metric)))) + 
    geom_histogram(aes(y=..density..),      #
                   # binwidth=.5,
                   # colour="black", fill="white"
                   ) +
    # geom_density(alpha=.2, fill="#FF6666")  +
  labs(#title = paste(metric),
         #subtitle = "(0 = Not Present, 1.5 = Present)",
        x="value", y="",
        caption=paste0("(",CURRENT_REGION_DISPLAY,
                      ")\n Number of Samples: ", dim(df_model)[1], 
                      "\n Number of Sites: ", 
                      length(unique(df_model$SiteCode))[1]))
  
  print(metric_box)
  print(metric_hist)
  
  temp <- df_model[,c(metric, "SiteCode")]
  temp$fillcolor <- is.na(temp[, metric])

  countMissing <- ggplot(temp, aes(x="", y=metric, fill=fillcolor)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    labs(x="% Complete", y=metric,
         caption=paste0("(",CURRENT_REGION_DISPLAY,
                        ")\n Number of Samples: ", dim(temp)[1], 
                        "\n Number of Sites: ", 
                        length(unique(temp$SiteCode))[1]))+
    scale_fill_manual(values=c("springgreen2","red"), name="Contains Missing")
  countMissing
  
  metric_box_region <- ggplot(df_model, aes(x = Region_detail, 
                                            y = eval(parse(text = metric)), fill = Region_detail)) + 
    geom_boxplot() +
    # geom_violin() +
    # geom_jitter(shape=16, position=position_jitter(0.01), size=1)+
    # geom_jitter(position=position_jitter(0.2)) +
    stat_summary(fun = "median", geom = "point", shape = 8,
                 size = 3, color = "red") +
    labs(#title = "SubstrateSorting_score",
      #subtitle = "(0 = Not Present, 1.5 = Present)",
      y="",
      caption=paste0("(",CURRENT_REGION_DISPLAY,
                     ")\n Number of Samples: ", dim(df_model)[1], 
                     "\n Number of Sites: ", 
                     length(unique(df_model$SiteCode))[1]))
  metric_box_region 
  
  
  
 

  if (save_plots==TRUE){
    ggsave(paste0(eda_dir, "/candidates/metric_", metric,".png"), 
                               arrangeGrob(countMissing, metric_hist, 
                                           metric_box, metric_box_region,
                                           nrow = 2, 
           # top=paste0("\n\n\n---------------------------------------------------------------------------------------------------------\n\n",metric)
           #                     ),
           #                     dpi=300, height=9, width=9)}
           top=metric ),
           dpi=300, height=9, width=9)}
}
################################################################################