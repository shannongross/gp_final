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

# model_version <- "DraftFinalModelsQC"
model_version <- "FinalModelQC_Apr2024"
chosen_model <- "NoGIS_Unstrat"
# chosen_version <- "V4"
chosen_version <- "V5"
parent_path <- paste0(HOME_DIR, "/output/models/", model_version,
                      "/", chosen_model,"/", chosen_version, "_", chosen_model)
disturb_path <- paste0(parent_path, "/disturb_analysis")
if (!dir.exists(disturb_path)){dir.create(disturb_path)}

# Get results dataset 
df_results <- read_csv(paste0(parent_path, "/refined_results.csv"))

# Get landuse/disturbances info
df_lu <- read_csv(paste0(HOME_DIR,"/input/raw/df_main_20240125.csv"))
df_lu <- df_lu[, c( "ParentGlobalID",
                    "Disturbances",
                    "Disturbances_details",
                    "Landuse",
                    "Landuse_s",
                    "Lanuse_Notes")]

# Join the dataframes
df = merge(x = df_results, y = df_lu, by = "ParentGlobalID",
           all.x = TRUE)

######################## WAS *SAMPLE* DISTURBED? ######################## 
# A site/sample is marked 'disturbed' if it's landuse is: urban,
# agricultural, or developed.
# If landuse is N/A, site is assumed "undisturbed".
#
#########################################################################
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

print(paste("number flagged agricultural in", chosen_model, chosen_version, 
            "model:", sum(df_disturbed$agricultural)))
print(paste("number flagged urban in", chosen_model, chosen_version, 
            "model:", sum(df_disturbed$urban)))
print(paste("number flagged developed in", chosen_model, chosen_version, 
            "model:", sum(df_disturbed$developed)))

#TODO: Should "developed" be included?
df_disturbed$disturbed <- df_disturbed$agricultural + df_disturbed$urban 

df_disturbed <- df_disturbed %>% mutate(
        SampleWasDisturbed = case_when(disturbed>=1~1, T~0))
df_disturbed$SampleWasDisturbed <- as.numeric(df_disturbed$SampleWasDisturbed)
number_disturbed <- sum(df_disturbed$SampleWasDisturbed)
print(paste("number with at least one flag in", chosen_model, chosen_version, 
            "model:", number_disturbed))

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
  ) %>% mutate(
    SampleWasDisturbed=case_when(SampleWasDisturbed==0~"NotDisturbed",
                                 SampleWasDisturbed==1~"Disturbed"))
# summary_disturb <- t(summary_disturb)
print(summary_disturb)

df_disturbed <- df_disturbed %>% mutate(
  SampleWasDisturbed=case_when(SampleWasDisturbed==0~"NotDisturbed",
                               SampleWasDisturbed==1~"Disturbed"),
  TypeOfDisturbance=case_when(agricultural==1 & urban==0 ~"agricultural",
                              urban==1 & agricultural==0 ~"urban",
                              urban==1 & agricultural==1 ~"ag and urban",
                              urban==0 & agricultural==0 ~"none",
  ))


write.csv(df_disturbed, paste0(disturb_path,"/df_lu_disturbances.csv"))

write.csv(summary_disturb, paste0(disturb_path,"/summary_disturb.csv"))








main_disturbance <- df %>% filter(Notes!="Augmented") %>%
  mutate(Disturbed_LU = case_when(
    is.na(Landuse) ~ "Undisturbed", #Fair assumption?
    str_detect(Landuse, "urban")~"Disturbed",
    str_detect(Landuse, "developed")~"Disturbed",
    str_detect(Landuse, "agricultural")~"Disturbed",
    str_detect(Landuse_s, "urban")~"Disturbed",
    str_detect(Landuse_s, "developed")~"Disturbed",
    str_detect(Landuse_s, "agricultural")~"Disturbed", 
    # SiteCode %in% c("ARNE230_V","MSSE8847_B","CAAW0247","MTAW9191","NVAW1187","NMWM1319")~"Disturbed", #These are all indicated as "other_describe", so these classifications are based on "Lanuse_Notes" and google earth imagery
    # SiteCode %in% c("KSCB27","COAW1449","COAW1442","AZWM1588","COWM1092")~"Undisturbed", #These are all indicated as "other_describe", so these classifications are based on "Lanuse_Notes" and google earth imagery
    T~"Undisturbed"))

main_disturbance %>%
  mutate(
         Agricultural = str_detect(Landuse, "ag"),
         Developed = str_detect(Landuse,"dev")) %>%
  group_by(Region_detail, Disturbed_LU) %>%tally() %>%
  pivot_wider(names_from = Disturbed_LU, values_from = n)

main_disturbance %>%
  mutate(
         Agricultural = str_detect(Landuse, "ag"),
         Developed = str_detect(Landuse,"dev")) %>%
  group_by(#Region_detail, 
           Disturbed_LU, Agricultural, Developed) %>%tally() 












plot_disturbed_samples <- ggplot(data=df_disturbed, 
                                 aes(x=Region_detail,  
                                     fill=TypeOfDisturbance)) +
  geom_bar(stat="count", position=position_dodge()) +
  facet_wrap(~Dataset) +
  labs(title = "Disturbances Analysis",
       # subtitle = "(Before train-test split or oversampling)", 
       y="",
       x=""
  ) 
print(plot_disturbed_samples)
ggsave(plot_disturbed_samples, 
          filename=paste0(disturb_path, "/disturb_by_subregion.png"),
          dpi=300, height=5, width=9)



plot_disturbed_samples <- ggplot(data=df_disturbed, 
                                 aes(x=Region_detail,  
                                     fill=TypeOfDisturbance)) +
  geom_bar(stat="count", position=position_dodge(), width = 0.6) +
  scale_fill_manual(values=c("ag and urban"="#bd2828", 
                             "agricultural"="#918d1d", 
                             "urban"="#2b98ab", 
                             "none"="#757574"
                            ))  +
  labs(title = "Disturbances Analysis",
       # subtitle = "(Before train-test split or oversampling)", 
       y="",
       x=""
  ) 
print(plot_disturbed_samples)
ggsave(plot_disturbed_samples, 
       filename=paste0(disturb_path, "/disturb_by_type.png"),
       dpi=300, height=5, width=9)







plot_disturbed_pvive <- ggplot(data=summary_disturb, 
       aes(x=SampleWasDisturbed, 
           y=pctCorrect_PvIvE_CorrectClass50,
           fill=SampleWasDisturbed)) +
  geom_bar(stat="identity") +
  labs(title = "Disturbances Analysis",
       subtitle = "PvIvE",
       y="% PvIvE Accuracy",
       x=""
  ) + ylim(0,1)
print(plot_disturbed_pvive)   
ggsave(plot_disturbed_pvive, 
       filename=paste0(disturb_path, "/disturb_pvive.png"),
       dpi=300, height=5, width=6)

plot_disturbed_evali <- ggplot(data=summary_disturb, 
                               aes(x=SampleWasDisturbed, 
                                   y=pctCorrect_EvALI_CorrectClass50, 
                                   fill=SampleWasDisturbed )) +
  geom_bar(stat="identity") +
  labs(title = "Disturbances Analysis",
       subtitle = "EvALI",
       y="% EvALI Accuracy",
       x=""
  ) +
  # facet_wrap(~Datset) + 
  ylim(0,1)
print(plot_disturbed_evali)   
ggsave(plot_disturbed_evali, 
       filename=paste0(disturb_path, "/disturb_evali.png"),
       dpi=300, height=5, width=6)


       
plot_disturbed_summary <- ggplot(data=summary_disturb, 
                                 aes(x=Region_detail,  
                                     fill=TypeOfDisturbance)) +
  geom_bar(stat="count", position=position_dodge(), width = 0.6) +
  scale_fill_manual(values=c("ag and urban"="#bd2828", 
                             "agricultural"="#918d1d", 
                             "urban"="#2b98ab", 
                             "none"="#757574"
  ))  +
  labs(title = "Disturbances Analysis",
       # subtitle = "(Before train-test split or oversampling)", 
       y="",
       x=""
  ) 
print(plot_disturbed_summary)
ggsave(plot_disturbed_summary, 
       filename=paste0(disturb_path, "/disturb_summary.png"),
       dpi=300, height=5, width=9)






############### FINAL RESULTS TO PDT
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

df_disturbed <- df %>% filter(Notes!="Augmented") %>% 
  mutate(
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

df_disturbed_aug <- df %>% filter(Notes=="Augmented") %>% 
  mutate(
    agricultural= "NA (augmented data)",
    urban="NA (augmented data)",
    developed= "NA (augmented data)",
    disturbed= "NA (augmented data)",
    SampleWasDisturbed= "NA (augmented data)",
    TypeOfDisturbance= "NA (augmented data)"
  ) 
df_disturbed <- df_disturbed %>% mutate(
  SampleWasDisturbed=case_when(SampleWasDisturbed==0~"NotDisturbed",
                               SampleWasDisturbed==1~"Disturbed"),
  TypeOfDisturbance=case_when(agricultural==1 & urban==0 ~"agricultural",
                              urban==1 & agricultural==0 ~"urban",
                              urban==1 & agricultural==1 ~"ag and urban",
                              urban==0 & agricultural==0 ~"none",
  ))
df_disturb_both <- rbind(df_disturbed, df_disturbed_aug)


write.csv(df_disturb_both, paste0(disturb_path,"/gp_final_results_disturbances.csv"))

