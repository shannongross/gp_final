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

model_version <- "Final_GP_Model"
chosen_model <- "NoGIS_Unstrat"
chosen_version <- "V5"
parent_path <- paste0(HOME_DIR, "/output/models/", model_version,
                      "/", chosen_model,"/", chosen_version, "_", chosen_model)
final_path <- paste0(parent_path, "/FINAL_RESULTS")

# Get results dataset 
df_results <- read_csv(paste0(final_path, "/GreatPlainsFinalResults.csv"))

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
df_disturbed <- df %>% 
  # filter(Notes!="Augmented") %>% 
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

ag_disturbed <- sum(df_disturbed %>% 
                      filter(Notes!="Augmented") %>% 
                      select(agricultural))
urban_disturbed <- sum(df_disturbed %>% 
                      filter(Notes!="Augmented") %>% 
                      select(urban))
developed_disturbed <- sum(df_disturbed %>% 
                         filter(Notes!="Augmented") %>% 
                         select(developed))
print(paste("number flagged agricultural in", chosen_model, chosen_version, 
            "model:", ag_disturbed))
print(paste("number flagged urban in", chosen_model, chosen_version, 
            "model:", urban_disturbed))
print(paste("number flagged developed in", chosen_model, chosen_version, 
            "model:", developed_disturbed))

#Count total number of disturbed sites
df_disturbed$disturbed <- df_disturbed$agricultural + 
                          df_disturbed$urban +
                          df_disturbed$developed

df_disturbed <- df_disturbed %>% mutate(
        SampleWasDisturbed = case_when(disturbed>=1~1, T~0))
df_disturbed$SampleWasDisturbed <- as.numeric(df_disturbed$SampleWasDisturbed)
number_disturbed <- sum(df_disturbed$SampleWasDisturbed)
print(paste("number with at least one flag in", chosen_model, chosen_version, 
            "model:", number_disturbed))

summary_disturb <- df_disturbed %>% 
  filter(Notes!="Augmented") %>% 
  group_by(SampleWasDisturbed) %>%
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
print(summary_disturb)

df_disturbed <- df_disturbed %>% mutate(
  SampleWasDisturbed=case_when(SampleWasDisturbed==0~"NotDisturbed",
                               SampleWasDisturbed==1~"Disturbed")
  )


write.csv(df_disturbed, paste0(final_path,"/GreatPlainsFinal_Disturbances.csv"))

write.csv(summary_disturb, paste0(final_path,"/Summary_Disturbances.csv"))


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
       filename=paste0(final_path, "/disturb_pvive.png"),
       dpi=300, height=5, width=5)

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
       filename=paste0(final_path, "/disturb_evali.png"),
       dpi=300, height=5, width=5)

