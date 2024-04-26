######################## MODEL COMPARISON PLOTS ############################## 
# The following section script gathers results from all preliminary model
# versions created in the previous step and creates summary plots to compare
# performance. Much of this script is repetitive (ie nearly the same figures,
# with slight alternations/groupings to visualize differently).
##############################################################################
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)
library(readxl)

# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))


############################# COMBINE ALL RESULTS ##############################
# model_version <- "DraftFinalModels2" 
model_version <- "FinalModelQC_Apr2024"

parent_path <- paste0(getwd(), "/output/models/", model_version)
myfiles <- list.files(parent_path, full.names = T, all.files = TRUE,
                      recursive = T, pattern = "results_for_plots.csv")
all_results_flat <- do.call(rbind, lapply(myfiles, read.csv))

all_results_flat <- all_results_flat %>% 
  mutate(
    ModNameStratGroup = case_when( #Group by unstrat or strat
      ModName %in% c(paste0(model_version,"_NoGIS_UNC"), paste0(model_version,"_NoGIS_S")) ~ "Strat_noGIS",
      ModName %in% c(paste0(model_version,"_2GIS_UNC"), paste0(model_version,"_2GIS_S")) ~ "Strat_2GIS",
      ModName %in% c(paste0(model_version,"_BaseModel_UNC"), paste0(model_version,"_BaseModel_S")) ~ "Strat_BaseModel",
      ModName %in% c(paste0(model_version,"_BaseModel_Unstrat")) ~ "Unstrat_BaseModel",
      ModName %in% c(paste0(model_version,"_NoGIS_Unstrat")) ~ "Unstrat_noGIS",
      ModName %in% c(paste0(model_version,"_2GIS_Unstrat")) ~ "Unstrat_2GIS"
    ),
    ModNameDisplay = case_when( #Plot display names
      ModName %in% c(paste0(model_version,"_NoGIS_UNC")) ~ "UNC (NoGIS)",
      ModName %in% c(paste0(model_version,"_NoGIS_S")) ~ "S (NoGIS)",
      ModName %in% c(paste0(model_version,"_NoGIS_Unstrat")) ~ "Unstrat (NoGIS)",
      
      ModName %in% c(paste0(model_version,"_2GIS_UNC")) ~ "UNC (2GIS)",
      ModName %in% c(paste0(model_version,"_2GIS_S")) ~ "S (2GIS)",
      ModName %in% c(paste0(model_version,"_2GIS_Unstrat")) ~ "Unstrat (2GIS)",
      
      ModName %in% c(paste0(model_version,"_BaseModel_UNC")) ~ "UNC (BaseModel)",
      ModName %in% c(paste0(model_version,"_BaseModel_S")) ~ "S (BaseModel)",
      ModName %in% c(paste0(model_version,"_BaseModel_Unstrat")) ~ "Unstrat (BaseModel)"
    ),
    P_or_I = case_when(Class %in% c("P", "I") ~ TRUE, T~F),
    I_or_E = case_when(Class %in% c("I", "E") ~ TRUE, T~F),
    P_or_I_wet = case_when(P_or_I & Wet ~ TRUE, T~F),
    I_or_E_dry = case_when(I_or_E & !Wet ~ TRUE, T~F)
  )
# reorder models so that similar ones are together (helps in plotting)
all_results_flat$ModNameDisplay <- factor(all_results_flat$ModNameDisplay ,
      levels = rev(c("Unstrat (BaseModel)","UNC (BaseModel)","S (BaseModel)",
                     "Unstrat (NoGIS)","UNC (NoGIS)","S (NoGIS)",
                     "Unstrat (2GIS)","UNC (2GIS)","S (2GIS)")))


############################# ADD PRECISION ##############################
repeated_data_set <- all_results_flat %>% filter(TotalVisits > 1,
                                                 Notes=="Original")

# PvIvE_precision_df <- repeated_data_set %>%
#   group_by(ModNameDisplay, SiteCode, RF_Prediction_50, TotalVisits) %>%
#   tally(name="TimesClassified") %>%
#   group_by(ModNameDisplay, SiteCode) %>%
#   slice_max(TimesClassified, n=1, with_ties=F) %>%
#   mutate(PvIvE_precision = (TimesClassified-1)/(TotalVisits-1))

PvIvE_precision_df <- repeated_data_set %>%
  group_by(ModNameDisplay, SiteCode, RF_Prediction_50, TotalVisits) %>%
  tally(name="DifferentClassifications") %>%
  group_by(ModNameDisplay, SiteCode) %>%
  slice_max(DifferentClassifications, n=1, with_ties=F) %>%
  mutate(PvIvE_precision = (DifferentClassifications-1)/(TotalVisits-1))

write_csv(PvIvE_precision_df, file=paste0(HOME_DIR,"/output/models/PvIvE_precision_df.csv"))


EvALI_precision_df <- repeated_data_set %>%
  mutate(Classification_EvALI = case_when(
        RF_Prediction_50=="E"~"E",
        RF_Prediction_50 %in% c("P","I","ALI")~"ALI",
        T~RF_Prediction_50)) %>%
  group_by(ModNameDisplay, SiteCode, Classification_EvALI, TotalVisits) %>%
  tally(name="DifferentClassifications") %>%
  group_by(ModNameDisplay, SiteCode) %>%
  slice_max(DifferentClassifications, n=1, with_ties=F) %>%
  mutate(EvALI_precision = (DifferentClassifications-1)/(TotalVisits-1))

write_csv(EvALI_precision_df, file=paste0(HOME_DIR,"/output/models/EvALI_precision_df.csv"))


######################### COMPARISION (DOT) PLOTS ##############################
#
################################################################################
all_results_flat_grouped2 <- all_results_flat %>% 
  group_by(ModNameDisplay, Dataset, Stratification, Region_detail) %>%
  summarise(n_tests=length(SiteCode),
            n_tests_wet=sum(Wet),
            n_tests_dry=sum(!Wet),
            n_correct_PvIvE=sum(PvIvE_correct),
            n_correct_EvALI=sum(EvALI_correct),
            n_correct_PvIwet=sum(Class[P_or_I_wet]==RF_Prediction_50[P_or_I_wet]),
            n_correct_IvEdry=sum(Class[I_or_E_dry]==RF_Prediction_50[I_or_E_dry]),
            n_wet_P_or_I=sum(P_or_I_wet),
            n_dry_I_or_E=sum(I_or_E_dry)
  ) %>%
  ungroup() %>%
  mutate(pct_correct_PvIvE = n_correct_PvIvE/n_tests,
         pct_correct_EvALI = n_correct_EvALI/n_tests,
         pct_correct_PvIwet = n_correct_PvIwet/n_wet_P_or_I,
         pct_correct_IvEdry = n_correct_IvEdry/n_dry_I_or_E
  ) %>% 
  as.data.frame()

################################# DOT PLOT 1 ###################################
# Plot of PvIvE, EvALI, PvIwet, IvEdry metrics. 
# Showing individual models.
################################################################################
df_dots1 <- all_results_flat_grouped2 %>%
    select(-n_correct_PvIvE, -n_correct_EvALI) %>%
    pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
                 names_to="Measure") %>% 
    mutate(Measure=str_sub(Measure, start=13)) %>%
    filter(ModNameDisplay %in% c("UNC (NoGIS)", "S (NoGIS)", "Unstrat (NoGIS)",
                               "UNC (2GIS)", "S (2GIS)", "Unstrat (2GIS)"))

df_dots1$Measure <- factor(df_dots1$Measure,
                           levels=c("PvIvE","EvALI","PvIwet","IvEdry" ))

df_dots1$Dataset <- factor(df_dots1$Dataset, 
                           levels=c("Testing","Training"))
label_names <- c(
  `PvIvE` = "Accuracy\nPvIvE",
  `EvALI` = "Accuracy\nEvALI",
  `PvIwet` = "Accuracy\nPvIwet",
  `IvEdry` = "Accuracy\nIvEdry"
)
dotplot1 <- ggplot(data=df_dots1, 
                   aes(x=ModNameDisplay, y=Accuracy))+
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
ggsave(dotplot1, height=4, width = 9, units="in", dpi=900,
       filename=paste0(parent_path, "/dotplot1.png"))


################################# DOT PLOT 2 ###################################
# Plot of PvIvE, EvALI, PvIwet, IvEdry metrics. 
# Showing individual models.
################################################################################

mod_summ_all_models2 <- all_results_flat_grouped2 %>%
  select(-n_correct_PvIvE, -n_correct_EvALI#, -n_correct_PvNP
  ) %>%
  pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
               names_to="Measure") %>% 
  mutate(Measure=str_sub(Measure, start=13))# %>%
# bind_rows(mod_summ_all_models %>%
#             rename(Accuracy=AvgPctMode) %>%
#             mutate(Measure="Precision")) %>%
# select(-Precision)

mod_summ_all_models2$Measure <- factor(mod_summ_all_models2$Measure, 
                                       levels=c("PvIvE","EvALI"#, "PvNP","PvIwet","IvEdry",
                                                # "Precision"
                                       ))

mod_summ_all_models2$Dataset <- factor(mod_summ_all_models2$Dataset, 
                                       levels=c("Testing","Training"))
#reorder for clarity
# mod_summ_all_models2$ModNameStratGroup <- factor(mod_summ_all_models2$ModNameStratGroup,
#                              levels = c("Unstrat_noGIS","Unstrat",
#                                         "Strat_noGIS","Strat"))
label_names <- c(
  `PvIvE` = "Accuracy\nPvIvE",
  `EvALI` = "Accuracy\nEvALI",
  `PvNP` = "Accuracy\nPvNP",
  `PvIwet` = "Accuracy\nPvIwet",
  `IvEdry` = "Accuracy\nIvEdry",
  `Precision` = "Precision"
)
# mod_summ_all_models2 <- mod_summ_all_models2 %>%
#   replace_na(list(Region_detail = "Unstrat"))

# #reorder for clarity
# mod_summ_all_models2$Region_detail <- factor(mod_summ_all_models2$Region_detail,
#                                       levels = c("Unstrat", "NE","SE"))
#Rename models
mod_summ_all_models2 <- mod_summ_all_models2 %>% mutate(
  ModName3=case_when(ModName==paste0(model_version,"_2GIS_S")~"S (2GIS)",
                     ModName==paste0(model_version,"_2GIS_UNC")~"UNC (2GIS)",
                     ModName==paste0(model_version,"_2GIS_Unstrat")~"Unstrat (2GIS)",
                     ModName==paste0(model_version,"_BaseModel_S")~"S (BaseModel)",
                     ModName==paste0(model_version,"_BaseModel_UNC")~"UNC (BaseModel)",
                     ModName==paste0(model_version,"_BaseModel_Unstrat")~"Unstrat (BaseModel)",
                     ModName==paste0(model_version,"_NoGIS_S")~"S (NoGIS)",
                     ModName==paste0(model_version,"_NoGIS_UNC")~"UNC (NoGIS)",
                     ModName==paste0(model_version,"_NoGIS_Unstrat")~"Unstrat (NoGIS)"
                     # ModName=="NE_allGIS"~"NE (all GIS)",
                     # ModName=="SE_allGIS"~"SE (all GIS)"
                     #  ModName=="Strat"~"Strat"
  ))
mod_summ_all_models2$ModName3 <- factor(mod_summ_all_models2$ModName3,
                                        levels = rev(c("Unstrat (BaseModel)","UNC (BaseModel)","S (BaseModel)",
                                                       "Unstrat (NoGIS)","UNC (NoGIS)","S (NoGIS)",
                                                       "Unstrat (2GIS)","UNC (2GIS)","S (2GIS)")))
mod_summ_all_models2_plot <- ggplot(data=mod_summ_all_models2, 
                                    aes(x=ModName3, y=Accuracy))+
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
  scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
  theme_bw()
mod_summ_all_models2_plot
ggsave(mod_summ_all_models2_plot, height=4.5, width = 7.5, units="in", dpi=900,
       filename=paste0(parent_path, "/dots_PvIvE_EvALI_precision.png"))



mod_summ_all_models2_plot_filter <- ggplot(data=mod_summ_all_models2 %>% 
                                             filter(ModName3 %in% c("Unstrat (NoGIS)","UNC (NoGIS)","S (NoGIS)",
                                                                    "Unstrat (2GIS)","UNC (2GIS)","S (2GIS)")), 
                                           aes(x=ModName3, y=Accuracy))+
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
  scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
  theme_bw()
mod_summ_all_models2_plot_filter
ggsave(mod_summ_all_models2_plot_filter, height=4, width = 7, units="in", dpi=900,
       filename=paste0(parent_path, "/dots_PvIvE_EvALI_precision_filtered.png"))



df3 <- mod_summ_all_models2 %>% mutate(
  ModNameStratGroup=case_when(ModName==paste0(model_version,"_2GIS_S")~"Strat (2GIS)",
                              ModName==paste0(model_version,"_2GIS_UNC")~"Strat (2GIS)",
                              ModName==paste0(model_version,"_2GIS_Unstrat")~"Unstrat (2GIS)",
                              ModName==paste0(model_version,"_BaseModel_S")~"Strat (BaseModel)",
                              ModName==paste0(model_version,"_BaseModel_UNC")~"Strat (BaseModel)",
                              ModName==paste0(model_version,"_BaseModel_Unstrat")~"Unstrat (BaseModel)",
                              ModName==paste0(model_version,"_NoGIS_S")~"Strat (NoGIS)",
                              ModName==paste0(model_version,"_NoGIS_UNC")~"Strat (NoGIS)",
                              ModName==paste0(model_version,"_NoGIS_Unstrat")~"Unstrat (NoGIS)"
                              # ModName=="NE_allGIS"~"NE (all GIS)",
                              # ModName=="SE_allGIS"~"SE (all GIS)"
                              #  ModName=="Strat"~"Strat"
  )) #group_by( Stratification)
mod_summ_all_models3_plot_filter <- ggplot(data=df3 %>%
                                             filter(ModName3 %in% c("Unstrat (NoGIS)","UNC (NoGIS)","S (NoGIS)",
                                                                    "Unstrat (2GIS)","UNC (2GIS)","S (2GIS)")),
                                           aes(x=ModNameStratGroup, y=Accuracy))+
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
  scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
  theme_bw()
mod_summ_all_models3_plot_filter
ggsave(mod_summ_all_models3_plot_filter, height=3.5, width = 7, 
       units="in", dpi=900,
       filename=paste0(parent_path, "/dots_3.png"))







# tmp <- predictor_matrix_grouped  %>%
#   ungroup() %>% 
#   group_by(Subcat)
# tmp <- tmp %>%arrange(Subcat) %>%select(Option) %>% unique()
# tmp2 <- tmp %>%arrange(Subcat) %>%select(Subcat) %>% unique()
# 
# write_csv(tmp, file=paste0(parent_path,"/tmp.csv"))
# write_csv(tmp2, file=paste0(parent_path,"/tmp2.csv"))
# print("wrote csv tmp")
# predictor_matrix_grouped$Option <- factor(predictor_matrix_grouped$Option,
#                                           levels=unique(tmp$Option))
# predictor_matrix_grouped$Subcat <- factor(predictor_matrix_grouped$Subcat,
#                                           levels=unique(tmp2$Subcat))
# 
# predictor_matrix_grouped$ModName <- factor(predictor_matrix_grouped$ModName, 
#                                            levels = c("SE_noGIS", "NE_noGIS","Unstrat_noGIS",
#                                                       "SE_2GIS", "NE_2GIS","Unstrat_2GIS", 
#                                                       "SE_allGIS", "NE_allGIS","Unstrat_allGIS"
#                                            ))
