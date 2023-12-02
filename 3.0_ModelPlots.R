library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)
library(readxl)
# library(rstudioapi)

# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

## Helper function: If not chosen as a testing site, then keep in training set
'%!in%' <- function(x,y)!('%in%'(x,y))

############################# COMBINE ALL RESULTS ##############################
model_version <- "PrelimFinalModels" 
parent_path <- paste0(getwd(), "/output/models/", model_version)
myfiles <- list.files(parent_path, full.names = T, all.files = TRUE,
                      recursive = T, pattern = "results_for_plots.csv")
all_results_flat <- do.call(rbind, lapply(myfiles, read.csv))

all_results_flat <- all_results_flat %>% 
  mutate(ModName2 = case_when(
    ModName %in% c(paste0(model_version,"_NoGIS_UNC"), paste0(model_version,"_NoGIS_S")) ~ "Strat_noGIS",
    ModName %in% c(paste0(model_version,"_2GIS_UNC"), paste0(model_version,"_2GIS_S")) ~ "Strat_2GIS",
    ModName %in% c(paste0(model_version,"_BaseModel_UNC"), paste0(model_version,"_BaseModel_S")) ~ "Strat_BaseModel",
    ModName %in% c(paste0(model_version,"_BaseModel_Unstrat")) ~ "Unstrat_BaseModel",
    ModName %in% c(paste0(model_version,"_NoGIS_Unstrat")) ~ "Unstrat_noGIS",
    ModName %in% c(paste0(model_version,"_2GIS_Unstrat")) ~ "Unstrat_2GIS"
  ))
all_results_flat <- all_results_flat %>% 
  mutate(ModNameDisplay = case_when(
    ModName %in% c(paste0(model_version,"_NoGIS_UNC")) ~ "UNC (NoGIS)",
    ModName %in% c(paste0(model_version,"_NoGIS_S")) ~ "S (NoGIS)",
    ModName %in% c(paste0(model_version,"_NoGIS_Unstrat")) ~ "Unstrat (NoGIS)",
    
    ModName %in% c(paste0(model_version,"_2GIS_UNC")) ~ "UNC (2GIS)",
    ModName %in% c(paste0(model_version,"_2GIS_S")) ~ "S (2GIS)",
    ModName %in% c(paste0(model_version,"_2GIS_Unstrat")) ~ "Unstrat (2GIS)",
    
    ModName %in% c(paste0(model_version,"_BaseModel_UNC")) ~ "UNC (BaseModel)",
    ModName %in% c(paste0(model_version,"_BaseModel_S")) ~ "S (BaseModel)",
    ModName %in% c(paste0(model_version,"_BaseModel_Unstrat")) ~ "Unstrat (BaseModel)"
  ))
## add revisted flag
# all_results_flat <- all_results_flat %>%
#   mutate(revist = case_when(TotalVisits>1~TRUE, T~FALSE))

summ_stratified_models <- all_results_flat %>% 
  group_by(ModName2, Dataset) %>%
  summarise(n_tests=length(SiteCode),
            n_tests_wet=sum(Wet),
            n_tests_dry=sum(!Wet),
            n_correct_PvIvE=sum(Class==RF_Prediction_50),
            n_correct_PvIvE2=sum(PvIvE_correct),
            n_correct_EvALI=sum(EvALI_correct)
            # n_correct_PvNP=sum(Class_PvNP==Prediction_PvNP, na.rm=T),
            # n_correct_PvIwet=sum(Class[Wet]==Prediction[Wet]),
            # n_correct_IvEdry=sum(Class[!Wet]==Prediction[!Wet])
            ) %>%
  ungroup() %>%
  mutate(pct_correct_PvIvE = n_correct_PvIvE/n_tests,
         pct_correct_EvALI = n_correct_EvALI/n_tests
         # pct_correct_PvNP =  n_correct_PvNP/n_tests,
         # pct_correct_PvIwet = n_correct_PvIwet/n_tests_wet,
         # pct_correct_IvEdry = n_correct_IvEdry/n_tests_dry
  ) %>% 
  as.data.frame()

# write.csv(all_results_flat%>%filter(
#   ModName2=="Unstrat_BaseModel"), "all_results_flat.csv")

####### Precision
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####### plot all 6 measures by stratification, color
all_results_flat_grouped <- all_results_flat %>% 
  group_by(ModName2, Dataset, Stratification, Region_detail) %>%
  summarise(n_tests=length(SiteCode),
            n_tests_wet=sum(Wet),
            n_tests_dry=sum(!Wet),
            n_correct_PvIvE=sum(PvIvE_correct),
            n_correct_EvALI=sum(EvALI_correct)
            # n_correct_PvNP=sum(Class_PvNP==Prediction_PvNP, na.rm=T),
            # n_correct_PvIwet=sum(Class[wet]==Prediction[wet]),
            # n_correct_IvEdry=sum(Class[!wet]==Prediction[!wet])
            ) %>%
  ungroup() %>%
  mutate(pct_correct_PvIvE = n_correct_PvIvE/n_tests,
         pct_correct_EvALI = n_correct_EvALI/n_tests
         # pct_correct_PvNP =  n_correct_PvNP/n_tests,
         # pct_correct_PvIwet = n_correct_PvIwet/n_tests_wet,
         # pct_correct_IvEdry = n_correct_IvEdry/n_tests_dry
         ) %>% 
  as.data.frame()


#########PLOT EACH Region_detail
mod_summ_all_models <- all_results_flat %>% 
  filter(TotalVisits > 1) %>% 
  group_by(ModName, SiteCode, Dataset) %>%
  summarise(n_visits=length(SiteCode),
            mode=getmode(RF_Prediction_50),
            pct_mode_PvIvE=sum(RF_Prediction_50==mode)/n_visits
  ) %>%
  ungroup() %>%
  group_by(ModName, Dataset) %>%
  summarise(AvgPctMode=mean(pct_mode_PvIvE)) %>% 
  ungroup() 


all_results_flat_grouped2 <- all_results_flat %>% 
  group_by(ModName, Dataset, Stratification, Region_detail) %>%
  summarise(n_tests=length(SiteCode),
            n_tests_wet=sum(Wet),
            n_tests_dry=sum(!Wet),
            n_correct_PvIvE=sum(PvIvE_correct),
            n_correct_EvALI=sum(EvALI_correct),
            # n_correct_PvNP=sum(Class_PvNP==Prediction_PvNP, na.rm=T),
            # n_correct_PvIwet=sum(Class[wet]==Prediction[wet]),
            # n_correct_IvEdry=sum(Class[!wet]==Prediction[!wet])
            ) %>%
  ungroup() %>%
  mutate(pct_correct_PvIvE = n_correct_PvIvE/n_tests,
         pct_correct_EvALI = n_correct_EvALI/n_tests
         # pct_correct_PvNP =  n_correct_PvNP/n_tests,
         # pct_correct_PvIwet = n_correct_PvIwet/n_tests_wet,
         # pct_correct_IvEdry = n_correct_IvEdry/n_tests_dry
         ) %>% 
  as.data.frame()

mod_summ_all_models2 <- all_results_flat_grouped2 %>%
  select(-n_correct_PvIvE, -n_correct_EvALI#, -n_correct_PvNP
         ) %>%
  pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
               names_to="Measure") %>% 
  mutate(Measure=str_sub(Measure, start=13)) %>%
  bind_rows(mod_summ_all_models %>%
              rename(Accuracy=AvgPctMode) %>%
              mutate(Measure="Precision"))

mod_summ_all_models2$Measure <- factor(mod_summ_all_models2$Measure, 
                                       levels=c("PvIvE","EvALI", #"PvNP","PvIwet","IvEdry",
                                                "Precision"))

mod_summ_all_models2$Dataset <- factor(mod_summ_all_models2$Dataset, 
                                       levels=c("Testing","Training"))
#reorder for clarity
# mod_summ_all_models2$ModName2 <- factor(mod_summ_all_models2$ModName2,
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
  ModName3=case_when(ModName=="PrelimFinalModels_2GIS_S"~"S (2GIS)",
                     ModName=="PrelimFinalModels_2GIS_UNC"~"UNC (2GIS)",
                     ModName=="PrelimFinalModels_2GIS_Unstrat"~"Unstrat (2GIS)",
                     ModName=="PrelimFinalModels_BaseModel_S"~"S (BaseModel)",
                     ModName=="PrelimFinalModels_BaseModel_UNC"~"UNC (BaseModel)",
                     ModName=="PrelimFinalModels_BaseModel_Unstrat"~"Unstrat (BaseModel)",
                     ModName=="PrelimFinalModels_NoGIS_S"~"S (NoGIS)",
                     ModName=="PrelimFinalModels_NoGIS_UNC"~"UNC (NoGIS)",
                     ModName=="PrelimFinalModels_NoGIS_Unstrat"~"Unstrat (NoGIS)"
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
ggsave(mod_summ_all_models2_plot, height=4.5, width = 9.5, units="in", dpi=900,
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
ggsave(mod_summ_all_models2_plot_filter, height=4, width = 8.5, units="in", dpi=900,
       filename=paste0(parent_path, "/dots_PvIvE_EvALI_precision_filtered.png"))


######################### HEATMAP PLOTS OF CHOSEN FEATURES #####################
## Open all RF, get their features
case_list <- c(
  "2GIS_S",
  "2GIS_UNC",
  "2GIS_Unstrat",
  "NoGIS_S",
  "NoGIS_UNC",
  "NoGIS_Unstrat",
  "BaseModel_S",
  "BaseModel_UNC",
  "BaseModel_Unstrat"
)

## Dictionary of selected predictors per model
predictor_dictionary = {}
for (c in case_list){
  # c2 <- sub(".*_", "", c)
  # print(paste("\n CASE:", c, "c2:", c2))
  rfname<-paste0(parent_path,"/", c, "/RF_", c , ".rds")
  print(rfname)
  RF <- readRDS(rfname)
  predictors <- row.names(RF$importance)
  print(predictors)
  predictor_dictionary[c] = list(predictors)
  
  print("----------------")
}









# 
# mod_summ <- all_results_flat %>% 
#   # filter(SiteCode %in% revisited_sites) %>%
#   filter(TotalVisits > 1) %>% ##TODO: 
#   group_by(ModName2, SiteCode, Dataset) %>%
#   summarise(n_visits=length(SiteCode),
#             mode=getmode(Prediction),
#             pct_mode_PvIvE=sum(Prediction==mode)/n_visits
#   ) %>%
#   ungroup() %>%
#   group_by(ModName2, Dataset) %>%
#   summarise(AvgPctMode=mean(pct_mode_PvIvE)) %>% 
#   ungroup() 
# 
# mod_summ2 <- all_results_flat_grouped %>%
#   select(-n_correct_PvIvE, -n_correct_EvALI, -n_correct_PvNP) %>%
#   pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
#                names_to="Measure") %>% 
#   mutate(Measure=str_sub(Measure, start=13)) %>%
#   bind_rows(mod_summ %>%
#               rename(Accuracy=AvgPctMode) %>%
#               mutate(Measure="Precision"))
# 
# mod_summ2$Measure <- factor(mod_summ2$Measure, 
#                       levels=c("PvIvE","EvALI", "PvNP","PvIwet","IvEdry",
#                                "Precision"))
# 
# mod_summ2$Dataset <- factor(mod_summ2$Dataset, 
#                             levels=c("Testing","Training"))
# #reorder for clarity
# mod_summ2$ModName2 <- factor(mod_summ2$ModName2,
#                              levels = c("Unstrat_noGIS","Unstrat",
#                                         "Strat_noGIS","Strat"))
# label_names <- c(
#   `PvIvE` = "Accuracy\nPvIvE",
#   `EvALI` = "Accuracy\nEvALI",
#   `PvNP` = "Accuracy\nPvNP",
#   `PvIwet` = "Accuracy\nPvIwet",
#   `IvEdry` = "Accuracy\nIvEdry",
#   `Precision` = "Precision"
# )
# mod_summ2 <- mod_summ2 %>%
#   replace_na(list(Region_detail = "Unstrat"))
# 
# #reorder for clarity
# mod_summ2$Region_detail <- factor(mod_summ2$Region_detail,
#                            levels = c("Unstrat", "NE","SE"))
# ##Rename models
# mod_summ2 <- mod_summ2 %>% mutate(
#   ModName3=case_when(ModName2=="Unstrat_noGIS"~"Unstrat (no GIS)",
#                      ModName2=="Unstrat_allGIS"~"Unstrat (all GIS)",
#                      ModName2=="Unstrat_2GIS"~"Unstrat (2 GIS)",
#                      ModName2=="Strat_noGIS"~"Strat (no GIS)",
#                      ModName2=="Strat_allGIS"~"Strat (all GIS)",
#                      ModName2=="Strat_2GIS"~"Strat (2 GIS)"))
# mod_summ2$ModName3 <- factor(mod_summ2$ModName3,
#                              levels = rev(c("Unstrat (all GIS)",
#                                             "Strat (all GIS)",
#                                             "Unstrat (no GIS)",
#                                             "Strat (no GIS)",
#                                             "Unstrat (2 GIS)",
#                                             "Strat (2 GIS)")))
# mod_summ_plot<-ggplot(data=mod_summ2, aes(x=ModName3, y=Accuracy))+
#   # geom_point(aes(size=Dataset, color=Region_detail))+
#   geom_jitter(aes(size=Dataset, color=Region_detail), width=0.04, height=0)+
#   scale_color_manual(
#     values=c(
#       "Unstrat"="black",
#       "NE"="red",
#       "SE"="blue")) +
#   scale_size_manual(values=c(1,2.5))+
#   facet_wrap(~Measure, nrow = 1, scales="free_x",
#              labeller=as_labeller(label_names))+
#   coord_flip()+
#   xlab("")+
#   # labs(title="Summary of All Model Performance")+
#   scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
#   theme_bw()
# mod_summ_plot
# # ggsave(mod_summ_plot, height=4.5, width = 9.5, units="in", dpi=900,
# #        filename=paste0(parent_path,"/dots_4models_6metrics.png"))
# 
# ############## PLOT: Simple summary 7 models 3 metrics
# mod_summ2 <- mod_summ2 %>%
#   replace_na(list(Region_detail = "Unstrat"))
# mod_summ_plot<-ggplot(data=mod_summ2%>%
#                         filter(Measure %in% c("PvIvE","EvALI","Precision")), 
#                       aes(x=ModName3, y=Accuracy))+
#   geom_point(aes(size=Dataset, color=Region_detail#, alpha=0.6
#   ))+
#   scale_size_manual(values=c(1,2.5))+
#   facet_wrap(~Measure, nrow = 1, scales="free_x")+
#   coord_flip()+
#   xlab("")+
#   labs(title="Summary of All Model Performance")+
#   scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
#   theme_bw()
# mod_summ_plot
# # ggsave(mod_summ_plot, height=4.5, width = 9.5, dpi=300,
# #        filename=paste0(parent_path, "/dots_4models_3metrics.png"))
# 
# 
# ############## PLOT: Simple summary just 4models 3 metrics
# label_names <- c(
#   `PvIvE` = "Accuracy\nPvIvE",
#   `EvALI` = "Accuracy\nEvALI",
#   `Precision` = "Precision"
# )
# mod_summ_simple5 <- mod_summ2 %>%
#   filter(ModName2 %in% c("Strat","Strat_noGIS","Unstrat",
#                          "Unstrat_noGIS")) %>%
#   filter(Measure %in% c("PvIvE","EvALI","Precision")) %>%
#   replace_na(list(Region_detail = "Unstrat"))
# 
# #reorder for clarity
# mod_summ_simple5$Region_detail <- factor(mod_summ_simple5$Region_detail,
#                                   levels = c("Unstrat","NE","SE"))
# 
# mod_summ_plot5<-ggplot(data=mod_summ_simple5, aes(x=ModName2, y=Accuracy))+
#   # geom_point(aes(size=Dataset, color=Region_detail))+
#   geom_jitter(aes(size=Dataset, color=Region_detail), width=0.07, height=0)+
#   scale_size_manual(values=c(1,2.5))+
#   facet_wrap(~Measure, nrow = 1, scales="free_x", 
#              labeller=as_labeller(label_names)
#   )+
#   coord_flip()+
#   xlab("")+
#   labs(title="Summary of All Model Performance")+
#   scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
#   theme_bw() +
#   scale_color_manual(values=c("grey38", "goldenrod3",
#                               "springgreen3","deepskyblue3","violetred3"))
# mod_summ_plot5
# # ggsave(mod_summ_plot5, height=4, width = 8, dpi=300,
# #        filename=paste0(parent_path,"/dots_4models_3metrics.png"))
# 
# 
# ####### SAVE SUMM FILE FOR REPORT
# report_summary <- all_results_flat %>% 
#   group_by(ModName2, Region_detail, Dataset) %>%
#   summarise(n_tests=length(SiteCode),
#             n_tests_wet=sum(wet),
#             n_tests_dry=sum(!wet),
#             n_correct_PvIvE=sum(Class==Prediction),
#             n_correct_EvALI=sum(Class_EvALI==Prediction_EvALI, na.rm=T),
#             n_correct_PvNP=sum(Class_PvNP==Prediction_PvNP, na.rm=T),
#             n_correct_PvIwet=sum(Class[wet]==Prediction[wet]),
#             n_correct_IvEdry=sum(Class[!wet]==Prediction[!wet])) %>%
#   ungroup() %>%
#   mutate(pct_correct_PvIvE = n_correct_PvIvE/n_tests,
#          pct_correct_EvALI = n_correct_EvALI/n_tests,
#          pct_correct_PvNP =  n_correct_PvNP/n_tests,
#          pct_correct_PvIwet = n_correct_PvIwet/n_tests_wet,
#          pct_correct_IvEdry = n_correct_IvEdry/n_tests_dry) %>% 
#   as.data.frame()
# 
# 
# report_precision <- all_results_flat %>% 
#   # filter(SiteCode %in% revisited_sites) %>%
#   filter(TotalVisits > 1) %>% ##TODO: 
#   group_by(ModName2, SiteCode, Dataset,Region_detail) %>%
#   summarise(n_visits=length(SiteCode),
#             mode=getmode(Prediction),
#             pct_mode_PvIvE=sum(Prediction==mode)/n_visits
#   ) %>%
#   ungroup() %>%
#   group_by(ModName2, Dataset,Region_detail) %>%
#   summarise(AvgPctMode=mean(pct_mode_PvIvE)) %>% 
#   ungroup() 
# write_csv(report_precision, file=paste0(parent_path,"/report_precision.csv"))
# 
# report_summary2 <- report_summary %>%
#   select(-n_correct_PvIvE, -n_correct_EvALI, -n_correct_PvNP) %>%
#   pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
#                names_to="Measure") %>% 
#   mutate(Measure=str_sub(Measure, start=13)) %>%
#   bind_rows(report_precision %>%
#               rename(Accuracy=AvgPctMode) %>%
#               mutate(Measure="Precision"))
# write_csv(report_summary2, file=paste0(parent_path,"/report_summary2.csv"))
# 


##############################  LISTS OF DATA ##################################
# Lists to store features (for convenience).
#
################################################################################

# use data dictionary to subset data into lists
metrics_lookup <- read_xlsx("input/raw/metrics_dictionary.xlsx",
                            sheet = "DATA_DICT") %>%
  filter(MetricSubtype!="Direct") %>%
  filter(GP_final=="TRUE") %>%
  filter(MetricCandidate_KF=="TRUE")
metrics_lookup <- metrics_lookup[!duplicated(metrics_lookup), ]

# info_list <- (parameters_info %>% filter(Action =="info"))$Column
# info_list <- replace(info_list, info_list=="Determination_final","Class") #Rename class
# info_list <- setdiff(info_list, "Disturbances")
# info_list <- c(info_list, "Lat_field","Long_field")
# candidate_list <- (parameters_info %>% filter(Action =="candidate"))$Column
# candidate_list <- setdiff(candidate_list, c("BMI_score","BMI_score_alt1","BMI_score_alt2","BMI_score_alt3","BMI_score_alt4"))

bio_list <- (metrics_lookup %>% filter(MetricType =="Bio"))$Metric
geomorph_list <- (metrics_lookup %>% filter(MetricType =="Geomorph"))$Metric
gis_list <- (metrics_lookup %>% filter(MetricType =="Geospatial"))$Metric
h20_indirect_list <- (metrics_lookup %>% filter(MetricType =="Hydro"))$Metric


subcat_list_algae <- (metrics_lookup %>% filter(MetricSubtype =="Algae"))$Metric
subcat_list_bmi <- (metrics_lookup %>% filter(MetricSubtype =="BMI"))$Metric
subcat_list_climate <- (metrics_lookup %>% filter(MetricSubtype =="Climate"))$Metric
subcat_list_geomorph <- (metrics_lookup %>% filter(MetricSubtype =="Geomorph"))$Metric
subcat_list_indir <- (metrics_lookup %>% filter(MetricSubtype =="Indirect"))$Metric
subcat_list_veg <- (metrics_lookup %>% filter(MetricSubtype =="Veg"))$Metric
subcat_list_vert <- (metrics_lookup %>% filter(MetricSubtype =="Vertebrates"))$Metric
subcat_list_watershed <- (metrics_lookup %>% filter(MetricSubtype =="Watershed"))$Metric


# Get all candidate predictors that passed screening
predictor_summary <- read_csv(paste0(HOME_DIR,
                                   "/output/screening/metric_summary.csv"))
predictor_summary <- predictor_summary %>% select(c("Predictor", "PassScreens"))
candidates_passed_screen <- (predictor_summary %>% filter(PassScreens =="TRUE"))$Predictor
candidates_passed_screen <- c(candidates_passed_screen, "Strata")## Add region
# candidates_passed_screen <- setdiff(candidates_passed_screen, c("BMI_score",
#           "BMI_score_alt1","BMI_score_alt2","BMI_score_alt3","BMI_score_alt4"))

# ###NEWSTUFF
# candidates_passed_screen <- setdiff(candidates_passed_screen, c("ppt","ppt.m01","ppt.m02","ppt.m03","ppt.m04","ppt.m05",
#                                                                 "ppt.m06","ppt.m07","ppt.m08","ppt.m09","ppt.m10","ppt.m11",
#                                                                 "ppt.m12","tmax","tmin","tmean","temp.m01","temp.m02","temp.m03",
#                                                                 "temp.m04","temp.m05","temp.m06","temp.m07","temp.m08","temp.m09","temp.m10","temp.m11","temp.m12"))
# gis_list <- setdiff(gis_list,c("ppt","ppt.m01","ppt.m02","ppt.m03","ppt.m04","ppt.m05",
#                                "ppt.m06","ppt.m07","ppt.m08","ppt.m09","ppt.m10","ppt.m11",
#                                "ppt.m12","tmax","tmin","tmean","temp.m01","temp.m02","temp.m03",
#                                "temp.m04","temp.m05","temp.m06","temp.m07","temp.m08","temp.m09","temp.m10","temp.m11","temp.m12"))
# candidates_passed_screen <- c(candidates_passed_screen, "ppt.234","ppt.567","ppt.8910","ppt.11121","temp.234","temp.567","temp.8910","temp.11121")
# gis_list <- c(gis_list, "ppt.234","ppt.567","ppt.8910","ppt.11121","temp.234","temp.567","temp.8910","temp.11121")
# ###NEWSTUFF

EligiblePreds <- candidates_passed_screen #setdiff(candidates_passed_screen, "BFI")
EligiblePreds_noGIS <- setdiff(candidates_passed_screen, gis_list)

# no_possible <- length(candidate_list)
no_eligible <- length(candidates_passed_screen)
no_eligible_noGIS <- length(EligiblePreds_noGIS)

# models <- all_results_flat$ModName %>%unique()
models <- all_results_flat$ModNameDisplay %>%unique()
## Create summary matrix of selections
# predictor_matrix <- data.frame(candidate_list) %>% 
predictor_matrix <- data.frame(candidates_passed_screen) %>% 
  crossing(unique(all_results_flat$ModNameDisplay)) %>%
  rename(ModNameDisplay="unique(all_results_flat$ModNameDisplay)",
         Option="candidates_passed_screen") %>%
  group_by(ModNameDisplay) %>% mutate(
    Selected = case_when(
      (Option %in% unlist(predictor_dictionary[["BaseModel_Unstrat"]]) & ModNameDisplay=="Unstrat (BaseModel)")~T, 
      (Option %in% unlist(predictor_dictionary[["BaseModel_UNC"]]) & ModNameDisplay=="UNC (BaseModel)")~T, 
      (Option %in% unlist(predictor_dictionary[["BaseModel_S"]]) & ModNameDisplay=="S (BaseModel)")~T, 
      (Option %in% unlist(predictor_dictionary[["2GIS_Unstrat"]]) & ModNameDisplay=="Unstrat (2GIS)")~T, 
      (Option %in% unlist(predictor_dictionary[["2GIS_UNC"]]) & ModNameDisplay=="UNC (2GIS)")~T, 
      (Option %in% unlist(predictor_dictionary[["2GIS_S"]]) & ModNameDisplay=="S (2GIS)")~T, 
      (Option %in% unlist(predictor_dictionary[["NoGIS_Unstrat"]]) & ModNameDisplay=="Unstrat (NoGIS)")~T, 
      (Option %in% unlist(predictor_dictionary[["NoGIS_UNC"]]) & ModNameDisplay=="UNC (NoGIS)")~T,
      (Option %in% unlist(predictor_dictionary[["NoGIS_S"]]) & ModNameDisplay=="S (NoGIS)")~T,
      T~F),
    IncludeGIS = case_when(
      ModNameDisplay %in% c("Unstrat (BaseModel)","UNC (BaseModel)","S (BaseModel)")~T,
      T~F),
    nEligible = case_when(
      ModNameDisplay %in% c("Unstrat (BaseModel)","UNC (BaseModel)","S (BaseModel)",
                            "Unstrat (2GIS)","UNC (2GIS)","S (2GIS)")~no_eligible,
      ModNameDisplay %in% c("Unstrat (NoGIS)","UNC (NoGIS)","S (NoGIS)")~no_eligible_noGIS),
    PredGroup = case_when(
      Option %in% bio_list~"Biological",
      Option %in% geomorph_list~"Geomorphological",
      # Option %in% h20_direct_list ~ "Hydrological",
      Option %in% h20_indirect_list ~ "H20_Indirect",
      Option %in% gis_list~"GIS",
      T~"Other"),
    Subcat = case_when(
      Option %in% subcat_list_algae~"Algae",
      Option %in% subcat_list_bmi~"BMI",
      Option %in% subcat_list_climate~"Climate",
      Option %in% subcat_list_geomorph~"Geomorph",
      Option %in% subcat_list_indir~"H20 (indirect)",
      Option %in% subcat_list_veg~"Veg",
      Option %in% subcat_list_vert~"Vert",
      Option %in% subcat_list_watershed~"Watershed",
      # Option %in% subcat_list_ai~"AI",
      # Option %in% subcat_list_plants~"Plants",
      # Option %in% subcat_list_fish~"Fish",
      # Option %in% subcat_list_fungi~"Fungi",
      Option %in% subcat_list_climate~"Other",
      T~"Other"),
    Eligible = case_when(
      !PredGroup %in% c("Hydrological","GIS")~T,
      PredGroup == "GIS" & IncludeGIS~T,
      PredGroup == "Hydrological" ~F,
      T~F),
    NoPredsSelected = sum(Selected),
    Fill = case_when(
      Selected == T ~ "Selected",
      (Eligible == T & Selected == F) ~ "NotSelected",
      # Option %in% failed_screen_list~"FailedScreen",
      T~"NotEligible"
    ),
    # # Fill = case_when(
    # #   Option %in% failed_screen_list~"FailedScreen",
    # #   T~Fill
    # # ),
    Strata = case_when(
      ModNameDisplay %in% c("UNC (BaseModel)", "UNC (NoGIS)", "UNC (2GIS)")~"UNC",
      ModNameDisplay %in% c("S (BaseModel)", "S (NoGIS)", "S (2GIS)")~"S",
      T~"Unstratified"
    ),
    Stratified = case_when(
      ModNameDisplay %in% c("UNC (BaseModel)", "UNC (NoGIS)", "UNC (2GIS)",
                     "S (BaseModel)", "S (NoGIS)", "S (2GIS)")~T,
      T~F
    )
  ) %>% ungroup() 


predictor_matrix <- predictor_matrix  %>%
  group_by(Subcat)
predictor_matrix <- predictor_matrix %>%arrange(Subcat) #%>%select(Option) %>% unique()



# write_csv(predictor_matrix, file=paste0(parent_path,"/predictor_matrix.csv"))
# print("wrote csv1")
# 
# # combine strat models
predictor_matrix_grouped <- predictor_matrix %>%
  mutate(ModNameGrouped = case_when(
      ModNameDisplay %in% c("UNC (NoGIS)", "S (NoGIS)") ~ "Strat_NoGIS",
      ModNameDisplay %in% c("UNC (2GIS)", "S (2GIS)") ~ "Strat_2GIS",
      ModNameDisplay %in% c("UNC (BaseModel)", "S (BaseModel)") ~ "Strat_BaseModel",
      ModNameDisplay %in% c("Unstrat (NoGIS)") ~ "Unstrat_NoGIS",
      ModNameDisplay %in% c("Unstrat (2GIS)") ~ "Unstrat_2GIS",
      ModNameDisplay %in% c("Unstrat (BaseModel)") ~ "Unstrat_BaseModel"
  )) %>% group_by(ModNameGrouped, Option) %>%
  mutate(FillCount=sum(Selected)) %>%
  ungroup() %>% mutate(
    FillTimes = case_when(
      # Fill == "FailedScreen" ~ "FailedScreen",
      FillCount == 0 & Eligible == TRUE ~ "Not Selected",
      # FillCount == 1 & Stratified == TRUE ~ "Selected - 1 Region",
      # FillCount == 1 & Stratified == FALSE ~ "Selected - Unstrat",
      # FillCount == 2 & Stratified == TRUE ~ "Selected - 2 Region",
      # FillCount == 3 & Stratified == TRUE ~ "Selected - 3 Region",
      # FillCount == 4 & Stratified == TRUE~ "Selected - All Region",
      Eligible == FALSE ~ "NotEligible"
    ),
    color_group = case_when(
      FillTimes == "Not Selected" ~ "not_selected_color",
      # FillTimes == "FailedScreen" ~ "failed_screen_color",
      # FillTimes == "Selected - 1 Region" ~ "light_color",
      # FillTimes == "Selected - 2 Region" ~ "med_color",
      # FillTimes == "Selected - 3 Region" ~ "red", #dark_color",
      FillTimes == "NotEligible" ~ "not_elig_color"
    )
  )
# #reorder for clarity
# predictor_matrix_grouped$ModName2 <- factor(predictor_matrix_grouped$ModNameGrouped,
#              levels = c("Unstrat_BaseModel","Unstrat_2GIS","Unstrat_NoGIS",
#                         "Strat_BaseModel","Strat_2GIS","Strat_NoGIS"))

## order xaxis by group
predlist <- arrange(predictor_matrix_grouped %>%ungroup(),
                    desc(Option),
                    group_by = PredGroup)
tmp <- predictor_matrix_grouped  %>%
                ungroup() %>%
                group_by(PredGroup)
tmp <- tmp %>% arrange(PredGroup) %>%select(Option) %>% unique()
predictor_matrix_grouped$Option <- factor(predictor_matrix_grouped$Option,
                                          levels=unique(tmp$Option))

######## HORIZONTAL
predictor_matrix_grouped$Option <- factor(predictor_matrix_grouped$Option,
                              levels=unique(tmp$Option))
heatmap <- ggplot(
  data=predictor_matrix_grouped,
  mapping = aes(x=Option, y=ModNameGrouped)) +
  geom_tile(aes(fill=factor(color_group)), color="grey30") +
  scale_fill_manual(
    values=c(
             "not_elig_color"="white",
             "failed_screen_color"="grey",
             "not_selected_color"="gray50",
             "light_color"="lightblue1",
             "med_color"="cornflowerblue",
             "dark_color"="blue"
             ),
    labels=c(
             "not_elig_color"="Not Elgiible",
             "failed_screen_color"="Did not pass screening",
             "not_selected_color"="Not Selected",
             "light_color"="Selected - 1 Region",
             "med_color"="Selected - 2 Region",
             "dark_color"="Selected - 3 Region"
             ),
    name="Features" ) +
  # xlab("Indicators")+
  ylab("")+ labs(
    title="Which Indicators are Used in the Model(s)",
    fill="Candidates")+
  theme_bw() + xlab("")+
  theme(axis.text.x = element_text( angle = 90,#60,
                                    vjust = 1,
                                    hjust=1, size=7.5
                                    # colour = colors
  ))
heatmap
ggsave(heatmap, height=4.5, width =12, dpi=300,
       filename="output/models/heatmap_horizontal.png")





### all version
## order xaxis by group
# predlist <- arrange(predictor_matrix_grouped %>%ungroup(), 
#                     desc(Option), 
#                     group_by = PredGroup)
# tmp <- predictor_matrix_grouped  %>%
#   ungroup() %>% 
#   group_by(Subcat)
# tmp <- tmp %>%arrange(Subcat) %>%select(Option) %>% unique()


tmp <- predictor_matrix_grouped  %>%
  ungroup() %>% 
  group_by(Subcat)
tmp <- tmp %>%arrange(Subcat) %>%select(Option) %>% unique()
tmp2 <- tmp %>%arrange(Subcat) %>%select(Subcat) %>% unique()

write_csv(tmp, file=paste0(parent_path,"/tmp.csv"))
write_csv(tmp2, file=paste0(parent_path,"/tmp2.csv"))
print("wrote csv tmp")
predictor_matrix_grouped$Option <- factor(predictor_matrix_grouped$Option,
                                          levels=unique(tmp$Option))
predictor_matrix_grouped$Subcat <- factor(predictor_matrix_grouped$Subcat,
                                          levels=unique(tmp2$Subcat))

predictor_matrix_grouped$ModName <- factor(predictor_matrix_grouped$ModName, 
                                           levels = c("SE_noGIS", "NE_noGIS","Unstrat_noGIS",
                                                      "SE_2GIS", "NE_2GIS","Unstrat_2GIS", 
                                                      "SE_allGIS", "NE_allGIS","Unstrat_allGIS"
                                           ))
# write_csv(predictor_matrix_grouped, file=paste0(parent_path,"/predictor_matrix_grouped.csv"))
# print("wrote csv predictor_matrix_grouped")
# heatmap_all <- ggplot(
#   data=predictor_matrix_grouped, 
#   mapping = aes(x=Option, y=ModName)) +
#   geom_tile(aes(fill=factor(Fill)), color="grey30") +
#   scale_fill_manual(
#     values=c(
#             "NotEligible"="white",
#             "NotSelected"="gray50",
#             # "FailedScreen"="grey",
#             "Selected"="blue"),
#     name="Features" ) +
#   # xlab("Indicators")+
#   ylab("")+ labs(
#     title="Which Indicators are Used in the Model(s)", 
#     fill="Candidates")+
#   theme_bw() + xlab("")+
#   theme(axis.text.x = element_text( angle = 90,#60,
#                                     vjust = 1,
#                                     hjust=1, size=7.5
#                                     # colour = colors
#   ))
# heatmap_all
# ggsave(heatmap_all, height=6, width =12, dpi=300,
#        filename=paste0(parent_path,"/heatmap_horizontal_all.png"))

######## VERTICAL
# ## Reorder for clarity
predictor_matrix_grouped$Option <- factor(predictor_matrix_grouped$Option,
                                          levels=rev(unique(tmp$Option)))


predictor_matrix_grouped$ModName <- factor(predictor_matrix_grouped$ModName, 
                                           levels = c("Unstrat_noGIS","NE_noGIS","SE_noGIS",
                                                      "Unstrat_2GIS","NE_2GIS","SE_2GIS",
                                                      "Unstrat_allGIS","NE_allGIS","SE_allGIS"))
# predictor_matrix_grouped$ModName3 <- factor(predictor_matrix_grouped$ModName3,
#                                             levels = c("Unstrat","Unstrat noGIS",
#                                                        "Strat","Strat noGIS" ))
# predictor_matrix_grouped$Option <- factor(predictor_matrix_grouped$Option, 
#                                           levels=rev(unique(tmp$Option)))
heatmap_vertical <- ggplot(
  data=predictor_matrix_grouped, 
  mapping = aes(y=Option, x=ModName)) +
  geom_tile(aes(fill=factor(Fill)), color="grey30") +
  scale_fill_manual(
    values=c(
      "NotEligible"="white",
      "NotSelected"="gray50",
      # "FailedScreen"="grey",
      "Selected"="blue"),
    name="" ) +
  ylab("")+ labs(
    title="Which Indicators are Used in the Model(s)", 
    fill="Candidates")+
  theme_bw() + xlab("")+
  theme(axis.text.x = element_text( angle =90,
                                    vjust = 1,
                                    hjust=1, size=9
  ))
ggsave(heatmap_vertical, height=12, width =7, dpi=900,
       filename=paste0(parent_path,"/heatmap_vertical.png"))

print(paste0("wrote to :", parent_path,"/heatmap_vertical.png"))


# 
# ### condensed version NO GIS
# heatmap_cond_noGIS <- ggplot(
#   data=pred_options_cond %>% 
#     filter(IncludeGIS == F) %>%
#     group_by(Option) %>%
#     mutate(TimesSel = sum(Selected)) %>% 
#     ungroup() %>% 
#     filter(TimesSel>0), 
#   mapping = aes(x=Option, y=ModName2)) +
#   geom_tile(aes(fill=factor(color_group)), color="grey30") +
#   scale_fill_manual(
#     values=c("grey"="grey",
#              # "white"="white",
#              "blue"="blue"),
#     labels=c("grey"="Not Selected",
#              # "white"="Not Elgiible",
#              "blue"="Selected"
#     ),
#     name="Features" ) +
#   xlab("")+ylab("")+labs(
#     title="Which Indicators are Used in the Model(s)", 
#     fill="Candidates")+
#   theme_bw() + 
#   theme(axis.text.x = element_text( angle = 90, 
#                                     vjust = 0.5, 
#                                     hjust=1
#   ))
# heatmap_cond_noGIS
# ggsave(heatmap_cond_noGIS, height=4, width = 8, dpi=300,
#        filename="output/models/heatmap_cond_noGIS.png")



# ##################### PLOT Region_detail RESULTS - PvIvE
# all_results_flat_grouped$Dataset <- factor(all_results_flat_grouped$Dataset, 
#                             levels=c("Training","Testing"))

# Unstrat_noGIS_plot1 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Unstrat_noGIS") %>% group_by(Dataset), 
#        aes(x=Region_detail, y=pct_correct_PvIvE*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Unstratified Model (No GIS)",
#        subtitle = "Classification % Correct between P vs I vs E",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Unstrat_noGIS"))[1])
#        ) +
#   geom_text( aes(label=round(pct_correct_PvIvE*100, digits=1)),
#             vjust=1.3, color="black", size=2.5, 
#             position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_brewer(palette="Set2") + theme_minimal()
# Unstrat_noGIS_plot1

# Unstrat_noGIS_plot1_notitle <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Unstrat_noGIS") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_PvIvE*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(#title = "Unstratified Model (No GIS)",
#     subtitle = "Classification % Correct between P vs I vs E",
#     caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#       ModName2=="Unstrat_noGIS"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_PvIvE*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_brewer(palette="Set2") + theme_minimal()
# Unstrat_noGIS_plot1_notitle

# Unstrat_plot1 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Unstrat") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_PvIvE*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Unstratified Model (GIS)",
#        subtitle = "Classification % Correct between P vs I vs E",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Unstrat"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_PvIvE*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_brewer(palette="Set2") + theme_minimal()

# Strat_plot1 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Strat") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_PvIvE*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Stratified Model (GIS)",
#        subtitle = "Classification % Correct between P vs I vs E",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Strat"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_PvIvE*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_brewer(palette="Set2") + theme_minimal()

# Strat_noGIS_plot1 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Strat_noGIS") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_PvIvE*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Stratified Model (No GIS)",
#        subtitle = "Classification % Correct between P vs I vs E",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Strat_noGIS"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_PvIvE*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_brewer(palette="Set2") + theme_minimal()

# library(gridExtra)
# ggsave(paste0(parent_path,"/accuracy_Unstrat_Region.png"), 
#        arrangeGrob(Unstrat_noGIS_plot1, Unstrat_plot1, nrow = 1),
#        dpi=300, height=4, width=9)

# ggsave(paste0(parent_path,"/accuracy_NESEunstrat_Region.png"), 
#        arrangeGrob(Strat_plot1, Strat_noGIS_plot1, nrow = 1),
#        dpi=300, height=4, width=9)

# ggsave(paste0(parent_path,"/accuracy_noGIS_Region.png"), 
#        arrangeGrob(Unstrat_noGIS_plot1, Strat_noGIS_plot1, nrow = 1),
#        dpi=300, height=4, width=9)

# ##################### PLOT Region_detail RESULTS - EvALI

# Unstrat_noGIS_plot2 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Unstrat_noGIS") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_EvALI*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(#title = "Unstratified Model (No GIS)",
#        subtitle = "% Correct: Ephemerval vs At Least Intermittent",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Unstrat_noGIS"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_EvALI*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_manual(values=c("skyblue2",
#                              "orangered1"))

# Unstrat_plot2 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Unstrat") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_EvALI*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Unstratified Model (GIS)",
#        subtitle = "% Correct: Ephemerval vs At Least Intermittent",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Unstrat"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_EvALI*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_manual(values=c("skyblue2",
#                              "orangered1"))

# Strat_plot2 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Strat") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_EvALI*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Stratified Model (GIS)",
#        subtitle = "% Correct: Ephemerval vs At Least Intermittent",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Strat"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_EvALI*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_manual(values=c("skyblue2",
#                              "orangered1"))

# Strat_noGIS_plot2 <- ggplot(all_results_flat_grouped%>%filter(
#   ModName2=="Strat_noGIS") %>% group_by(Dataset), 
#   aes(x=Region_detail, y=pct_correct_EvALI*100, fill=Dataset)) + 
#   geom_bar(stat="identity", position=position_dodge(width = 1)) +
#   labs(title = "Stratified Model (No GIS)",
#        subtitle = "% Correct: Ephemerval vs At Least Intermittent",
#        caption=paste0("Total observations: ", dim(all_results_flat%>%filter(
#          ModName2=="Strat_noGIS"))[1])
#   ) +
#   geom_text( aes(label=round(pct_correct_EvALI*100, digits=1)),
#              vjust=1.3, color="black", size=2.5, 
#              position=position_dodge(width = 1)) + 
#   ylab("% Correct") + ylim(0,100)+ 
#   scale_fill_manual(values=c("skyblue2",
#                              "orangered1"))

# ggsave(paste0(parent_path, "/accuracy_EvALI_Unstrat_Region.png"), 
#        arrangeGrob(Unstrat_noGIS_plot2, Unstrat_plot2, nrow = 1),
#        dpi=300, height=4, width=9)

# ggsave(paste0(parent_path, "/accuracy_EvALI_NESEunstrat_Region.png"), 
#        arrangeGrob(Strat_plot2, Strat_noGIS_plot2, nrow = 1),
#        dpi=300, height=4, width=9)

# ggsave(paste0(parent_path,"/accuracy_EvALI_noGIS_Region.png"), 
#        arrangeGrob(Unstrat_noGIS_plot2, Strat_noGIS_plot2, nrow = 1),
#        dpi=300, height=4, width=9)

# ggsave(paste0(parent_path, "/accuracy_PvIvE_EvALI_noGIS_Region.png"), 
#        arrangeGrob(Unstrat_noGIS_plot1_notitle, Unstrat_noGIS_plot2, nrow = 1,
#           top="Unstratified Unstratified Model (No GIS)"),
#        dpi=300, height=4, width=9)

# ggsave(paste0(parent_path,"/accuracy_PvIvE_NESEunstrat_Region.png"), 
#        arrangeGrob(Unstrat_noGIS_plot1, Unstrat_plot1, nrow = 1),
#        dpi=300, height=4, width=9)


