################################################################################
# This script uses "Single Indicators" to override classifications made during 
# earlier model building. The presence of Single Indicators means that the 
# classification automatically becomes ALI. This may correct some classifications,
# or possibly introduce new error. 
#
# Note: last input dataset update from Rafi on 25 Jan 2024
################################################################################
library(rlang)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(stringr)
library(readxl)

model_version <- "DraftFinalModels2"
chosen_model <- "NoGIS_Unstrat"

########################## Get Data ############################################
# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Create output dirs if they do not exist
input_clean_dir <- paste0(HOME_DIR, "/input/processed")
if (!dir.exists(input_clean_dir)){dir.create(input_clean_dir)}

parent_path <- paste0(HOME_DIR, "/output/models/", model_version, "/", chosen_model)

out_dir <- paste0(parent_path, "/SI")
if (!dir.exists(out_dir)){dir.create(out_dir)}

refined_files <- list.files(parent_path, full.names = T, all.files = TRUE,
                      recursive = T, pattern = "refined_results_summary.csv")
df_results <- do.call(rbind, lapply(refined_files, read.csv))
write.csv(df_results, paste0(out_dir, "/all_refined_results.csv"))


info_list <- c("ParentGlobalID","CollectionDate","Region",
               "Region_detail","SiteCode","Class","Dataset", "Wet")


####################### Columns needed for SI analysis ######################### 
addl_cols <- c("ParentGlobalID", "SiteCode", 
               "Fish_PA","Fish_PA_nomosq", "Turtles_yn",
               "perintper_ISA_abundance","TotalAbundance",
               "EPT_abundance","perennial_ISA_abundance",
               "AlgalCover_LiveOrDead_NoUpstream","HydricSoils_score", 
               "ironox_bfscore_NM","hydrophytes_present","AmphSnake_PA")

df_addl <- read_csv(paste0(HOME_DIR,"/input/raw/df_main_20240125.csv"))
df_addl_subset <- df_addl[, addl_cols ]

# Join these addl columns needed for SI analysis to the main dataframe 
df <- df_results %>%
  left_join(df_addl_subset) 


df_si <- df %>% mutate(
  # SI_Fish_PA = case_when(is.na(Fish_PA)~0, Fish_PA==1~1, T~0), #This includes mosquitofish, and was a candidate metric
  SI_Fish_NoMosq_PA = case_when(is.na(Fish_PA_nomosq)~0, Fish_PA_nomosq==1~1, T~0), #This excludes mosuitosifh and was not a candidate metric
  
  SI_BMI_PA = case_when(is.na(TotalAbundance)~0, TotalAbundance>=1~1, T~0),
  SI_BMI_10 = case_when(is.na(TotalAbundance)~0, TotalAbundance>=10~1, T~0),
  SI_EPT_PA = case_when(is.na(EPT_abundance)~0, EPT_abundance>=1~1, T~0),
  SI_EPT_10 = case_when(is.na(EPT_abundance)~0, EPT_abundance>=10~1, T~0),
  
  #This is for the AW, WM and GP models
  SI_BMI_Peren_PA = case_when(is.na(perennial_ISA_abundance)~0, perennial_ISA_abundance>=1~1, T~0),
  SI_BMI_Peren_10 = case_when(is.na(perennial_ISA_abundance)~0, perennial_ISA_abundance>=10~1, T~0),
  
  # #This is for the West model
  # SI_BMI_PerenInt_PA = case_when(is.na(perintper_ISA_abundance)~0, perintper_ISA_abundance>=1~1, T~0),
  # SI_BMI_PerenInt_10 = case_when(is.na(perintper_ISA_abundance)~0, perintper_ISA_abundance>=1~1, T~0),
  
  SI_Algae_10pct = case_when(is.na(AlgalCover_LiveOrDead_NoUpstream)~0,AlgalCover_LiveOrDead_NoUpstream>=3~1, T~0),
  SI_HydricSoil_PA = case_when(is.na(HydricSoils_score)~0,HydricSoils_score==3~1, T~0),
  SI_IOFB_PA = case_when(is.na(ironox_bfscore_NM)~0,ironox_bfscore_NM==1.5~1, T~0),
  
  SI_Hydrophytes_PA =case_when(is.na(hydrophytes_present)~0, hydrophytes_present>=1~1, T~0),
  
  SI_Vertebrates_PA = case_when(AmphSnake_PA==1~1, Turtles_yn=="present"~1, T~0)
)

SI_variables = c(
  # "SI_Fish_PA", #Drop: Not identified by PDT as a potential single indicator
  "SI_Fish_NoMosq_PA",
  "SI_BMI_PA",
  "SI_BMI_10", #TODO
  "SI_EPT_PA",
  "SI_EPT_10",
  "SI_BMI_Peren_PA",     #Only for AW, WM, GP models
  "SI_BMI_Peren_10",     #Only for AW, WM, GP models
  # "SI_BMI_PerenInt_PA",  #Only for West model
  # "SI_BMI_PerenInt_10",  #Only for West model
  "SI_Algae_10pct",
  "SI_HydricSoil_PA",
  "SI_IOFB_PA",
  "SI_Hydrophytes_PA",
  
  "SI_Vertebrates_PA"
)

si_summary <- df_si  %>% 
  mutate(
    sum_SI = rowSums(across(SI_variables)),
    Corrections=case_when(                                      #TODO: CHECK LTP
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & sum_SI>=1)~1, T~0),
    Misses=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & sum_SI==0)~1, T~0),
    Mistakes=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & sum_SI>=1)~1, T~0),
    
    Corrections_SI_Fish_NoMosq_PA=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_Fish_NoMosq_PA>=1)~1, T~0),
    Misses_SI_Fish_NoMosq_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_Fish_NoMosq_PA==0)~1, T~0),
    Mistakes_SI_Fish_NoMosq_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Fish_NoMosq_PA>=1)~1, T~0),
    
    Corrections_SI_BMI_PA=case_when(                                     
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_BMI_PA>=1)~1, T~0),
    Misses_SI_BMI_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_BMI_PA==0)~1, T~0),
    Mistakes_SI_BMI_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_PA>=1)~1, T~0),
    
    Corrections_SI_BMI_10=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_BMI_10>=1)~1, T~0),
    Misses_SI_BMI_10=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_BMI_10==0)~1, T~0),
    Mistakes_SI_BMI_10=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_10>=1)~1, T~0),
    
    Corrections_SI_EPT_PA=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_EPT_PA>=1)~1, T~0),
    Misses_SI_EPT_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_EPT_PA==0)~1, T~0),
    Mistakes_SI_EPT_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_EPT_PA>=1)~1, T~0),
    
    Corrections_SI_EPT_10=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_EPT_10>=1)~1, T~0),
    Misses_SI_EPT_10=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_EPT_10==0)~1, T~0),
    Mistakes_SI_EPT_10=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_EPT_10>=1)~1, T~0),
    
    Corrections_SI_BMI_Peren_PA=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_BMI_Peren_PA>=1)~1, T~0),
    Misses_SI_BMI_Peren_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_BMI_Peren_PA==0)~1, T~0),
    Mistakes_SI_BMI_Peren_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_Peren_PA>=1)~1, T~0),
    
    Corrections_SI_BMI_Peren_10=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_BMI_Peren_10>=1)~1, T~0),
    Misses_SI_BMI_Peren_10=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_BMI_Peren_10==0)~1, T~0),
    Mistakes_SI_BMI_Peren_10=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_Peren_10>=1)~1, T~0),

    Corrections_SI_Algae_10pct=case_when(                               
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_Algae_10pct>=1)~1, T~0),
    Misses_SI_Algae_10pct=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_Algae_10pct==0)~1, T~0),
    Mistakes_SI_Algae_10pct=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Algae_10pct>=1)~1, T~0),
    
    Corrections_SI_HydricSoil_PA=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_HydricSoil_PA>=1)~1, T~0),
    Misses_SI_HydricSoil_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_HydricSoil_PA==0)~1, T~0),
    Mistakes_SI_HydricSoil_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_HydricSoil_PA>=1)~1, T~0),
    
    Corrections_SI_IOFB_PA=case_when(                                 
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_IOFB_PA>=1)~1, T~0),
    Misses_SI_IOFB_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_IOFB_PA==0)~1, T~0),
    Mistakes_SI_IOFB_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_IOFB_PA>=1)~1, T~0),
    
    Corrections_SI_Hydrophytes_PA=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_Hydrophytes_PA>=1)~1, T~0),
    Misses_SI_Hydrophytes_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_Hydrophytes_PA==0)~1, T~0),
    Mistakes_SI_Hydrophytes_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Hydrophytes_PA>=1)~1, T~0),
    
    Corrections_SI_Vertebrates_PA=case_when(                              
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI") & SI_Vertebrates_PA>=1)~1, T~0),
    Misses_SI_Vertebrates_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI") & SI_Vertebrates_PA==0)~1, T~0),
    Mistakes_SI_Vertebrates_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Vertebrates_PA>=1)~1, T~0),
    
    # RF_Prediction_50_SI=case_when(
    #   (sum_SI>=1)~"ALI (SI)", T~RF_Prediction_50)
  )
si_summary <- si_summary %>% mutate(RF_Prediction_50_SI = ifelse(
  sum_SI>=1 , "ALI", RF_Prediction_50))



# Store for QC
si_summary_subset <- si_summary[, c("ParentGlobalID","SiteCode","Class","Dataset",
       "Notes","RF_Prediction_50", "RF_Prediction_50_SI",
       "Refinement", "sum_SI",
       SI_variables, 
       "Corrections_SI_Fish_NoMosq_PA",
       "Corrections_SI_BMI_PA",
       "Corrections_SI_BMI_10",
       "Corrections_SI_EPT_PA",
       "Corrections_SI_EPT_10",
       "Corrections_SI_BMI_Peren_PA",
       "Corrections_SI_BMI_Peren_10",
       "Corrections_SI_Algae_10pct",
       "Corrections_SI_HydricSoil_PA",
       "Corrections_SI_IOFB_PA",
       "Corrections_SI_Hydrophytes_PA",
       "Corrections_SI_Vertebrates_PA",
       
       "Misses_SI_Fish_NoMosq_PA",
       "Misses_SI_BMI_PA",
       "Misses_SI_BMI_10",
       "Misses_SI_EPT_PA",
       "Misses_SI_EPT_10",
       "Misses_SI_BMI_Peren_PA",
       "Misses_SI_BMI_Peren_10",
       "Misses_SI_Algae_10pct",
       "Misses_SI_HydricSoil_PA",
       "Misses_SI_IOFB_PA",
       "Misses_SI_Hydrophytes_PA",
       "Misses_SI_Vertebrates_PA",
       
       "Mistakes_SI_Fish_NoMosq_PA",
       "Mistakes_SI_BMI_PA",
       "Mistakes_SI_BMI_10",
       "Mistakes_SI_EPT_PA",
       "Mistakes_SI_EPT_10",
       "Mistakes_SI_BMI_Peren_PA",
       "Mistakes_SI_BMI_Peren_10",
       "Mistakes_SI_Algae_10pct",
       "Mistakes_SI_HydricSoil_PA",
       "Mistakes_SI_IOFB_PA",
       "Mistakes_SI_Hydrophytes_PA",
       "Mistakes_SI_Vertebrates_PA"
      )]

si_summary_subset <- si_summary_subset %>% filter(Notes != "Augmented") %>%
  mutate(BigError=case_when(                                     
            (Class == "E" & RF_Prediction_50=="P")~1, 
            (Class == "P" & RF_Prediction_50=="E")~1,
            T~0)
         )
    
write.csv(si_summary_subset, paste0(out_dir, "/si_summary_subset.csv"))



# 
# df_longer <- si_summary_subset %>%
#   pivot_longer(cols = starts_with("Misses_"),
#                names_to = "Missed",
#                names_prefix = "Misses_",
#                values_to = "count") %>%
#   unique() %>%
#   pivot_longer(cols = starts_with("Mistakes_"),
#                names_to = "Mistake",
#                names_prefix = "Mistakes_",
#                values_to = "count",
#                names_repair = "unique") %>%
#   unique()
# 
# df_longer




big_errors <- si_summary_subset %>% 
                  filter(Notes != "Augmented", BigError == 1)# %>%
                 # select("ParentGlobalID","SiteCode","Class","Dataset",
                   #      "Notes","RF_Prediction_50","Refinement", "BigError")
write.csv(big_errors, paste0(out_dir, "/big_errors.csv"))


si_summary2 <- si_summary %>% group_by(Refinement) %>% 
  summarise(
      SI_nomosqfish_corrections = sum(Corrections_SI_Fish_NoMosq_PA),
      SI_nomosqfish_misses = sum(Misses_SI_Fish_NoMosq_PA),
      SI_nomosqfish_mistakes = sum(Mistakes_SI_Fish_NoMosq_PA),
      
      SI_bmipa_corrections = sum(Corrections_SI_BMI_PA),
      SI_bmipa_misses = sum(Misses_SI_BMI_PA),
      SI_bmipa_mistakes = sum(Mistakes_SI_BMI_PA),
      
      SI_bmi10_corrections = sum(Corrections_SI_BMI_10),
      SI_bmi10_misses = sum(Misses_SI_BMI_10),
      SI_bmi10_mistakes = sum(Mistakes_SI_BMI_10),
      
      SI_eptpa_corrections = sum(Corrections_SI_EPT_PA),
      SI_eptpa_misses = sum(Misses_SI_EPT_PA),
      SI_eptpa_mistakes = sum(Mistakes_SI_EPT_PA),
      
      SI_ept10_corrections = sum(Corrections_SI_EPT_10),
      SI_ept10_misses = sum(Misses_SI_EPT_10),
      SI_ept10_mistakes = sum(Mistakes_SI_EPT_10),
      
      SI_bmiperenpa_corrections = sum(Corrections_SI_BMI_Peren_PA),
      SI_bmiperenpa_misses = sum(Misses_SI_BMI_Peren_PA),
      SI_bmiperenpa_mistakes = sum(Mistakes_SI_BMI_Peren_PA),
      
      SI_bmiperen10_corrections = sum(Corrections_SI_BMI_Peren_10),
      SI_bmiperen10_misses = sum(Misses_SI_BMI_Peren_10),
      SI_bmiperen10_mistakes = sum(Mistakes_SI_BMI_Peren_10),
      
      SI_algae10_corrections = sum(Corrections_SI_Algae_10pct),
      SI_algae10_misses = sum(Misses_SI_Algae_10pct),
      SI_algae10_mistakes = sum(Mistakes_SI_Algae_10pct),
      
      SI_hydricsoils_corrections = sum(Corrections_SI_HydricSoil_PA),
      SI_hydricsoils_misses = sum(Misses_SI_HydricSoil_PA),
      SI_hydricsoils_mistakes = sum(Mistakes_SI_HydricSoil_PA),
      
      SI_iofb_corrections = sum(Corrections_SI_IOFB_PA),
      SI_iofb_misses = sum(Misses_SI_IOFB_PA),
      SI_iofb_mistakes = sum(Mistakes_SI_IOFB_PA),
      
      SI_hydrophytes_corrections = sum(Corrections_SI_Hydrophytes_PA),
      SI_hydrophytes_misses = sum(Misses_SI_Hydrophytes_PA),
      SI_hydrophytes_mistakes = sum(Mistakes_SI_Hydrophytes_PA),
      
      SI_vertebrates_corrections = sum(Corrections_SI_Vertebrates_PA),
      SI_vertebrates_misses = sum(Misses_SI_Vertebrates_PA),
      SI_vertebrates_mistakes = sum(Mistakes_SI_Vertebrates_PA)
  )

si_summary_pivoted <- si_summary2 %>% 
  pivot_longer(cols=starts_with("SI_"), 
               names_prefix ="SI_", 
               names_to = "Change", 
               values_to = "count")

si_summary_pivoted <- si_summary_pivoted %>% mutate(Status = case_when(
  (str_detect(Change, "corrections") ~ "Correction"),
  (str_detect(Change, "mistakes") ~ "Mistake"), 
  (str_detect(Change, "misses") ~ "Missed"),
  TRUE ~ "error") 
  ) %>% mutate(Label = case_when(
  (str_detect(Change, "nomosqfish") ~ "Fish NoMosq (PA)"),
  (str_detect(Change, "bmipa") ~ "BMI (PA)"), 
  (str_detect(Change, "bmi10") ~ "BMI (10)"), 
  (str_detect(Change, "eptpa") ~ "EPT (PA)"),
  (str_detect(Change, "ept10") ~ "EPT (10)"),
  (str_detect(Change, "bmiperenpa") ~ "BMI Peren (PA)"), 
  (str_detect(Change, "bmiperen10") ~ "BMI Peren (10)"), 
  (str_detect(Change, "algae10") ~ "Algae 10"),
  (str_detect(Change, "hydricsoils") ~ "Hydric Soils"),
  (str_detect(Change, "iofb") ~ "IOFB"), 
  (str_detect(Change, "hydrophytes") ~ "Hydrophytes"),
  (str_detect(Change, "vertebrates") ~ "Vertebrates"),
  TRUE ~ "error")
  )
# Store for QC
write.csv(si_summary_pivoted, paste0(out_dir, "/si_summary_pivoted.csv"))


ref_versions <-  si_summary_pivoted %>% select(Refinement) %>% unique()
ref_versions <- ref_versions[, "Refinement"]

si_plot2 <- ggplot(data=si_summary_pivoted %>% group_by(Refinement, Label, Status) , 
                   aes(x=count, y=Label))+
  geom_point(aes(color=Status, group=Label, alpha=0.5 ))+
  scale_color_manual(
    values=c(
      "Correction"="#04912a",
      "Mistake"="red",
      "Missed"="blue")) +
  facet_wrap(~Refinement)+
  xlab("Number of samples changed")+
  ylab("")+
  labs(title = "Single Indicators")
si_plot2
ggsave(si_plot2,
       filename=paste0(out_dir,"/si_plot2.png"),
       width=9, height=6)


si_plot3 <- ggplot(data=si_summary_pivoted %>% 
                     group_by(Refinement, Label, Status) , 
                   aes(x=count, y=Refinement))+
  geom_point(aes(color=Status, group=Label, alpha=0.5 ),
  position = position_dodge(width=.2))+
  scale_color_manual(
    values=c(
      "Correction"="#04912a",
      "Mistake"="red",
      "Missed"="blue")
  ) +
  facet_wrap(~Label)+
  xlab("Number of samples changed")+
  ylab("")+
  labs(title = "Single Indicators", 
       subtitle = "Training and Testing, non-augmented")
ggsave(si_plot3,
       filename=paste0(out_dir,"/si_plot3.png"),
       width=9, height=6)



si_plot3b <- ggplot(data=si_summary_pivoted %>% 
                      filter(Status %in% 
                               c("Correction","Mistake")) %>% 
                      group_by(Refinement, Label, Status) , 
                   aes(x=count, y=Refinement))+
  geom_point(aes(color=Status, group=Label #
, alpha=0.5
  ), position = position_dodge(width=.2)
  )+
  scale_color_manual(
    values=c(
      "Correction"="#04912a",
      "Mistake"="red")
  ) +
  facet_wrap(~Label)+
  xlab("Number of samples changed")+
  ylab("")+
  labs(title = "Single Indicators", 
       subtitle = "Training and Testing, non-augmented")
ggsave(si_plot3b,
       filename=paste0(out_dir,"/si_plot3b.png"),
       width=9, height=6)




# si_plot3c <- ggplot(data=si_summary_pivoted %>% 
#                       # filter(Refinement %in% 
#                       #          c("V0_NoGIS_Unstrat", "V4_NoGIS_Unstrat")) %>%
#                       filter(Label %in%
#                                c("Fish NoMosq (PA)","BMI Peren","EPT (10)")) %>%
#                       filter(Status %in% 
#                                c("Correction","Mistake")) %>% 
#                       group_by(Refinement, Label, Status) , 
#                     aes(x=count, y=Refinement))+
#   geom_point(aes(color=Status, group=Label #, alpha=0.5
#   ), position = position_dodge(width=.2), size=2.3
#   )+
#   scale_color_manual(
#     values=c(
#       "Correction"="#04912a",
#       "Mistake"="red"#,
#       # "Missed"="blue"
#     )
#   ) +
#   facet_wrap(~Label)+
#   xlab("Number of samples changed")+
#   ylab("")+
#   labs(title = "Single Indicators", 
#        subtitle = "Training and Testing, non-augmented")
# 
# ggsave(si_plot3c,
#        filename=paste0(out_dir,"/si_plot3c.png"),
#        width=8.5, height=3.5)
# 



## Only models of interest
si_plot4 <- ggplot(data=si_summary_pivoted %>% 
                  filter(Refinement %in% 
                    c("V0_NoGIS_Unstrat", "V4_NoGIS_Unstrat")) %>%
                  filter(Label %in% 
                    c("Fish NoMosq (PA)")) %>% 
                  group_by(Refinement, Label, Status) , 
                  aes(x=count, y=Refinement))+
  geom_point(aes(color=Status, group=Label#, alpha=0.5
                 ),
            position = position_dodge(width=.2),
            size=2.3
  )+ scale_color_manual(
    values=c(
      "Correction"="#04912a",
      "Mistake"="red",
      "Missed"="blue")
  ) + facet_wrap(~Label) +
  xlab("Number of samples changed")+
  ylab("")+
  labs(title = "Single Indicators", 
       subtitle = "Training and Testing, non-augmented")
si_plot4

ggsave(si_plot4,
       filename=paste0(out_dir,"/si_plot4.png"),
       width=5.5, height=3)

si_plot5 <- ggplot(data=si_summary_pivoted %>% 
                     filter(Refinement %in% 
                              c("V0_NoGIS_Unstrat", "V4_NoGIS_Unstrat")) %>%
                     filter(Label %in% 
                              c("Fish NoMosq (PA)")) %>% 
                     filter(Status %in% 
                              c("Correction","Mistake")) %>% 
                     group_by(Refinement, Label, Status) , 
                   aes(x=count, y=Refinement))+
  geom_point(aes(color=Status, group=Label#, alpha=0.5
  ),
  position = position_dodge(width=.2),
  size=4
  )+ scale_color_manual(
    values=c(
      "Correction"="#04912a",
      "Mistake"="red",
      "Missed"="blue")
  ) + facet_wrap(~Label) +
  xlab("Number of samples changed")+
  ylab("")+
  labs(title = "Single Indicators", 
       subtitle = "Training and Testing, non-augmented")
si_plot5

ggsave(si_plot5,
       filename=paste0(out_dir,"/si_plot5.png"),
       width=6, height=4)


##Plot each version separately
ref_versions <-  si_summary_pivoted %>% select(Refinement) %>% unique()

for (v in ref_versions$Refinement) {
  print(v)
  si_plot_v <- ggplot(data=si_summary_pivoted %>% filter(Refinement == v) %>%
                        # filter(Status %in% c("Correction","Mistake")) %>%
                        group_by(Refinement, Label, Status) ,
                      aes(x=count, y=Label))+
    geom_point(aes(color=Status, group=Label, alpha=0.5
    ),
    position = position_dodge(width=.2)
    )+
    scale_color_manual(
      values=c(
        "Correction"="#04912a",
        "Mistake"="red",
        "Missed"="blue")
    ) +
    # facet_wrap(~Label)+
    xlab("Number of samples changed")+
    ylab("")+
    labs(title = paste(v, "- Single Indicator performance"),
         subtitle = "Training and testing, non-augmented")
  si_plot_v
  
  ggsave(si_plot_v,
         filename=paste0(out_dir,"/si_plot_",v,".png"),
         width=6, height=4.8)
  
  
  ##output SI summary
  v_csv <- si_summary %>% filter(Refinement == v) 
  write.csv(v_csv, paste0(out_dir, "/si_summary_",v,".csv"))
  
  
}



# ############################## ONLY LOOK AT TESTING
# si_summary_test <- si_summary %>% group_by(Refinement) %>%
#   filter(Dataset =="Testing") %>%
#   # tally()
#   summarise(
#     SI_nomosqfish_corrections = sum(Corrections_SI_Fish_NoMosq_PA),
#     SI_nomosqfish_misses = sum(Misses_SI_Fish_NoMosq_PA),
#     SI_nomosqfish_mistakes = sum(Mistakes_SI_Fish_NoMosq_PA),
# 
#     SI_bmipa_corrections = sum(Corrections_SI_BMI_PA),
#     SI_bmipa_misses = sum(Misses_SI_BMI_PA),
#     SI_bmipa_mistakes = sum(Mistakes_SI_BMI_PA),
# 
#     SI_eptpa_corrections = sum(Corrections_SI_EPT_PA),
#     SI_eptpa_misses = sum(Misses_SI_EPT_PA),
#     SI_eptpa_mistakes = sum(Mistakes_SI_EPT_PA),
# 
#     SI_ept10_corrections = sum(Corrections_SI_EPT_10),
#     SI_ept10_misses = sum(Misses_SI_EPT_10),
#     SI_ept10_mistakes = sum(Mistakes_SI_EPT_10),
# 
#     SI_bmiperen_corrections = sum(Corrections_SI_BMI_Peren_PA),
#     SI_bmiperen_misses = sum(Misses_SI_BMI_Peren_PA),
#     SI_bmiperen_mistakes = sum(Mistakes_SI_BMI_Peren_PA),
# 
#     SI_algae10_corrections = sum(Corrections_SI_Algae_10pct),
#     SI_algae10_misses = sum(Misses_SI_Algae_10pct),
#     SI_algae10_mistakes = sum(Mistakes_SI_Algae_10pct),
# 
#     SI_hydricsoils_corrections = sum(Corrections_SI_HydricSoil_PA),
#     SI_hydricsoils_misses = sum(Misses_SI_HydricSoil_PA),
#     SI_hydricsoils_mistakes = sum(Mistakes_SI_HydricSoil_PA),
# 
#     SI_iofb_corrections = sum(Corrections_SI_IOFB_PA),
#     SI_iofb_misses = sum(Misses_SI_IOFB_PA),
#     SI_iofb_mistakes = sum(Mistakes_SI_IOFB_PA),
# 
#     SI_hydrophytes_corrections = sum(Corrections_SI_Hydrophytes_PA),
#     SI_hydrophytes_misses = sum(Misses_SI_Hydrophytes_PA),
#     SI_hydrophytes_mistakes = sum(Mistakes_SI_Hydrophytes_PA)
#   )
# 
# si_summary_test_pivoted <- si_summary_test %>%
#   pivot_longer(cols=starts_with("SI_"),
#                names_prefix ="SI_",
#                names_to = "Change",
#                values_to = "count")
# 
# si_summary_test_pivoted <- si_summary_test_pivoted %>% mutate(Status = case_when(
#   (str_detect(Change, "corrections") ~ "Correction"),
#   (str_detect(Change, "mistakes") ~ "Mistake"),
#   (str_detect(Change, "misses") ~ "Missed"),
#   TRUE ~ "error")
# ) %>% mutate(Label = case_when(
#   (str_detect(Change, "fish") ~ "Fish NoMosq (PA)"),
#   (str_detect(Change, "bmipa") ~ "BMI (PA)"),
#   (str_detect(Change, "eptpa") ~ "EPT (PA)"),
#   (str_detect(Change, "ept10") ~ "EPT (10)"),
#   (str_detect(Change, "bmiperen") ~ "BMI Peren"),
#   (str_detect(Change, "algae10") ~ "Algae 10"),
#   (str_detect(Change, "hydricsoils") ~ "Hydric Soils"),
#   (str_detect(Change, "iofb") ~ "IOFB"),
#   (str_detect(Change, "hydrophytes") ~ "Hydrophytes"),
#   TRUE ~ "error")
# )
# # Store for QC
# # write.csv(si_summary_test_pivoted, paste0(out_dir, "/si_summary_test_pivoted.csv"))
# 
# si_plot6 <- ggplot(data=si_summary_test_pivoted %>%
#                      filter(Status %in% c("Correction","Mistake")) %>%
#                        group_by(Refinement, Label, Status) ,
#                      aes(x=count, y=Refinement))+
#   geom_point(aes(color=Status, group=Label, alpha=0.5
#   ),
#   position = position_dodge(width=.2)
#   )+
#   scale_color_manual(
#     values=c(
#       "Correction"="#04912a",
#       "Mistake"="red",
#       "Missed"="blue")
#   ) +
#   facet_wrap(~Label)+
#   xlab("Number of samples changed")+
#   ylab("")+
#   labs(title = "Single Indicators",
#        subtitle = "Testing, non-augmented")
# 
# si_plot6
# 
# ggsave(si_plot6,
#        filename=paste0(out_dir,"/si_plot6.png"),
#        width=9, height=6)
# 
# 
