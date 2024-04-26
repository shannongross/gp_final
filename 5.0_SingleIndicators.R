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
library(reshape2)

# model_version <- "DraftFinalModels2"
model_version <- "FinalModelQC_Apr2024"
chosen_model <- "NoGIS_Unstrat"
refinement_chosen <- "V5_NoGIS_Unstrat"

########################## Get Data ############################################
# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Create output dirs if they do not exist
input_clean_dir <- paste0(HOME_DIR, "/input/processed")
if (!dir.exists(input_clean_dir)){dir.create(input_clean_dir)}

parent_path <- paste0(HOME_DIR, "/output/models/", model_version, "/",
                      chosen_model)

out_dir <- paste0(parent_path, "/SI")
if (!dir.exists(out_dir)){dir.create(out_dir)}

# chosen_ref_dir <- paste0(parent_path,"/", chosen_version, "/final_w_SI")
# if (!dir.exists(chosen_ref_dir)){dir.create(chosen_ref_dir)}


refined_files <- list.files(parent_path, full.names = T, all.files = TRUE,
                      # recursive = T, pattern = "refined_results_summary.csv")
                      recursive = T, pattern = "full_results.csv")
df_results <- do.call(rbind, lapply(refined_files, read.csv))
write.csv(df_results, paste0(out_dir, "/all_refinement_results.csv"))


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
  "SI_BMI_10", 
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

    Corrections_SI_Fish_NoMosq_PA=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_Fish_NoMosq_PA>=1)~1, T~0),
    Corrections_SI_BMI_PA=case_when(                                     
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_BMI_PA>=1)~1, T~0),
    Corrections_SI_BMI_10=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_BMI_10>=1)~1, T~0),
    Corrections_SI_EPT_PA=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_EPT_PA>=1)~1, T~0),
    Corrections_SI_EPT_10=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_EPT_10>=1)~1, T~0),
    Corrections_SI_BMI_Peren_PA=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_BMI_Peren_PA>=1)~1, T~0),
    Corrections_SI_BMI_Peren_10=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_BMI_Peren_10>=1)~1, T~0),
    Corrections_SI_Algae_10pct=case_when(                               
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_Algae_10pct>=1)~1, T~0),
    Corrections_SI_HydricSoil_PA=case_when(                                    
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_HydricSoil_PA>=1)~1, T~0),
    Corrections_SI_IOFB_PA=case_when(                                 
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_IOFB_PA>=1)~1, T~0),
    Corrections_SI_Hydrophytes_PA=case_when(                                   
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_Hydrophytes_PA>=1)~1, T~0),
    Corrections_SI_Vertebrates_PA=case_when(                              
      (Class %in% c("I","P") & RF_Prediction_50  %in% c("E","NMI","LTP") & SI_Vertebrates_PA>=1)~1, T~0),
    
    Misses_SI_Fish_NoMosq_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_Fish_NoMosq_PA==0)~1, T~0),
    Misses_SI_BMI_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_BMI_PA==0)~1, T~0),
    Misses_SI_BMI_10=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_BMI_10==0)~1, T~0),
    Misses_SI_EPT_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_EPT_PA==0)~1, T~0),
    Misses_SI_EPT_10=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_EPT_10==0)~1, T~0),
    Misses_SI_BMI_Peren_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_BMI_Peren_PA==0)~1, T~0),
    Misses_SI_BMI_Peren_10=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_BMI_Peren_10==0)~1, T~0),
    Misses_SI_Algae_10pct=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_Algae_10pct==0)~1, T~0),
    Misses_SI_HydricSoil_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_HydricSoil_PA==0)~1, T~0), 
    Misses_SI_IOFB_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_IOFB_PA==0)~1, T~0), 
    Misses_SI_Hydrophytes_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_Hydrophytes_PA==0)~1, T~0),
    Misses_SI_Vertebrates_PA=case_when(
      (Class %in% c("I","P") & RF_Prediction_50 %in% c("E","NMI","LTP") & SI_Vertebrates_PA==0)~1, T~0),
    
    Mistakes_SI_Fish_NoMosq_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Fish_NoMosq_PA>=1)~1, T~0),
    Mistakes_SI_BMI_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_PA>=1)~1, T~0),
    Mistakes_SI_BMI_10=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_10>=1)~1, T~0),
    Mistakes_SI_EPT_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_EPT_PA>=1)~1, T~0),
    Mistakes_SI_EPT_10=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_EPT_10>=1)~1, T~0),
    Mistakes_SI_BMI_Peren_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_Peren_PA>=1)~1, T~0),
    Mistakes_SI_BMI_Peren_10=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_BMI_Peren_10>=1)~1, T~0),
    Mistakes_SI_Algae_10pct=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Algae_10pct>=1)~1, T~0),
    Mistakes_SI_HydricSoil_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_HydricSoil_PA>=1)~1, T~0),
    Mistakes_SI_IOFB_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_IOFB_PA>=1)~1, T~0),
    Mistakes_SI_Hydrophytes_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Hydrophytes_PA>=1)~1, T~0),
    Mistakes_SI_Vertebrates_PA=case_when(
      (Class %in% c("E") & RF_Prediction_50  %in% c("E") & SI_Vertebrates_PA>=1)~1, T~0)

  )

si_summary_subset <- si_summary %>% filter(Notes != "Augmented") %>%
  mutate(BigError=case_when(                                     
            (Class == "E" & RF_Prediction_50=="P")~1, 
            (Class == "P" & RF_Prediction_50=="E")~1,
            T~0)
         )
write.csv(si_summary_subset, paste0(out_dir, "/si_summary_subset.csv"))


# FINAL MODEL PERFORMANCE STRATA
si_summary_full <- si_summary_subset %>% 
  mutate(P_or_I = case_when(Class %in% c("P", "I") ~ TRUE, T~F),
         I_or_E = case_when(Class %in% c("I", "E") ~ TRUE, T~F),
         P_or_I_wet = case_when(P_or_I & Wet ~ TRUE, T~F),
         I_or_E_dry = case_when(I_or_E & !Wet ~ TRUE, T~F)) %>%
  group_by(Refinement, Dataset, Region_detail) %>%
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

write.csv(si_summary_full, paste0(out_dir, "/si_summary_full.csv"))


############################################################
# Plot of original base model vs chosen refinement
si_0andRefined <-  si_summary_full %>% filter(Refinement %in% 
                      c("V0_NoGIS_Unstrat",refinement_chosen)) %>% 
          group_by(Refinement)
  df_dots_0andRefined  <- si_0andRefined %>%
    select(-n_correct_PvIvE, -n_correct_EvALI) %>%
    pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
                 names_to="Measure") %>% 
    mutate(Measure=str_sub(Measure, start=13)) 
  
  
  df_dots_0andRefined$Measure <- factor(df_dots_0andRefined$Measure,
                            levels=c("PvIvE","EvALI","PvIwet","IvEdry" ))
  
  df_dots_0andRefined$Dataset <- factor(df_dots_0andRefined$Dataset, 
                            levels=c("Testing","Training"))
  label_names <- c(
    `PvIvE` = "Accuracy\nPvIvE",
    `EvALI` = "Accuracy\nEvALI",
    `PvIwet` = "Accuracy\nPvIwet",
    `IvEdry` = "Accuracy\nIvEdry"
  )
  dotplot_0andRefined <- ggplot(data=df_dots_0andRefined, 
                     aes(x=Region_detail, y=Accuracy))+
    geom_jitter(aes(size=Dataset, color=Region_detail, shape=Refinement), 
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
    scale_shape_manual(values=c(1, 16)) +
    facet_wrap(~Measure, nrow = 1, scales="free_x",
               labeller=as_labeller(label_names))+
    coord_flip()+
    xlab("")+
    labs(title=paste0("Summary of Original Unstrat_NoGIS vs ", refinement_chosen, " by Strata"))+
    scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
    theme_bw()
  dotplot_0andRefined
  ggsave(dotplot_0andRefined, height=4, width = 9, units="in", dpi=900,
         filename=paste0(out_dir, "/dotplot_0andRefined_fixed.png"))


##########################################################################
ref_versions <-  si_summary_full %>% select(Refinement) %>% unique()

for (chosen_version in ref_versions$Refinement) {
    print(chosen_version)
    parent_path <- paste0(HOME_DIR, "/output/models/", model_version,
                  "/", chosen_model,"/", chosen_version)
    
    out_dir <- paste0(parent_path, "/final_w_SI")
    if (!dir.exists(out_dir)){dir.create(out_dir)}
    
    
    sub <- si_summary_full %>% filter(Refinement==chosen_version)
    current <- si_summary_subset %>% filter(Refinement==chosen_version)
    write.csv(sub, 
              paste0(out_dir, "/", chosen_version,
                     "_refined_si_summary.csv"))
    write.csv(current, 
              paste0(out_dir, "/", chosen_version,
                     "_refined_si_full.csv"))
    
    
    df_dots <- sub %>%
      select(-n_correct_PvIvE, -n_correct_EvALI) %>%
      pivot_longer(cols=starts_with("pct"), values_to="Accuracy", 
                   names_to="Measure") %>% 
      mutate(Measure=str_sub(Measure, start=13)) 
    
    
    df_dots$Measure <- factor(df_dots$Measure,
                              levels=c("PvIvE","EvALI","PvIwet","IvEdry" ))
    
    df_dots$Dataset <- factor(df_dots$Dataset, 
                              levels=c("Testing","Training"))
    label_names <- c(
      `PvIvE` = "Accuracy\nPvIvE",
      `EvALI` = "Accuracy\nEvALI",
      `PvIwet` = "Accuracy\nPvIwet",
      `IvEdry` = "Accuracy\nIvEdry"
    )
    dotplot1 <- ggplot(data=df_dots, 
                       aes(x=Region_detail, y=Accuracy))+
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
      labs(title=paste0("Summary of Refined Model (",refinement_chosen,") by Strata"))+
      # scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
      theme_bw()
    dotplot1
    ggsave(dotplot1, height=4, width = 9, units="in", dpi=900,
         filename=paste0(out_dir, "/dotplot1.png"))
    
    
    ## FINAL CONFUSION MATRIX - before
    results_conf_mat <- current %>% group_by(Class, Dataset)
    results_conf_mat$Class <- as.factor(results_conf_mat$Class)
    results_conf_mat$Dataset <- as.factor(results_conf_mat$Dataset)
    results_conf_mat$RF_Prediction_50 <- as.factor(results_conf_mat$RF_Prediction_50)
    results_conf_mat2 <- results_conf_mat %>%
      group_by( Class, Dataset, RF_Prediction_50)%>%
      summarise(n=length(SiteCode)) %>%
      rename(ActualClass="Class")
    results_conf_mat3 <- spread(results_conf_mat2, key = RF_Prediction_50, value = n)
    results_conf_mat3 <- results_conf_mat3 %>% arrange(Dataset)
    melted <- melt(results_conf_mat3)
    cm_pivot_table <- melted %>%
      pivot_wider(
        names_from = c(ActualClass, Dataset),
        values_from = value
      ) %>% arrange(factor(variable, levels = c('E', 'I', 'ALI', 'P', 'LTP', 'NMI')))
    ## FINAL CM -after
    this_SI <- "SI_Hydrophytes_PA"
    results_conf_mat_after <- results_conf_mat %>% mutate(
      RF_Prediction_50_SI_Hydrophytes_PA=case_when(
          (SI_Hydrophytes_PA>=1)~"ALI (SI)", T~RF_Prediction_50))
    
    results_conf_mat_after$RF_Prediction_50_SI_Hydrophytes_PA <- as.factor(
        results_conf_mat_after$RF_Prediction_50_SI_Hydrophytes_PA)
    results_conf_mat_after2 <- results_conf_mat_after %>%
      group_by( Class, Dataset, RF_Prediction_50_SI_Hydrophytes_PA)%>%
      summarise(n=length(SiteCode)) %>%
      rename(ActualClass="Class")
    results_conf_mat_after3 <- spread(results_conf_mat_after2, key = RF_Prediction_50_SI_Hydrophytes_PA, value = n)
    results_conf_mat_after3 <- results_conf_mat_after3 %>% arrange(Dataset)
    melted_after <- melt(results_conf_mat_after3)
    cm_pivot_table_after <- melted_after %>%
      pivot_wider(
        names_from = c(ActualClass, Dataset),
        values_from = value
      ) %>% arrange(factor(variable, levels = c('E', 'I', 'ALI','ALI (SI)', 'P', 'LTP', 'NMI')))
    cm_out_dir <- paste0(out_dir, "/CM_SI_detail")
    if (!dir.exists(cm_out_dir)){dir.create(cm_out_dir)}
    write.csv(cm_pivot_table, 
              paste0(cm_out_dir, "/CM_",chosen_model,
                     "_noSI.csv"))
    write.csv(cm_pivot_table_after, 
              paste0(cm_out_dir, "/CM_",chosen_model,
                     "_",this_SI,".csv"))
}
