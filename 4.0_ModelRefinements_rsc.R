################################################################################
# This script shows a subset of manual refinements for the Great Plains
# Unstrat No GIS model that were presented to the Regional Steering Committee
#
################################################################################
library(ggrepel)
library(tidyverse)
library(sf)
library(skimr)
library(vegan)
library(ggpubr)
library(ggforce)
library(randomForest)
library(tidymodels)
library(caret)
library(ggplot2)
library(gridExtra)
library(rstudioapi)
library(RColorBrewer)
library(reshape2)

graphics.off()

info_list <- c("ParentGlobalID","CollectionDate","Region",
                "Region_detail","SiteCode","Class","Dataset", "Wet")

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

# Explicitly set datatypes
df_input$Class <- as.factor(df_input$Class)
df_input$Region_detail <- as.factor(df_input$Region_detail)

# Create Strata column
df_input <- df_input %>% mutate( Strata = Region_detail)



################################## ADDITIONAL COLUMNS ##########################
# CREATE BINS/MODIFY EXISTING
################################################################################
df_input <- df_input %>% mutate(
    BankWidth_10 = case_when(BankWidthMean<10~0, T~1), #BankWidthMean: less than 10, gte 10
    TotalAbund_8 = case_when(TotalAbundance<8~0, T~1), #TotalAbundance: less than 8, gte 8
    TotalAbund_8_24 = case_when(TotalAbundance<8~0,  #TotalAbundance: less than 8, 8 to 23, 24+
        ((TotalAbundance>=8) & (TotalAbundance<24)~1),
          TotalAbundance>=24~2), #performed same as TotalAbund_8
    TotalAbund_0_8_24 = case_when(TotalAbundance==0~0, #TotalAbundance: 0, 1-8, 8-23, 24+
                                  ((TotalAbundance>0) & (TotalAbundance<8)~1),
                                ((TotalAbundance>=8) & (TotalAbundance<24)~2),
                                TotalAbundance>=24~3),
    TotalAbund_0_10 = case_when(TotalAbundance==0~0, #TotalAbundance: 0, 1-9, 10+
                                  ((TotalAbundance>0) & (TotalAbundance<10)~1),
                                  TotalAbundance>=10~2),
    UplandRooted_PA = case_when(UplandRootedPlants_score<3~0, T~1),
    ephISAabund_PA = case_when(ephinteph_ISA_abundance==0~0, T~1),
    ephISAabund_0_2 = case_when(ephinteph_ISA_abundance==0~0, 
                ephinteph_ISA_abundance==1~1,
                ephinteph_ISA_abundance>=2~2),
    hydrophytes_2 = case_when(hydrophytes_present<2~0, T~1),
    hydrophytes_0_2 = case_when(hydrophytes_present==0~0,
                              (hydrophytes_present==1~1),
                              hydrophytes_present>=2~2),
    
    # #INCORRECT - CHANGES REFINEMENT STATS
    # hydrophytes_2 = case_when(hydrophytes_present<3~0, T~1),
    # hydrophytes_0_2 = case_when(hydrophytes_present==0~0, 
    #                             ((hydrophytes_present>0) & (hydrophytes_present<=3)~1),
    #                             hydrophytes_present>=3~2),
    # #
    
    DiffInVeg_1.5 = case_when(DifferencesInVegetation_score<3~0, T~1)
  )  


# tmp <- df_input[,c("SiteCode", "ephinteph_ISA_abundance","ephISAabund_PA")]
# tmp %>% filter(TotalAbundance==8)
  
new_preds_list <- c("BankWidth_10",
                    "TotalAbund_8",
                    "TotalAbund_8_24",
                    "TotalAbund_0_8_24",
                    "TotalAbund_0_10",
                    "UplandRooted_PA",
                    "ephISAabund_PA", 
                    "ephISAabund_0_2",
                    "hydrophytes_2", 
                    "hydrophytes_0_2",
                    "DiffInVeg_1.5")

#plotting function
ggsummaryPlot <- function(df){
  start<-0
  text.size <- 2.5
  ggplot(df %>%
           select(Step, n_varz, 
                  pctCorrectPvIvE, pctCorrect_EvALI, pctCorrect_EvIdry
                  ) %>%
           pivot_longer(cols=c(
                pctCorrectPvIvE, 
                pctCorrect_EvALI,
                pctCorrect_EvIdry)), 
         aes(x=Step-start, y=value, label=round(value,2)))+  
    geom_hline(data = . %>% filter(Step==start), 
               aes(color=name, yintercept=value), 
               linetype="dashed")+
    geom_path(aes(color=name))+
    geom_point(aes(color=name), size=2)+
    geom_text(hjust=0.02, vjust=0.02, size= text.size)+ 
    geom_label_repel(data=df %>%
          select(Step, Description, pctCorrectPvIvE) %>%
          pivot_longer(cols=c(pctCorrectPvIvE)),
          aes(x = df$Step-start,
          y = df$pctCorrectPvIvE), 
          label = df$Description,
          nudge_y = -(df$pctCorrectPvIvE/2),
          size=1.8
    ) +
    geom_point(data=. %>%
                 group_by(name) %>%
                 slice_max(value, n=1),
               color="black", size=2, stroke=1.3, 
               shape=21)+
    scale_color_brewer(palette="Set1")+
    coord_cartesian(ylim=c(0,1))+
    scale_x_continuous(breaks=0:nrow(df), labels=0:nrow(df)+start) + 
    labs(title=paste("Refinement of",chosen_model,"Base Model"), 
            subtitle = "Performance on Test Dataset",
            fill="smthg") + xlab("") + ylab("") +
    theme(legend.position="bottom")+ 
    scale_color_discrete(name="")+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

#######################################################################
# Stepwise refinement of GP model
#######################################################################
# model_version <- "DraftFinalModelsQC" 
model_version <- "FinalModelQC_Apr2024"
chosen_model <- "NoGIS_Unstrat"
plotwidth <- 10.5
numTrees <- 1500

HOME_DIR <- getwd()
OUT_DIR = paste0(HOME_DIR, "/output/models")
BASE_DIR <- paste0(OUT_DIR, "/", model_version,"/", chosen_model)

# Train\test
df_MODEL <- df_input %>% filter(Dataset=="Training")
df_TESTING <- df_input %>% filter(Dataset=="Testing")

################################################################
#     V0 --> Restore Original model
################################################################
thisstep <- 0
refinement_version <- paste0("V",thisstep,"_", chosen_model)
this_dir <- paste0(BASE_DIR, "/", refinement_version)
if (!dir.exists(this_dir)){dir.create(this_dir)}
print(paste("BASE DIR:", BASE_DIR))
print(paste("this dir:", this_dir))

# Create logfile
log_con <- file(paste0(this_dir,"/V", thisstep, ".log"), open="w")
cat(paste0(chosen_model, " V", thisstep), file = log_con, sep="\n")

# Restore the object
RF_BASE <- readRDS(paste0(BASE_DIR, "/RF_", chosen_model,".rds"))
predictors <- row.names(RF_BASE$importance)
print(predictors)
descript=paste(sort(predictors), collapse="\n")

#### Refinement
model_vars_step0 <- RF_BASE %>%
  importance() %>% as.data.frame() %>%
  arrange(-(MeanDecreaseAccuracy))%>%
  row.names()

field_model_vars <- model_vars_step0

print(paste0("V", thisstep, " model vars:", field_model_vars))
cat(paste0("V", thisstep, " model vars:", field_model_vars),
        file=log_con, append = TRUE, sep="\n")
thismod <- RF_BASE

set.seed(1111)

# Train results
train_results <- tibble(df_MODEL[unique(c(
  info_list, "ParentGlobalID","Class",
  "SiteCode","Wet", "BankWidthMean",
  "Dataset","Region_detail",
  "Notes","TotalVisits", "revist",predictors, new_preds_list))]) %>%
  add_column(RF_Prediction_Majority = thismod$predicted) %>%
  bind_cols(
    predict(thismod, type="prob") %>%
      as_tibble() #Generate the prediction probabilities and bind to the first column
  ) 

#define new data frame
new_data <- df_TESTING[, c("Class", field_model_vars)]

# MAKE PREDICTION USING TEST SET
pred <- predict(thismod, newdata = new_data)

# Test results
test_results <- tibble(df_TESTING[unique(c(
  info_list, "ParentGlobalID","Class",
  "SiteCode","Wet", "BankWidthMean",
  "Dataset","Region_detail",
  "Notes","TotalVisits", "revist", predictors, new_preds_list))]) %>%
  add_column(pred) %>%
  rename(RF_Prediction_Majority="pred") %>%
  bind_cols(
    predict(thismod,
          newdata=new_data,  #Generate predictions on new data
          type="prob") %>%
    as_tibble()
    )

# Combine train and test dataframes, reclassify according to 50% probability
full_results <- rbind(train_results, test_results)
refined_results <- full_results %>% mutate(
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
  #Identify correct classifications
  EvALI_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
    T~F),
  PvIvE_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I") & RF_Prediction_50 %in% c("I")~T,
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    T~F),
  #TODO: dont count P in EvI Dry!!!
  EvIdry_correct = case_when(
    Class %in% c("E") & !Wet & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I") & !Wet & RF_Prediction_50 %in% c("I")~T,
    T~F),
  ##TODO: add PvLTP
  PvLTP_correct = case_when(
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    Class %in% c("I","E") & RF_Prediction_50 %in% c("I","E","LTP")~T,
    T~F),
  ##TODO: add PvIwet
  PvIwet_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
    T~F),
  Refinement=paste0(refinement_version)
)

write_csv(refined_results, file=paste0(this_dir,"/refined_results.csv"))

## capture rf performance in logfile
capture.output(RF_BASE, file=log_con, append = TRUE, sep="\n")

## FINAL CONFUSION MATRIX
results_conf_mat <- refined_results %>% group_by(Class, Dataset)
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

write_csv(cm_pivot_table, file=paste0(this_dir, "/confusion_matrix.csv"))

############## SMG: added to store full results to debug dir
debug_dir <- paste0(this_dir,"/debug")
if (!dir.exists(debug_dir)){dir.create(debug_dir)}

train_results_full <- tibble(df_MODEL) %>%
  add_column(RF_Prediction_Majority = thismod$predicted) %>%
  bind_cols( predict(thismod, type="prob") %>% 
               as_tibble())
test_results_full <- tibble(df_TESTING) %>%
  add_column(pred) %>%
  rename(RF_Prediction_Majority="pred") %>%
  bind_cols(predict(thismod, newdata=new_data, 
                    type="prob") %>% as_tibble()) 
full_results_full <- rbind(train_results_full, test_results_full)
refined_results_full <- full_results_full %>% mutate(
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
  #Identify correct classifications
  EvALI_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
    T~F),
  PvIvE_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I") & RF_Prediction_50 %in% c("I")~T,
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    T~F),
  #dont count P in EvI Dry!!!
  EvIdry_correct = case_when(
    Class %in% c("E") & !Wet & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I") & !Wet & RF_Prediction_50 %in% c("I")~T,
    T~F),
  #add PvLTP
  PvLTP_correct = case_when(
    Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
    Class %in% c("I","E") & RF_Prediction_50 %in% c("I","E","LTP")~T,
    T~F),
  #add PvIwet
  PvIwet_correct = case_when(
    Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
    Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
    T~F),
  Refinement=paste0(refinement_version)
)
write_csv(refined_results_full, file=paste0(debug_dir,"/full_results.csv"))
#################################

# ################ For all models, summarize:
## final summary
refined_stats <- refined_results %>%
  filter(Dataset=="Testing") %>%
  # group_by(Dataset, Region_detail) %>%
  # select(ModName, Class, Correct, EvALI_correct, Dataset,
  #        Region_detail, SiteCode) %>%
  summarise(n_tests=length(SiteCode),
            n_correct_PvIvE=sum(PvIvE_correct==TRUE),
            pctCorrectPvIvE=n_correct_PvIvE/n_tests,
            n_correct_EvALI=sum(EvALI_correct==TRUE),
            pctCorrect_EvALI=n_correct_EvALI/n_tests,
            n_correct_EvIdry=sum(EvIdry_correct==TRUE), #TODO: dont count P in EvI Dry!!!
            # n_dry=sum(Wet==FALSE),
            # pctCorrect_EvIdry=n_correct_EvIdry/n_dry
            n_dry_notP=sum(Wet==FALSE & Class %in% c("I","E")),
            pctCorrect_EvIdry=(n_correct_EvIdry/n_dry_notP),
            #TODO: did we have any P dry sites?
            n_dry_P=sum(Wet==FALSE & Class %in% c("P"))
  )
print(head(refined_stats))
write_csv(refined_stats, file=paste0(this_dir,"/refined_stats.csv"))

## capture rf performance
cat(paste("\nTRAINING"),file=log_con, append = TRUE)
capture.output(thismod, file=log_con, append = TRUE)
cat(paste("\nsummary_stats"),file=log_con, append = TRUE)
capture.output(summary_stats, file=log_con, append = TRUE, sep="\n")

if (thisstep==0){
      refined_model_summary <- tibble(Step=thisstep,
          Description=descript,
          n_varz= thismod$importance %>% nrow(),
          varz=list(field_model_vars),
          pctCorrectPvIvE=refined_stats$pctCorrectPvIvE,
          pctCorrect_EvALI=refined_stats$pctCorrect_EvALI,
          pctCorrect_EvIdry=refined_stats$pctCorrect_EvIdry
          )
    } else {
       refined_model_summary <- refined_model_summary %>% bind_rows(
       tibble(Step=thisstep,
            Description=descript,
            n_varz= thismod$importance %>% nrow(),
            varz=list(field_model_vars),
            pctCorrectPvIvE=refined_stats$pctCorrectPvIvE,
            pctCorrect_EvALI=refined_stats$pctCorrect_EvALI,
            pctCorrect_EvIdry=refined_stats$pctCorrect_EvIdry
            )
      )
}
print(refined_model_summary)
write_csv(refined_model_summary, file=paste0(this_dir,"/refined_model_summary.csv"))

#Plot steps
plot_steps <- ggsummaryPlot(refined_model_summary) + labs()
ggsave(paste0(this_dir, "/refinement", refinement_version,".png"), plot_steps,
       dpi=600, height=6, width=plotwidth)

##################################################################
#     function
##################################################################
make_refinements <- function (thisstep, chosen_model, descript, field_model_vars) {
     refinement_version <- paste0("V",thisstep,"_", chosen_model)
     this_dir <- paste0(BASE_DIR, "/", refinement_version)
     if (!dir.exists(this_dir)){dir.create(this_dir)}
     print(paste("BASE DIR:", BASE_DIR))
     print(paste("this dir:", this_dir))
     debug_dir <- paste0(this_dir,"/debug")
     if (!dir.exists(debug_dir)){dir.create(debug_dir)}
     
     # Create logfile
     log_con <- file(paste0(this_dir,"/V", thisstep, ".log"), open="w")
     cat(paste0(chosen_model, " V", thisstep), file = log_con, sep="\n") 

     print(paste0("V", thisstep, " model vars:", field_model_vars))
     cat(paste0("V", thisstep, " model vars:", field_model_vars), 
          file=log_con, append = TRUE, sep="\n")
     print(paste("MODEL",thisstep,"has these model vars:", field_model_vars))
     old_data <- df_MODEL[, c("Class", field_model_vars)]

     set.seed(1111)
     RF_step <- randomForest(Class~.,
                         data=old_data,
                         ntree=numTrees,
                         importance=T,
                         proximity=T)
     thismod <- RF_step

     # Train results
     train_results <- tibble(df_MODEL[unique(c(
       info_list, "ParentGlobalID","Class",
       "SiteCode","Wet", "BankWidthMean",
       "Dataset","Region_detail",
       "Notes","TotalVisits", "revist", field_model_vars))]) %>%
       add_column(RF_Prediction_Majority = thismod$predicted) %>%
       bind_cols(
         predict(thismod, type="prob") %>% 
           as_tibble() #Generate the prediction probabilities and bind to the first column
       ) #check bind cols
     
     
     
     # define new data frame
     new_data <- df_TESTING[, c("Class", field_model_vars)]
     # MAKE PREDICTION USING TEST SET
     pred <- predict(thismod, newdata = new_data)
     # Test results
     test_results <- tibble(df_TESTING[unique(c(
       info_list, "ParentGlobalID","Class",
       "SiteCode","Wet", "BankWidthMean",
       "Dataset","Region_detail",
       "Notes","TotalVisits", "revist", field_model_vars))]) %>%
       add_column(pred) %>%
       rename(RF_Prediction_Majority="pred") %>%
       bind_cols(
         predict(thismod, 
                 newdata=new_data, #Generate predictions
                 type="prob") %>%
           as_tibble() 
       ) 
  
     # Combine train and test dataframes, reclassify according to 50% probability
     full_results <- rbind(train_results, test_results)
     print(head(full_results))
     
     refined_results <- full_results %>% mutate(
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
       #Identify correct classifications
       EvALI_correct = case_when(
         Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
         T~F),
       PvIvE_correct = case_when(
         Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I") & RF_Prediction_50 %in% c("I")~T,
         Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
         T~F),
       #dont count P in EvI Dry!!!
       EvIdry_correct = case_when(
         Class %in% c("E") & !Wet & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I") & !Wet & RF_Prediction_50 %in% c("I")~T,
         T~F),
       #add PvLTP
       PvLTP_correct = case_when(
         Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
         Class %in% c("I","E") & RF_Prediction_50 %in% c("I","E","LTP")~T,
         T~F),
       #add PvIwet
       PvIwet_correct = case_when(
         Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
         T~F),
       Refinement=paste0(refinement_version)
     )
     
     write_csv(refined_results, file=paste0(this_dir,"/refined_results.csv"))
     
     ## capture rf performance in logfile
     capture.output(RF_BASE, file=log_con, append = TRUE, sep="\n")
     
     ## FINAL CONFUSION MATRIX
     results_conf_mat <- refined_results %>% group_by(Class, Dataset)
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
     
     write_csv(cm_pivot_table, file=paste0(this_dir, "/confusion_matrix.csv"))
     
     # Saving on object in RData format
     rdata_name <- paste0(this_dir, "/RF_", chosen_model,
                          "_", thisstep, ".rds")
     print(paste("Saving:", rdata_name))
     saveRDS(thismod, file=rdata_name)
     
     
     ############## SMG: added to store full results to debug dir
     train_results_full <- tibble(df_MODEL) %>%
       add_column(RF_Prediction_Majority = thismod$predicted) %>%
       bind_cols( predict(thismod, type="prob") %>% 
                    as_tibble())
     test_results_full <- tibble(df_TESTING) %>%
       add_column(pred) %>%
       rename(RF_Prediction_Majority="pred") %>%
       bind_cols(predict(thismod, newdata=new_data, 
                         type="prob") %>% as_tibble()) 
     full_results_full <- rbind(train_results_full, test_results_full)
     refined_results_full <- full_results_full %>% mutate(
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
       #Identify correct classifications
       EvALI_correct = case_when(
         Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
         T~F),
       PvIvE_correct = case_when(
         Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I") & RF_Prediction_50 %in% c("I")~T,
         Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
         T~F),
       #dont count P in EvI Dry!!!
       EvIdry_correct = case_when(
         Class %in% c("E") & !Wet & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I") & !Wet & RF_Prediction_50 %in% c("I")~T,
         T~F),
       #add PvLTP
       PvLTP_correct = case_when(
         Class %in% c("P") & RF_Prediction_50 %in% c("P")~T,
         Class %in% c("I","E") & RF_Prediction_50 %in% c("I","E","LTP")~T,
         T~F),
       #add PvIwet
       PvIwet_correct = case_when(
         Class %in% c("E") & RF_Prediction_50 %in% c("E")~T,
         Class %in% c("I","P") & RF_Prediction_50 %in% c("I","P","ALI")~T,
         T~F),
       Refinement=paste0(refinement_version)
     )
     write_csv(refined_results_full, file=paste0(debug_dir,"/full_results.csv"))
     #################################
     
     
     
     
     
     # ################ For all models, summarize:
     ## final summary
     refined_stats <- refined_results %>%
       filter(Dataset=="Testing") %>%
       # group_by(Dataset, Region_detail) %>%
       # select(ModName, Class, Correct, EvALI_correct, Dataset,
       #        Region_detail, SiteCode) %>%
       summarise(n_tests=length(SiteCode),
                 n_correct_PvIvE=sum(PvIvE_correct==TRUE),
                 pctCorrectPvIvE=n_correct_PvIvE/n_tests,
                 n_correct_EvALI=sum(EvALI_correct==TRUE),
                 pctCorrect_EvALI=n_correct_EvALI/n_tests,
                 n_correct_EvIdry=sum(EvIdry_correct==TRUE), #dont count P in EvI Dry!!!
                 n_dry_notP=sum(Wet==FALSE & Class %in% c("I","E")),
                 pctCorrect_EvIdry=(n_correct_EvIdry/n_dry_notP),
                 n_dry_P=sum(Wet==FALSE & Class %in% c("P")#did we have any P dry sites?
             )
       )
     print(head(refined_stats))
     write_csv(refined_stats, file=paste0(this_dir,"/refined_stats.csv"))
     
     ## capture rf performance
     cat(paste("\nTRAINING"),file=log_con, append = TRUE)
     capture.output(thismod, file=log_con, append = TRUE)
     cat(paste("\nsummary_stats"),file=log_con, append = TRUE)
     
     
     if (thisstep==0){
       refined_model_summary <- tibble(Step=thisstep,
                 Description=descript,
                 n_varz= thismod$importance %>% nrow(),
                 varz=list(field_model_vars),
                 pctCorrectPvIvE=refined_stats$pctCorrectPvIvE,
                 pctCorrect_EvALI=refined_stats$pctCorrect_EvALI,
                 pctCorrect_EvIdry=refined_stats$pctCorrect_EvIdry
       )
     } else {
       refined_model_summary <- refined_model_summary %>% bind_rows(
         tibble(Step=thisstep,
                Description=descript,
                n_varz= thismod$importance %>% nrow(),
                varz=list(field_model_vars),
                pctCorrectPvIvE=refined_stats$pctCorrectPvIvE,
                pctCorrect_EvALI=refined_stats$pctCorrect_EvALI,
                pctCorrect_EvIdry=refined_stats$pctCorrect_EvIdry
         )
       )
     }
     print(refined_model_summary)
     write_csv(refined_model_summary, file=paste0(this_dir,
                                                  "/refined_model_summary.csv"))
     
     plot_steps <- ggsummaryPlot(refined_model_summary) + labs()
     plot_steps
     # plot_steps
     ggsave(paste0(this_dir, "/refinement", refinement_version,".png"), plot_steps,
            dpi=600, height=6, width=plotwidth)

     # # Saving on object in RData format
     # rdata_name <- paste0("RF_", refinement_version, ".rds")
     # saveRDS(RF_step, file = paste0(this_dir, "/", rdata_name))

     plot_steps <- ggsummaryPlot(refined_model_summary) + labs()
     # plot_steps
     ggsave(paste0(this_dir, "/refinement", refinement_version,".png"), plot_steps,
          dpi=600, height=6, width=plotwidth)

     return(refined_model_summary)

}
##################################################################
#      V1 -->  formerly v5
##################################################################
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "Strata",
                             "UplandRootedPlants_score")
)
this_model_vars <- c(this_model_vars,"UplandRooted_PA")

refined_model_summary <- make_refinements(thisstep=1,
            chosen_model=chosen_model,
            descript=paste(sort(this_model_vars), collapse="\n"),
            field_model_vars=this_model_vars
)
##################################################################
#    V2 --> formerly v7
##################################################################
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "UplandRootedPlants_score",
                             "ephinteph_ISA_abundance",
                             "Strata",
                             "hydrophytes_present")
)
this_model_vars <- c(this_model_vars,
                     "ephISAabund_PA",
                     "UplandRooted_PA",
                     "hydrophytes_2")

refined_model_summary <- make_refinements(thisstep=2,
            chosen_model=chosen_model,
            # descript=description,
            descript=paste(sort(this_model_vars), collapse="\n"),
            field_model_vars=this_model_vars
)

##################################################################
#    V3 --> formerly v8
##################################################################
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "UplandRootedPlants_score",
                             "ephinteph_ISA_abundance",
                             "hydrophytes_present",
                             "Strata",
                             "TotalAbundance")
)
this_model_vars <- c(this_model_vars,
                     "ephISAabund_PA",
                     "UplandRooted_PA",
                     "hydrophytes_2",
                     "TotalAbund_0_10")

refined_model_summary <- make_refinements(thisstep=3,
              chosen_model=chosen_model,
              descript=paste(sort(this_model_vars), collapse="\n"),
              field_model_vars=this_model_vars
)
##################################################################
#    V4 --> formerly v9
##################################################################
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "UplandRootedPlants_score",
                             "ephinteph_ISA_abundance",
                             "hydrophytes_present",
                             "Strata",
                             "Slope",
                             "TotalAbundance")
)
this_model_vars <- c(this_model_vars,
                     "ephISAabund_PA",
                     "UplandRooted_PA",
                     "hydrophytes_2",
                     "TotalAbund_0_10")

refined_model_summary <- make_refinements(thisstep=4,
                chosen_model=chosen_model,
                descript=paste(sort(this_model_vars), collapse="\n"),
                field_model_vars=this_model_vars
)
##################################################################
#     V5 --> same as former v9 except drop ephISAabund_PA 
##################################################################
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "UplandRootedPlants_score",
                             "ephinteph_ISA_abundance",
                             "hydrophytes_present",
                             "Strata",
                             "Slope",
                             "TotalAbundance")
)
this_model_vars <- c(this_model_vars,
                     "UplandRooted_PA",
                     "hydrophytes_2",
                     "TotalAbund_0_10")

refined_model_summary <- make_refinements(thisstep=5,
                    chosen_model=chosen_model,
                    descript=paste(sort(this_model_vars), collapse="\n"),
                    field_model_vars=this_model_vars
)
##################################################################
#     V6 --> same as former v9 except drop TotalAbund_0_10
##################################################################
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "UplandRootedPlants_score",
                             "ephinteph_ISA_abundance",
                             "hydrophytes_present",
                             "Strata",
                             "Slope",
                             "TotalAbundance")
)
this_model_vars <- c(this_model_vars,
                     "UplandRooted_PA",
                     "hydrophytes_2",
                     "ephISAabund_PA")
refined_model_summary <- make_refinements(thisstep=6,
                    chosen_model=chosen_model,
                    descript=paste(sort(this_model_vars), collapse="\n"),
                    field_model_vars=this_model_vars
)

##################################################################
#     V7 --> same as former v9 except drop ephISAabund_PA and TotalAbund_0_10
##################################################################
# description<-paste0("REFINED MODEL:\nBankWidthMean\nBMI_score_alt4\nDRNAREA_0.5bin\nNaturalValley\nPctShading\nppt.8910\nSlope\nUplandRootedPlants")
this_model_vars <- setdiff(model_vars_step0,
                           c("PctShading",
                             "UplandRootedPlants_score",
                             "ephinteph_ISA_abundance",
                             "hydrophytes_present",
                             "Strata",
                             "Slope",
                             "BankWidthMean",
                             "TotalAbundance")
)
this_model_vars <- c(this_model_vars,
                            "UplandRooted_PA",
                            "hydrophytes_2",
                            "ephISAabund_PA",
                            "TotalAbund_0_10",
                     "BankWidth_10")

refined_model_summary <- make_refinements(thisstep=7,
                    chosen_model=chosen_model,
                    # descript=description,
                    descript=paste(sort(this_model_vars), collapse="\n"),
                    field_model_vars=this_model_vars
)





