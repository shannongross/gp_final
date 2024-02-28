################################################################################
# This script performs a Sensitivity Analsysis on the errors for the final model
# chosen (V4 refined)
#
# Note: last input dataset update from Rafi on 25 Jan 2024
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
library(readxl)

graphics.off()

model_version <- "DraftFinalModels2"
chosen_model <- "NoGIS_Unstrat"
chosen_version <- "V4"
CURRENT_REGION_DISPLAY <- "Great Plains V4"
plotwidth <- 10.5
numTrees <- 1500

########################## Get Data ############################################
# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Create output dirs if they do not exist
parent_path <- paste0(HOME_DIR, "/output/models/", model_version,
                      "/", chosen_model,"/", chosen_version, "_", chosen_model)

out_dir <- paste0(parent_path, "/sensitivity_analyis")
if (!dir.exists(out_dir)){dir.create(out_dir)}

print(paste("Your output dir has been set to:", out_dir))

################################ GET MODEL DATA ################################

# Get input dataset (contains augmented training, testing data)
df_input <- read_csv(paste0(HOME_DIR,"/input/processed/df_model_aug.csv"))

# Explicitly set datatypes
df_input$Class <- as.factor(df_input$Class)
df_input$Region_detail <- as.factor(df_input$Region_detail)

# Create Strata column
df_input <- df_input %>% mutate(Strata = Region_detail)

############################## CREATE NEW METRICS ##############################
df_input <- df_input %>% mutate(
  TotalAbund_0_10 = case_when(TotalAbundance==0~0, 
                              ((TotalAbundance>0) & (TotalAbundance<=10)~1),
                              TotalAbundance>=10~2),
  UplandRooted_PA = case_when(UplandRootedPlants_score<3~0, T~1),
  ephISAabund_PA = case_when(ephinteph_ISA_abundance==0~0, T~1),
  hydrophytes_2 = case_when(hydrophytes_present<3~0, T~1)
)  

############################## CURRENT METRICS ################################# 
current_metrics <- c( "BankWidthMean",
                      "SubstrateSorting_score",
                      "DifferencesInVegetation_score",
                      "RifflePoolSeq_score",
                      "SedimentOnPlantsDebris_score", 
                      "ephISAabund_PA", 
                      "UplandRooted_PA",
                      "hydrophytes_2",
                      "TotalAbund_0_10" )

# ########################### EDA PLOTS OF NEW METRICS ###########################
# for (metric in c("ephISAabund_PA",
# "hydrophytes_2",
# "TotalAbund_0_10",
# "UplandRooted_PA")) {
#   print(metric)
#   #boxplot
#   metric_box <- ggplot(df_input, aes(x = Class,
#                        y = eval(parse(text = metric)), fill = Class)) +
#     geom_boxplot() +
#     stat_summary(fun = "median", geom = "point", shape = 8,
#                  size = 3, color = "red") +
#     labs(y="", caption=paste0("(",CURRENT_REGION_DISPLAY,
#                      ")\n Number of Samples: ", dim(df_input)[1],
#                      "\n Number of Sites: ",
#                      length(unique(df_input$SiteCode))[1]))
#     # Histogram overlaid with kernel density curve
#     metric_hist <- df_input %>% ggplot(aes(x=eval(parse(text = metric)))) +
#       geom_histogram(aes(y=..density..) ) +
#       labs( x="value", y="",
#         caption=paste0("(",CURRENT_REGION_DISPLAY,
#                        ")\n Number of Samples: ", dim(df_input)[1],
#                      "\n Number of Sites: ",
#                      length(unique(df_input$SiteCode))[1]))
#     temp <- df_input[,c(metric, "SiteCode")]
#     temp$fillcolor <- is.na(temp[, metric])
#     metric_box_region <- ggplot(df_input, aes(x = Region_detail,
#                     y = eval(parse(text = metric)), fill = Region_detail)) +
#       geom_boxplot() +
#       stat_summary(fun = "median", geom = "point", shape = 8,
#                    size = 3, color = "red") +
#       labs( y="",
#         caption=paste0("(",CURRENT_REGION_DISPLAY,
#                        ")\n Number of Samples: ", dim(df_input)[1],
#                        "\n Number of Sites: ",
#                        length(unique(df_input$SiteCode))[1]))
#     ggsave(paste0(out_dir, "/metric_", metric,".png"),
#            arrangeGrob( metric_hist,
#                         arrangeGrob(metric_box, metric_box_region, ncol=2),
#                         nrow = 2,
#                         top=metric ),
#            dpi=300, height=9, width=9)
# }
################################################################################


################################ FUNCTION ######################################
# Create RF model given different input datasets
#
################################################################################
make_models <- function (input_dataset, nickname, numTrees) {
  # Set up output dir
  fpath <- paste0(out_dir, "/", nickname)
  if (!dir.exists(fpath)){dir.create(fpath)}
  else {print(paste("Creating RF for ", nickname))}
  
  # Create logfile
  log_con <- file(paste0(fpath,"/", nickname, ".log"), open="w")
  cat(paste("Starting: ", nickname), file = log_con, sep="\n") 
  cat(paste(nickname, "model vars:", current_metrics), 
      file=log_con, append = TRUE, sep="\n")
  
  # Separate datasets
  df_MODEL <- df_input %>% filter(Dataset=="Training")
  df_TEST <- df_input %>% filter(Dataset=="Testing")
  
  old_data <- df_MODEL[, c("Class", current_metrics)]
  
  ############################### CREATE RF ################################### 
  set.seed(1111)
  RF <- randomForest(Class~.,
                      data=old_data,
                      ntree=numTrees,
                      importance=T,
                      proximity=T)
  
  # Saving on object in RData format
  rdata_obj <- paste0("RF_", nickname, ".rds")
  saveRDS(RF, file = paste0(fpath, "/", rdata_obj))
  #############################################################################

  # Train results
  train_results <- tibble(df_MODEL) %>%
    add_column( RF_Prediction_Majority = RF$predicted) %>%
    bind_cols(
      predict(RF, type="prob") %>% 
        as_tibble() 
    )
  
  # define new data frame
  new_data <- df_TEST[, c("Class", current_metrics)]
  pred <- predict(RF, newdata = new_data)
  # Test results
  test_results <- tibble(df_TEST) %>%
    add_column(pred) %>%
    rename(RF_Prediction_Majority="pred") %>%
    bind_cols(
      predict(RF, 
              newdata=new_data, #Generate predictions
              type="prob") %>%
        as_tibble() 
    ) 
  
  # Combine train and test dataframes, reclassify according to 50% probability
  comb_results <- rbind(train_results, test_results)
  comb_results <- comb_results %>% mutate(
    #Reclassify with 50% minimum probability
    RF_Prediction_50=case_when(
      E>=.5~"E",
      I>=.5~"I",
      P>=.5~"P",
      P>E~"ALI",
      E>P~"LTP",
      P==E & I>P~"NMI", # no longer LI
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
    EvIdry_correct = case_when(
      Class %in% c("E") & !Wet & RF_Prediction_50 %in% c("E")~T,
      Class %in% c("I") & !Wet & RF_Prediction_50 %in% c("I")~T,
      T~F),
    ModName=nickname
  )
  write_csv(comb_results, file=paste0(fpath,"/full_results.csv"))
  
  ## capture rf performance in logfile
  capture.output(RF, file=log_con, append = TRUE, sep="\n")
  
  ## FINAL CONFUSION MATRIX
  results_conf_mat <- comb_results %>% group_by(Class, Dataset)
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
  
  write_csv(cm_pivot_table, file=paste0(fpath, "/confusion_matrix.csv"))
  
}
################################ original dataset ############################## 
make_models(input_dataset=df_input, 
            nickname="V4original", 
            numTrees=numTrees) 

################################ original dataset ############################## 
make_models(input_dataset=df_input, 
            nickname="V4original", 
            numTrees=numTrees) 

################################ original dataset ############################## 
make_models(input_dataset=df_input, 
            nickname="V4original", 
            numTrees=numTrees) 
################################################################################


set.seed(1111)
df_MODEL <- df_input %>% filter(Dataset=="Training")
df_TEST <- df_input %>% filter(Dataset=="Testing")
X_train  <- df_MODEL[, current_metrics]
y_train = df_MODEL$Class
X_test<- df_TEST[, current_metrics]
y_test <- df_TEST$Class


RF_initial <- randomForest(x=X_train,
                           y=y_train,
                           ntree=numTrees,
                           importance=T, 
                           verbose=T, 
                           proximity=T)
options(dplyr.summarise.inform = FALSE)
# define new data frame
testsample <- df_TEST[3,]
testsample <- testsample[, c("ParentGlobalID","SiteCode","Class", current_metrics)]


sensitivity_analysis_setbound <- function(perturbed_metric, minb, maxb, stepb){
  originalvalue <- testsample[[perturbed_metric]]
  
  originalpredsample <- predict(RF, newdata = testsample, type="prob") %>% 
    as_tibble() %>% mutate(
      RF_Prediction_50=case_when(
        E>=.5~"E",
        I>=.5~"I",
        P>=.5~"P",
        P>E~"ALI",
        E>P~"LTP",
        P==E & I>P~"NMI",
        P==E & I<=P~"NMI",
        T~"Other"))
  print("originalpredsample$RF_Prediction_50")
  print(originalpredsample$RF_Prediction_50)
  datalist = list()
  
  for (r in seq(minb, maxb, by=stepb)){
    # print(r)
    
    testsample[[perturbed_metric]] <- r
    # print(testsample)
    pred <- predict(RF, newdata = testsample[, c("Class", current_metrics)])
    # print(pred)
    predsample <- predict(RF, newdata = testsample, type="prob")
    predclass <- colnames(predsample)[apply(predsample, 1, which.max)]
    predprod <- max(predsample)
    # print(paste(perturbed_metric,"value of", r, "predicts:", predclass, 
                # "with prob:", predprod, "(using majority voting). 50% update below:" ))

    # Test results
    test_results <- tibble(testsample) %>%
      add_column(pred) %>%
      rename(RF_Prediction_Majority="pred") %>%
      bind_cols(predsample %>%as_tibble())
    
    comb_results <- test_results %>% mutate(
      #Reclassify with 50% minimum probability
      RF_Prediction_50_Perturbed=case_when(
        E>=.5~"E",
        I>=.5~"I",
        P>=.5~"P",
        P>E~"ALI",
        E>P~"LTP",
        P==E & I>P~"NMI",
        P==E & I<=P~"NMI",
        T~"Other"),
      Metric_Perturbed = perturbed_metric,
      Original_Value = originalvalue
      
    ) 
    # print(comb_results$RF_Prediction_50 )
    # datalist[[r]] <- comb_results
    datalist <- rbind(datalist, comb_results)
    
    }
  return (datalist)
}
res <- sensitivity_analysis_setbound(perturbed_metric="BankWidthMean", 
                              minb=0,
                              maxb=100,
                              stepb=20)
res


sensitivity_analysis_setbound(perturbed_metric="DifferencesInVegetation_score", 
                              minb=0,
                              maxb=5,
                              stepb=2)




sensitivity_analysis_autobound <- function(nsteps, perturbed_metric){
    print(paste("Perturbing:", perturbed_metric))
    minb=min(df_input[[perturbed_metric]])
    maxb=max(df_input[[perturbed_metric]])
    stepb=round((maxb- minb)/nsteps)
    if (stepb == 0){
      stepb=1
    }
    print(paste("nsteps:", nsteps))
    print(paste("minb:", minb))
    print(paste("maxb:", maxb))
    print(paste("stepb:", stepb))
  
    for (r in seq(minb, maxb, by=stepb)){
        # print(r)
        testsample[[perturbed_metric]] <- r
        # print(testsample)
        
        predsample <- predict(RF, newdata = testsample, type="prob")
        predclass <- colnames(predsample)[apply(predsample,1,which.max)]
        predprod <- max(predsample)
        print(paste(perturbed_metric,"value of", r, "predicts:", predclass, 
                    "with prob:", predprod ))
        
    }
}
sensitivity_analysis_autobound(perturbed_metric="DifferencesInVegetation_score", nsteps=4)



mycopy <- testsample
mycopy$BankWidthMean  <- 1
mycopy$SubstrateSorting_score <- 1
mycopy$DifferencesInVegetation_score<- 1
mycopy$RifflePoolSeq_score  <- 1



predsample <- predict(RF, newdata = mycopy, type="prob")
predsample




set.seed(1111)
df_MODEL <- df_input %>% filter(Dataset=="Training")
df_TEST <- df_input %>% filter(Dataset=="Testing")
X_train  <- df_MODEL[, current_metrics]
y_train = df_MODEL$Class
X_test<- df_TEST[, current_metrics]
y_test <- df_TEST$Class


RF_initial <- randomForest(x=X_train,
                           y=y_train,
                           ntree=numTrees,
                           importance=T, 
                           verbose=T, 
                           proximity=T)

