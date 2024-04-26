#################################################################################
# This script tests out various preliminary models for the random forest modeling
# including stratified vs unstratified and No GIS vs only a *limited* number of 
# GIS variables. 
#################################################################################
library(tidyverse)
library(skimr)
library(ggforce)
library(randomForest)
library(grid)
library(gridExtra)
library(caret)
library(dplyr)
library(ggplot2)
library(rsample)
library(readxl)
library(tidyr)
library(reshape2)

# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# Create dir for storage
models_dir <- paste0(HOME_DIR, "/output/models/")
if (!dir.exists(models_dir)){dir.create(models_dir)}

################################ SETUP PRELIM MODELS ###########################
# model_version: a nickname for the prelim model
# case_list: which models to run. Uncomment ones you want to run.
################################################################################
# name of directory to store these results
model_version <- "FinalModelQC_Apr2024"

# how many trees
numOfTrees <- 1500 

# models to run
case_list <- c(
  # "BaseModel_Unstrat", #for reference only
  # "BaseModel_S",       #for reference only
  # "BaseModel_UNC",      #for reference only

  "NoGIS_Unstrat"#,   #also, 2BMI limit? #more of a reference
  # "NoGIS_S",         #also, 2BMI limit?
  # "NoGIS_UNC",       #also, 2BMI limit?
  # 
  # "2GIS_Unstrat",    #also, 2BMI limit
  # "2GIS_UNC",        #also, 2BMI limit
  # "2GIS_S"           #also, 2BMI limit
)

  
###############################  LISTS OF DATA #################################
# Lists to store features (for convenience).
#
################################################################################
# identifying columns
info_list <- c("ParentGlobalID","CollectionDate","Region",
               "Region_detail","SiteCode","Class","Dataset", "Wet")

# Get all candidate predictors that passed screening
predictor_summary <- read_csv(paste0(HOME_DIR,
                       "/output/screening/metric_summary.csv"))
predictor_summary <- predictor_summary %>% select(c("Predictor", "PassScreens"))
candidates_failed_screen <- (predictor_summary %>% 
                               filter(!PassScreens))$Predictor
print(paste("candidates_failed_screen", candidates_failed_screen))

# use data dictionary to subset data into lists
metrics_lookup <- read_xlsx("input/raw/metrics_dictionary.xlsx",
                            sheet = "DATA_DICT") %>%
    filter(MetricSubtype!="Direct") %>%
    filter(MetricCandidate_KF=="TRUE")
gp_bm_metrics <- (metrics_lookup %>% filter(BaseModel_Unstrat=="TRUE"))$Metric
sgp_bm_metrics <- (metrics_lookup %>% filter(BaseModel_S=="TRUE"))$Metric
ngp_bm_metrics <- (metrics_lookup %>% filter(BaseModel_UNC=="TRUE"))$Metric

gp_nogis_metrics <- (metrics_lookup %>% filter(NoGIS_Unstrat=="TRUE"))$Metric
sgp_nogis_metrics <- (metrics_lookup %>% filter(NoGIS_S=="TRUE"))$Metric
ngp_nogis_metrics <- (metrics_lookup %>% filter(NoGIS_UNC=="TRUE"))$Metric

# candidate_list <- metrics_lookup$Metric
metrics_gis_list <- (metrics_lookup %>% filter(MetricType =="Geospatial"))$Metric


################################ GET MODEL DATA ################################
# Stored in csv file
#
################################################################################
# Get input dataset (contains augmented training, testing data)
df_input <- read_csv(paste0(HOME_DIR,"/input/processed/df_model_aug.csv"))

# Explicitly set datatypes
df_input$Class <- as.factor(df_input$Class)
df_input$Region <- as.factor(df_input$Region)
df_input$Region_detail <- as.factor(df_input$Region_detail)
# df_input$HydricSoils_score <- as.numeric(df_input$HydricSoils_score)

# Create Strata column
df_input <- df_input %>% mutate( Strata = Region_detail)

# create a copy for manipulation
df_input_copy <- df_input

# Drop any rows with NaN (there shouldn't be any at this point)
print(paste("Number of rows:", nrow(df_input_copy)))
df_input_copy <- df_input_copy %>% na.omit()
print(paste("Number of rows after dropping nans:",nrow(df_input_copy)))

df_input_copy <- df_input_copy %>%
  group_by(SiteCode) %>%
  mutate(VisitNo = 1:n())

# Separate datasets
df_test <- df_input_copy %>% filter(Dataset=="Testing")
df_train <- df_input_copy %>% filter(Dataset=="Training" & Notes=="Original")
df_aug_train <- df_input_copy %>% filter(Dataset=="Training")


############################# RANDOM FOREST GENERATION #########################
# The following function `make_models` can be used to generate RFs for different
# cases. Each case may have different strata it can choose from, as well as
# different candidate metrics allowed (e.g. No GIS).
#
################################################################################
# Create dir to store model results if it does not exist yet
modelsdir <- paste0(models_dir, model_version)
if (!dir.exists(modelsdir)){dir.create(modelsdir)}

make_models <- function (case, numTrees) {
  start_time <- Sys.time()
  debug_dir <- paste0(modelsdir, "/", case, "/debug")
  if (!dir.exists(debug_dir)){dir.create(debug_dir)}
  
  # Set up output dir
  fpath <- paste0(modelsdir, "/", case)
  if (!dir.exists(fpath)){dir.create(fpath)}
  else {print(paste("Creating RF for ", case))}
  
  # Create logfile
  log_con <- file(paste0(fpath,"/", case, ".log"), open="w")
  cat(paste("Starting case: ", case), file = log_con, sep="\n") 
  
  print(paste("***STARTING CASE",case))
  col_pre_pred <- c(
    info_list, 
    "VisitNo","Region_detail","revist", "Notes")
  col_post_pred <- c(
    info_list, "RF_Prediction_50","RF_Prediction_Majority",
    "EvIdry_correct","PvIvE_correct","EvALI_correct",
    "VisitNo", "Stratification","ModName","Region_detail",
    "Notes","TotalVisits", "revist")
  
  # Config model cases
  if(case == "BaseModel_Unstrat"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(gp_bm_metrics, candidates_failed_screen)
    
    # All eligible candidates
    candidate_preds <- setdiff(gp_bm_metrics, candidates_failed_screen)
    
    df_MODEL <- df_aug_train
    df_TEST <- df_test
    is_strat <- "Unstrat"
    topGIS <- "All GIS included"
  }

  else if(case == "BaseModel_S"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(sgp_bm_metrics, candidates_failed_screen)
    
    # All eligible candidates
    candidate_preds <- setdiff(sgp_bm_metrics, candidates_failed_screen)
    
    df_MODEL <- df_aug_train[df_aug_train$Region_detail %in% c('GP_S'),]
    df_TEST <- df_test[df_test$Region_detail %in% c('GP_S'),]
    is_strat <- "Strat"
    topGIS <- "All GIS included"
  }
  else if(case == "BaseModel_UNC"){ 
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(ngp_bm_metrics, candidates_failed_screen)
    
    # All eligible candidates
    candidate_preds <- setdiff(ngp_bm_metrics, candidates_failed_screen)
    
    df_MODEL <- df_aug_train[df_aug_train$Region_detail %in% 
                               c('GP_U','GP_N','GP_C'),]
    df_TEST <- df_test[df_test$Region_detail %in% 
                         c('GP_U','GP_N','GP_C'),]
    is_strat <- "Strat"
    topGIS <- "All GIS included"
  }

  
  else if(case == "NoGIS_Unstrat"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(gp_nogis_metrics, candidates_failed_screen)
    
    # All eligible candidates
    candidate_preds <- setdiff(gp_nogis_metrics, candidates_failed_screen)
     
    print("all the candidate preds:")
    print(candidate_preds)
    df_MODEL <- df_aug_train
    df_TEST <- df_test
    is_strat <- "Unstrat"
    topGIS <- "NA- No GIS allowed." 
    
  }
  else if(case == "NoGIS_S"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(sgp_nogis_metrics, candidates_failed_screen)
    
    # All eligible candidates
    candidate_preds <- setdiff(sgp_nogis_metrics, candidates_failed_screen)
    
    print("all the candidate preds:")
    print(candidate_preds)
    df_MODEL <- df_aug_train[df_aug_train$Region_detail %in% 
                               c('GP_S'),]
    df_TEST <- df_test[df_test$Region_detail %in% 
                         c('GP_S'),]
    is_strat <- "Strat"
    topGIS <- "NA- No GIS allowed."
    
  }
  else if(case == "NoGIS_UNC"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(ngp_nogis_metrics, candidates_failed_screen)
    
    # All eligible candidates
    candidate_preds <- setdiff(ngp_nogis_metrics, candidates_failed_screen)
    
    
    print("all the candidate preds:")
    print(candidate_preds)
    df_MODEL <- df_aug_train[df_aug_train$Region_detail %in% 
                               c('GP_U','GP_N','GP_C'),]
    df_TEST <- df_test[df_test$Region_detail %in% 
                         c('GP_U','GP_N','GP_C'),]
    is_strat <- "Strat"
    topGIS <- "NA- No GIS allowed."
    
  }
  else if(case == "2GIS_Unstrat"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(gp_bm_metrics, candidates_failed_screen)
    
    # All eligible candidates
    eligible <- setdiff(gp_bm_metrics, candidates_failed_screen)
    
    # Remove GIS candidates
    eligibleNoGIS <- setdiff(eligible, metrics_gis_list)
    
    # Eligible candidates that are also GIS
    preds_allCandidates_GIS <- intersect(eligible, metrics_gis_list)
    
    # Restore the full model (all GIS allowed)
    base_model_candidates <- readRDS(paste0(modelsdir, "/BaseModel_Unstrat/debug/RF_initial_BaseModel_Unstrat.rds"))
    base_model_candidates <- as.data.frame(base_model_candidates$importance)
    base_model_candidates_gis <- base_model_candidates %>%
      filter(row.names(base_model_candidates)
             %in% c(preds_allCandidates_GIS))
    
    # Get the top 2 GIS metrics
    topGIS <- rownames(base_model_candidates_gis[1:2,])
    print(paste("top 2 GIS:", topGIS))

    # Add back in top 2 performing GIS
    candidate_preds <- c(eligibleNoGIS, topGIS)
    
    print("all the candidate preds:")
    print(candidate_preds)
    df_MODEL <- df_aug_train
    df_TEST <- df_test
    is_strat <- "Unstrat"
    
  }
  else if(case == "2GIS_UNC"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(ngp_bm_metrics, candidates_failed_screen)
    
    # All eligible candidates
    eligible <- setdiff(ngp_bm_metrics, candidates_failed_screen)
    
    # Remove GIS candidates
    eligibleNoGIS <- setdiff(eligible, metrics_gis_list)
    
    # Eligible candidates that are also GIS
    preds_allCandidates_GIS <- intersect(eligible, metrics_gis_list)
    
    # Restore the full model (all GIS and BMI allowed)
    base_model_candidates <-  readRDS(paste0(modelsdir, "/BaseModel_UNC/debug/RF_initial_BaseModel_UNC.rds"))
    base_model_candidates <- as.data.frame(base_model_candidates$importance)
    base_model_candidates_gis <- base_model_candidates %>%
      filter(row.names(base_model_candidates)
             %in% c(preds_allCandidates_GIS))
    
    # Get the top 2 GIS metrics
    topGIS <- rownames(base_model_candidates_gis[1:2,]) #put explicty ordering back in
    print(paste("top 2 GIS:", topGIS))
    
    # Add back in top 2 performing GIS
    candidate_preds <- c(eligibleNoGIS, topGIS)
    
    print("all the candidate preds:")
    print(candidate_preds)
    df_MODEL <- df_aug_train[df_aug_train$Region_detail %in% 
                               c('GP_U','GP_N','GP_C'),]
    df_TEST <- df_test[df_test$Region_detail %in% 
                         c('GP_U','GP_N','GP_C'),]
    is_strat <- "Strat"
  }
  
  else if(case == "2GIS_S"){
    # Tracking eligible but failed screening
    eligible_but_failed <- intersect(sgp_bm_metrics, candidates_failed_screen)
    
    # All eligible candidates
    eligible <- setdiff(sgp_bm_metrics, candidates_failed_screen)
    
    # Remove GIS candidates
    eligibleNoGIS <- setdiff(eligible, metrics_gis_list)
    
    # Eligible candidates that are also GIS
    preds_allCandidates_GIS <- intersect(eligible, metrics_gis_list)
    
    # Restore the full model (all GIS and BMI allowed)
    base_model_candidates <-  readRDS(paste0(modelsdir, "/BaseModel_S/debug/RF_initial_BaseModel_S.rds"))
    base_model_candidates <- as.data.frame(base_model_candidates$importance)
    base_model_candidates_gis <- base_model_candidates %>%
      filter(row.names(base_model_candidates)
             %in% c(preds_allCandidates_GIS))
    
    # Get the top 2 GIS metrics
    topGIS <- rownames(base_model_candidates_gis[1:2,])
    print(paste("top 2 GIS:", topGIS))
    
    # Add back in top 2 performing GIS
    candidate_preds <- c(eligibleNoGIS, topGIS)
    
    print("all the candidate preds:")
    print(candidate_preds)
    df_MODEL <- df_aug_train[df_aug_train$Region_detail %in% c('GP_S'),]
    df_TEST <- df_test[df_test$Region_detail %in% c('GP_S'),]
    is_strat <- "Strat"
    }
  
    else {
      stop("Invalid case entered.")
    }
    debug_dir <- paste0(modelsdir, "/", case, "/debug")
    if (!dir.exists(debug_dir)){dir.create(debug_dir)}
    
    # store data used to generate model
    write.csv(df_MODEL, paste0(debug_dir, "/df_training.csv"))

    ############################ Random Forest Model ###########################
    X_train  <- df_MODEL[, candidate_preds]
    y_train = df_MODEL$Class
    
    X_test<- df_TEST[, candidate_preds]
    y_test <- df_TEST$Class
    
    ## Get optimal features w backwards feature selection
    len_predictors <- length(candidate_preds)
    print(paste(case, "len_predictors",len_predictors))
    
    cat(paste("(Eligible but failed screening: ", eligible_but_failed, ")"), 
        file=log_con, append = TRUE, sep="\n")
    cat(paste("Starting with: ", len_predictors, " candidate predictors"), 
        file=log_con, append = TRUE, sep="\n")
    cat(paste("Possible predictors: ", sort(candidate_preds)), 
        file=log_con, append = TRUE, sep="\n")
    
    cat(paste("topGIS: ", topGIS), file=log_con, append = TRUE, sep="\n")

    ############################### MAKE RF INITIAL ############################# 
    set.seed(3)
    RF_initial <- randomForest(x=X_train,
                       y=y_train,
                       ntree=numTrees,
                       importance=T, 
                       verbose=T, 
                       proximity=T)
    
    # Saving on object in RData format
    rdata_obj <- paste0("RF_initial_", case, ".rds")
    saveRDS(RF_initial, file = paste0(debug_dir, "/", rdata_obj))
    ############################################################################
    
    set.seed(4)
    RFE_obj <- rfe(
      y=y_train,
      x=X_train,
      sizes=c(10:20, seq(from=25, to=len_predictors, length.out = 10),
              len_predictors) %>% unique() %>% round(),
      rfeControl = rfeControl(functions = rfFuncs,
                              method = "cv",
                              verbose = TRUE,
                              returnResamp = "all"),
      metric="Kappa",
      maximize = T )
    print(RFE_obj)
    print("finished Recursive Feature Elimination ")

    ## capture RFE performance
    capture.output(RFE_obj, file=log_con, append = TRUE, sep="\n")
    opt_size <- pickSizeTolerance(RFE_obj$results[1:20,], metric="Kappa",
                                  tol=0.5, #denotes the acceptable difference in optimal performance
                                  maximize = T)

    print(paste("opt_size for case:",case, opt_size))

    #If more than 20 variables chosen by rfe, limit to 20
    opt_preds <- pickVars(RFE_obj$variables, size=min(opt_size, 20))
    print(paste("opt_preds:",opt_preds))
    # Some logging
    cat(paste("\nBackwards rfe chose: ", length(opt_preds), 
              " possible predictors"), file=log_con, append = TRUE, sep="\n")
    cat(paste("Optimal predictors: ", opt_preds), 
        file=log_con, append = TRUE, sep="\n")
    predictors_kappa <- (RFE_obj$results %>% as.data.frame() %>%
                           filter(Variables==opt_size))$Kappa
    cat(paste("Optimal predictors kappa: ", predictors_kappa),
        file=log_con, append = TRUE, sep="\n")
    
    # Make RF
    set.seed(5)
    RF <- randomForest(x=X_train[opt_preds],
                       y=y_train,
                       ntree=numTrees,
                       importance=T, 
                       verbose=T, 
                       proximity=T)
    
    # Train results
    train_results <- tibble(df_MODEL[unique(c(
          info_list, "ParentGlobalID","Class",
          "SiteCode","Wet", "BankWidthMean",
          "VisitNo","Dataset","Region_detail",
          "Notes","TotalVisits", "revist",candidate_preds))]) %>%
        add_column(Stratification=is_strat,
        RF_Prediction_Majority = RF$predicted) %>%
        bind_cols(
          predict(RF, type="prob") %>% 
          as_tibble() #Generate the prediction probabilities and bind to the first column
        ) #check bind cols
    
    
    pred <- predict(RF, newdata = X_test[opt_preds])
    # Test results
    test_results <- tibble(df_TEST[unique(c(
          info_list, "ParentGlobalID","Class",
          "SiteCode","Wet", "BankWidthMean",
          "VisitNo","Dataset","Region_detail",
          "Notes","TotalVisits", "revist",candidate_preds))]) %>%
        add_column(Stratification=is_strat, pred) %>%
        rename(RF_Prediction_Majority="pred") %>%
        bind_cols(
          predict(RF, 
                newdata=X_test[opt_preds], #Generate predictions
                type="prob") %>%
          as_tibble() 
    ) ##make sure that df_TEST and X_test are perfectly aligned when using bind cols
    
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
        EvIdry_correct = case_when(
            Class %in% c("E") & !Wet & RF_Prediction_50 %in% c("E")~T,
            Class %in% c("I") & !Wet & RF_Prediction_50 %in% c("I")~T,
            T~F),
        ##add PvLTP
      
        ModName=paste0(model_version,"_",case)
        )
    write_csv(comb_results, file=paste0(fpath,"/combined_results.csv"))
    write_csv(comb_results[, c(unique(c(info_list, "ModName","ParentGlobalID",
      "Class", "SiteCode","Wet", "BankWidthMean","VisitNo","Dataset",
      "Region_detail","Notes","TotalVisits", "revist","Stratification",
      "RF_Prediction_Majority","RF_Prediction_50","EvALI_correct",
      "PvIvE_correct","EvIdry_correct")))], 
      file=paste0(debug_dir,"/results_for_plots.csv"))
  
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
    possible <- unique(results_conf_mat2$RF_Prediction_50)
    # results_conf_mat3$RowSums <- rowSums(
    # results_conf_mat3[,c(possible)], na.rm=TRUE)
    colsums <- colSums( results_conf_mat3[,c(possible)], na.rm=TRUE)
    results_conf_mat3[is.na(results_conf_mat3)] <- 0
    write_csv(rbind(results_conf_mat3, colsums), 
              file=paste0(debug_dir,"/old_confusion_matrix.csv"))
    
    melted <- melt(results_conf_mat3)
    cm_pivot_table <- melted %>%
        pivot_wider(
          names_from = c(ActualClass, Dataset),
          values_from = value
        ) %>% arrange(factor(variable, levels = c('E', 'I', 'ALI', 'P', 'LTP', 'NMI')))
    
    write_csv(cm_pivot_table, file=paste0(fpath,"/confusion_matrix.csv"))


    ## Plot importance of chosen variables
    varimp <- varImpPlot(RF, 
                         main="RandomForest: Variables in Order of Importance")
    varimp <- as.data.frame(varimp)
    varimp$SelPred <- rownames(varimp) # row names to column
    rownames(varimp) <- NULL
    varimp$ModName <- case
    write_csv(varimp, file=paste0(debug_dir, "/varImp.csv"))

    ## Plot importance of chosen variables
    png(file=paste0(modelsdir, "/", case, "/varImp.png"), width=800, height=450)
    varImpPlot(RF, main="RandomForest: Variables in Order of Importance")
    dev.off()
    
    # Saving on object in RData format
    rdata_name <- paste0("RF_", case, ".rds")
    saveRDS(RF, file = paste0(modelsdir, "/",  case, "/", rdata_name))
    
    ## Big errors
    df_big_errors <- comb_results %>% select(all_of(
      c(info_list, "RF_Prediction_50","ModName","Stratification","Notes"))) %>%
      filter((Class=="E"& RF_Prediction_50=="P")|(Class=="P"& RF_Prediction_50=="E"))
    if (length(df_big_errors)>0) {
      write_csv(df_big_errors, file=paste0(debug_dir, "/df_big_errors.csv"))
    }

    
    comb_results1 <- comb_results %>% filter(Dataset=="Training")
    comb_results2 <- comb_results %>% filter(Dataset=="Testing")
    
    plot_summary_train <- ggplot(comb_results1, aes(Class, fill=RF_Prediction_50)) + 
      geom_bar() +
      labs(subtitle = "Training results", y="Actual Class",x="",
           caption=paste0("(",case,
                          ")\n Number of Samples: ", dim(comb_results1)[1], 
                          "\n Number of Sites: ", 
                          length(unique(comb_results1$SiteCode))[1]) ) # +
      # scale_fill_manual(values=c("#07b52a", #ALI
      #                            "#eb344f", #E
      #                            "#edea1f", #I
      #                            "#e09c09", #LTP
      #                            "#4764e6", #P
      #                            "grey"))   #nmi
    
    plot_summary_test <- ggplot(comb_results2, aes(Class, fill=RF_Prediction_50)) + 
      geom_bar() +
      labs(subtitle = "Testing results", y="Actual Class", x="",
           caption=paste0("(",case,
                          ")\n Number of Samples: ", dim(comb_results2)[1], 
                          "\n Number of Sites: ", 
                          length(unique(comb_results2$SiteCode))[1]) ) #+
      # scale_fill_manual(values=c("#07b52a", #ALI
      #                            "#eb344f", #E
      #                            "#edea1f", #I
      #                            "#e09c09", #LTP
      #                            "#4764e6", #P
      #                            "grey"))   #nmi

    ggsave(paste0(debug_dir, "/", case,"_plot_summary.png"), 
                 arrangeGrob(plot_summary_train,
                             plot_summary_test,
                             nrow = 1, top= case),
                 dpi=300, height=4, width=8)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(paste0("******* FINISHED CASE: ", case, "*******"))
  cat(run_time)
  print(run_time)
}

############################### RUN RANDOM FORESTS #############################
# Create RF for all the following cases
#
################################################################################

## Generate RFs
for (c in case_list) {
  make_models(c, numOfTrees)
}

