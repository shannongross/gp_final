################################################################################
# This script performs a Sensitivity Analsysis on the errors for the final GP 
# model chosen (V5 refined)
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

model_version <- "Final_GP_Model"
chosen_model <- "NoGIS_Unstrat"
chosen_version <- "V5"
CURRENT_REGION_DISPLAY <- "Great Plains Final"
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

output_path <- paste0(parent_path, "/FINAL_RESULTS")
if (!dir.exists(output_path)){dir.create(output_path)}

sa_dir <- paste0(output_path, "/sensitivity_analyis")
if (!dir.exists(sa_dir)){dir.create(sa_dir)}

################################ GET MODEL DATA ################################
df_refined_results <- read_csv(paste0(output_path, "/GreatPlainsFinalResults.csv"))

# Explicitly set datatypes
df_refined_results$Class <- as.factor(df_refined_results$Class)
df_input <- df_refined_results


############################## CURRENT METRICS #################################
#NOTE: feed the list of "current metrics" in the same order that the model was
#originally created in order to replicate the results exactly. Otherwise you may
#receive slightly different results (similar to a different random seed)

current_metrics <- c( "BankWidthMean",
                      "SubstrateSorting_score",
                      "DifferencesInVegetation_score",
                      "RifflePoolSeq_score",
                      "SedimentOnPlantsDebris_score", 
                      "UplandRooted_PA",
                      "hydrophytes_2",
                      "TotalAbund_0_10" )




################################ CREATE FAKE DATA ###################################
set.seed(1111)
df_MODEL <- df_refined_results %>% filter(Dataset=="Training")
df_TEST <- df_refined_results %>% filter(Dataset=="Testing")
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

# # define a small dataset to do this perturbation on. 
# testsample <- sample_n(df_TEST, 7) #Create fake data for 5 random sites
# testsample <- testsample[, c("ParentGlobalID","SiteCode","Class", current_metrics)]
# testsample
# 
# 
# ########################### EDA PLOTS OF ALL METRICS FOR SE ###########################
# for (metric in c(current_metrics)) {
#   print(metric)
# 
#   #boxplot
#   metric_box <- ggplot(df_refined_results, aes(x = Class,
#                             y = eval(parse(text = metric)), fill = Class)) +
#     geom_boxplot() +
#     stat_summary(fun = "median", geom = "point", shape = 8,
#                  size = 3, color = "red") +
#     labs(y="", x="")
#   # Histogram 
#   if (metric %in% c("SubstrateSorting_score",
#                     "hydrophytes_2",
#                     "DifferencesInVegetation_score",
#                     "RifflePoolSeq_score",
#                     "TotalAbund_0_10",
#                     "UplandRooted_PA",
#                     "SedimentOnPlantsDebris_score",
#                     "FibrousRootedPlants_score",
#                     "UplandRootedPlants_score")){
#     metric_hist <- df_refined_results %>% ggplot(
#           aes(x=eval(parse(text = metric)))) +
#       geom_histogram(aes(y=..density.., fill=Class), #alpha=0.6, 
#                      position = 'dodge') +
#       labs(x="value", y="",
#            caption=paste0("(Great Plains Final)\n Number of Samples: ",
#                           dim(df_refined_results)[1],
#                           "\n Number of Sites: ",
#                           length(unique(df_refined_results$SiteCode))[1])
#       )
#   } else {
#     metric_hist <- df_refined_results %>% ggplot(
#             aes(x=eval(parse(text = metric)))) +
#       geom_histogram(aes(y=..density.., fill=Class), alpha=0.6, 
#                      position = 'identity') +
#       labs(x="value", y="",
#            caption=paste0("(Great Plains Final)\n Number of Samples: ",
#                           dim(df_refined_results)[1],
#                           "\n Number of Sites: ",
#                           length(unique(df_refined_results$SiteCode))[1])
#       )
#   }
#   metric_dens <- df_refined_results %>% ggplot(
#           aes(x=eval(parse(text = metric)))) +
#           geom_density(aes(y=..density.., fill=Class), alpha=0.6) +
#     labs(x="value", y="",
#          caption=paste0("(Great Plains Final)\n Number of Samples: ",
#                         dim(df_refined_results)[1],
#                         "\n Number of Sites: ",
#                         length(unique(df_refined_results$SiteCode))[1])
#     )
#   
#   metric_box_region <- ggplot(df_refined_results, aes(x = Dataset,
#                                      y = eval(parse(text = metric)), 
#                                      fill = Dataset)) +
#     geom_boxplot() +
#     stat_summary(fun = "median", geom = "point", shape = 8,
#                  size = 3, color = "red") +
#     labs(x="", y="")
#   metric_box_region
#   
#   
#   ggsave(paste0(sa_dir, "/candidates/GP_metric_", metric,".png"),
#          arrangeGrob( metric_hist,
#                       arrangeGrob(metric_box, metric_box_region, ncol=2),
#                       nrow = 2,
#                       top=metric ),
#          dpi=300, height=6, width=7)
#   ggsave(paste0(sa_dir, "/candidates/GP_metric_dens_", metric,".png"),
#          arrangeGrob( metric_dens,
#                       arrangeGrob(metric_box, metric_box_region, ncol=2),
#                       nrow = 2,
#                       top=metric ),
#          dpi=300, height=6, width=7)
# }
# ################################################################################
# 
# 
# 
# ######################### Perturb input data (automatically) ###################
# # Inputs:
# # - perturbed_metric: Name of the variable you're going to perturb
# # - nsteps: How many intervals you want to create
# # This function takes the max and min of the metric, divides by the nsteps you
# # specify, and then iteratively sets the metric equal to each step value and
# # gets a new prediction. Result is a dataframe of results.
# 
# sensitivity_analysis_autobound <- function(nsteps, perturbed_metric){
#   print(paste("Perturbing:", perturbed_metric))
#   minb=min(df_input[[perturbed_metric]])
#   maxb=max(df_input[[perturbed_metric]])
#   stepb=round((maxb- minb)/nsteps)
#   if (stepb == 0){
#     stepb=1
#   }
#   print(paste("nsteps:", nsteps))
#   print(paste("minb:", minb))
#   print(paste("maxb:", maxb))
#   print(paste("stepb:", stepb))
#   
#   originalvalue <- testsample[[perturbed_metric]]
#   
#   originalpredsample <- predict(RF_initial, newdata = testsample, type="prob") %>% 
#     as_tibble() %>% mutate(
#       RF_Prediction_50=case_when(
#         E>=.5~"E",
#         I>=.5~"I",
#         P>=.5~"P",
#         P>E~"ALI",
#         E>P~"LTP",
#         P==E & I>P~"NMI",
#         P==E & I<=P~"NMI",
#         T~"Other"))
#   
#   datalist = list()
#   
#   for (r in seq(minb, maxb, by=stepb)){
#     testsample[[perturbed_metric]] <- r
#     pred <- predict(RF_initial, newdata = testsample[, c("Class", current_metrics)])
#     predsample <- predict(RF_initial, 
#                           newdata = testsample, 
#                           type="prob")
#     predclass <- colnames(predsample)[apply(predsample, 1, which.max)]
#     predprod <- max(predsample)
#     
#     # Test results
#     test_results <- tibble(testsample) %>%
#       add_column(pred) %>%
#       rename(RF_Prediction_Majority="pred") %>%
#       bind_cols(predsample %>%as_tibble())
#     
#     comb_results <- test_results %>% mutate(
#       #Reclassify with 50% minimum probability
#       RF_Prediction_50_Perturbed=case_when(
#         E>=.5~"E",
#         I>=.5~"I",
#         P>=.5~"P",
#         P>E~"ALI",
#         E>P~"LTP",
#         P==E & I>P~"NMI",
#         P==E & I<=P~"NMI",
#         T~"Other"),
#       Metric_Perturbed = perturbed_metric,
#       Original_Value = originalvalue,
#       Original_RF_Prediction_50 = originalpredsample$RF_Prediction_50
#       
#     ) 
#     # print(comb_results$RF_Prediction_50 )
#     # datalist[[r]] <- comb_results
#     datalist <- rbind(datalist, comb_results)
#     
#   }
#   return (datalist)
# }
# 
# # Use the sensitivity_analysis_autobound function to generate fake data
# res_dif <- sensitivity_analysis_autobound(
#   perturbed_metric="DifferencesInVegetation_score",
#   nsteps=4)
# res_ban <- sensitivity_analysis_autobound(
#   perturbed_metric="BankWidthMean",
#   nsteps=20)
# res_sub <- sensitivity_analysis_autobound(
#   perturbed_metric="SubstrateSorting_score",
#   nsteps=4)
# res_rif <- sensitivity_analysis_autobound(
#   perturbed_metric="RifflePoolSeq_score",
#   nsteps=4)
# res_sed <- sensitivity_analysis_autobound(
#   perturbed_metric="SedimentOnPlantsDebris_score",
#   nsteps=4)
# # res_eph <- sensitivity_analysis_autobound(
# #   perturbed_metric="ephISAabund_PA",
# #   nsteps=2)
# res_up <- sensitivity_analysis_autobound(
#   perturbed_metric="UplandRooted_PA",
#   nsteps=2)
# res_hyd <- sensitivity_analysis_autobound(
#   perturbed_metric="hydrophytes_2",
#   nsteps=3)
# res_tot <- sensitivity_analysis_autobound(
#   perturbed_metric="TotalAbund_0_10",
#   nsteps=3)
# #Combine all the fake data generate above into a single dataframe
# data <- rbind(res_dif, res_ban, res_sub, res_rif, res_sed, #res_eph, 
#               res_up, res_hyd, res_tot)
# 
# #Loop through each metric in the fake datasets above and plot 
# for (metric in current_metrics) {#c("BankWidthMean")
#   print(metric)
#   
#   sa_data <- data%>%filter(Metric_Perturbed==metric)%>%
#     group_by(ParentGlobalID)
#   sa_data$Original_RF_Prediction_50 <- as.factor(
#     sa_data$Original_RF_Prediction_50)
#   
#   dotplot1 <- ggplot(data=sa_data,
#                      aes(x=RF_Prediction_50_Perturbed, 
#                          y=!!sym(metric),
#                          color=RF_Prediction_50_Perturbed),
#                      size=2, shape=2)+
#     geom_point()+
#     geom_point(data=sa_data,
#                aes(x=Original_RF_Prediction_50,
#                    y=Original_Value),
#                shape=4, size=4, color="black", #show.legend=TRUE
#                #legend=FALSE
#     )+
#     
#     xlab("Prediction")+
#     coord_flip()+
#     labs(title=paste(metric, "Sensitivity Analysis"),
#          subtitle="Perturb metric to see when predicted class changes"
#     )+
#     facet_wrap(~ParentGlobalID, ncol = 1)+
#     # scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
#     theme_bw() +
#     theme(legend.position="bottom")
#   print(dotplot1)
#   ggsave(dotplot1, height=11.2, width=7, units="in", dpi=900,
#          filename=paste0(sa_dir, "/",metric,".png"))
#   
# }


# ######################### Perturb input data (specify intervals) ###################
# # Inputs:
# # - perturbed_metric: Name of the variable you're going to perturb
# # - minb: Floor of your boundary for this metric
# # - maxb: Ceiling of your boundary for this metric
# # - stepb: Step size
# 
# # This function is more hands on than sensitivity_analysis_autobound() because you
# # need to fully describe how you want to create the fake data. But otherwise this
# # function is the same.
# 
# 
# sensitivity_analysis_setbound <- function(perturbed_metric, minb, maxb, stepb){
#   originalvalue <- testsample[[perturbed_metric]]
#   
#   originalpredsample <- predict(RF_initial, newdata = testsample, type="prob") %>%
#     as_tibble() %>% mutate(
#       RF_Prediction_50=case_when(
#         E>=.5~"E",
#         I>=.5~"I",
#         P>=.5~"P",
#         P>E~"ALI",
#         E>P~"LTP",
#         P==E & I>P~"NMI",
#         P==E & I<=P~"NMI",
#         T~"Other"))
#   
#   datalist = list()
#   
#   for (r in seq(minb, maxb, by=stepb)){
#     
#     testsample[[perturbed_metric]] <- r
#     pred <- predict(RF_initial, newdata = testsample[, c("Class", current_metrics)])
#     predsample <- predict(RF_initial, newdata = testsample, type="prob")
#     predclass <- colnames(predsample)[apply(predsample, 1, which.max)]
#     predprod <- max(predsample)
#     
#     # Test results
#     test_results <- tibble(testsample) %>%
#       add_column(pred) %>%
#       rename(RF_Prediction_Majority="pred") %>%
#       bind_cols(predsample %>%as_tibble())
#     
#     comb_results <- test_results %>% mutate(
#       #Reclassify with 50% minimum probability
#       RF_Prediction_50_Perturbed=case_when(
#         E>=.5~"E",
#         I>=.5~"I",
#         P>=.5~"P",
#         P>E~"ALI",
#         E>P~"LTP",
#         P==E & I>P~"NMI",
#         P==E & I<=P~"NMI",
#         T~"Other"),
#       Metric_Perturbed = perturbed_metric,
#       Original_Value = originalvalue,
#       Original_RF_Prediction_50 = originalpredsample$RF_Prediction_50
#       
#     )
#     # print(comb_results$RF_Prediction_50 )
#     # datalist[[r]] <- comb_results
#     datalist <- rbind(datalist, comb_results)
#     
#   }
#   return (datalist)
# }
# res <- sensitivity_analysis_setbound(perturbed_metric="BankWidthMean",
#                                      minb=0,
#                                      maxb=100,
#                                      stepb=20)
# res
# 
# 
# ########################### Spearman Correlation  ############################## 
# ## Calculate Spearman correlation coefficients
# cor_matrix <- cor(old_data2, method = "spearman")
# # 
# # Create a correlation heatmap with color gradients
# corrplot(
#   cor_matrix,
#   method = "color",
#   type = "lower",
#   tl.cex = 0.7,
#   tl.col = "black",
#   tl.srt = 45,
#   addCoef.col = "black",
#   main = "Spearman Correlation Heatmap"
# )
# ################################################################################ 
gp_subset <- df_refined_results[, c("ParentGlobalID",
                                    "SiteCode",
                                    "Class", 
                                    "RF_Prediction_50", 
                                    current_metrics)]
gp_subset_crossed <- crossing(Change = factor(c(
  "-9 Unit", "-8 Unit", "-7 Unit", "-6 Unit", 
  "-5 Unit", "-4 Unit", "-3 Unit", "-2 Unit","-1 Unit",
  "0", "+1 Unit", "+2 Unit", "+3 Unit","+4 Unit","+5 Unit",
  "+6 Unit","+7 Unit","+8 Unit","+9 Unit"),
  levels = c("-9 Unit", "-8 Unit", "-7 Unit", "-6 Unit", 
   "-5 Unit", "-4 Unit", "-3 Unit", "-2 Unit","-1 Unit",
  "0", "+1 Unit", "+2 Unit", "+3 Unit","+4 Unit","+5 Unit",
  "+6 Unit","+7 Unit","+8 Unit","+9 Unit")),
  Indicator=current_metrics) %>%
  crossing(gp_subset) %>%
  mutate(
    DifferencesInVegetation_score = 
       case_when(Indicator!="DifferencesInVegetation_score"~DifferencesInVegetation_score,
                 Change=="-5 Unit"~DifferencesInVegetation_score-2.5,
                 Change=="-4 Unit"~DifferencesInVegetation_score-2,
                 Change=="-3 Unit"~DifferencesInVegetation_score-1.5,
                 Change=="-2 Unit"~DifferencesInVegetation_score-1,
                 Change=="-1 Unit"~DifferencesInVegetation_score-0.5,
                 Change=="0"~DifferencesInVegetation_score,
                 Change=="+1 Unit"~DifferencesInVegetation_score+0.5,
                 Change=="+2 Unit"~DifferencesInVegetation_score+1,
                 Change=="+3 Unit"~DifferencesInVegetation_score+1.5,
                 Change=="+4 Unit"~DifferencesInVegetation_score+2,
                 Change=="+5 Unit"~DifferencesInVegetation_score+2.5,
                 T~NA_real_),
    RifflePoolSeq_score = 
       case_when(Indicator!="RifflePoolSeq_score"~RifflePoolSeq_score,
                 Change=="-5 Unit"~RifflePoolSeq_score-2.5,
                 Change=="-4 Unit"~RifflePoolSeq_score-2,
                 Change=="-3 Unit"~RifflePoolSeq_score-1.5,
                 Change=="-2 Unit"~RifflePoolSeq_score-1,
                 Change=="-1 Unit"~RifflePoolSeq_score-0.5,
                 Change=="0"~RifflePoolSeq_score,
                 Change=="+1 Unit"~RifflePoolSeq_score+0.5,
                 Change=="+2 Unit"~RifflePoolSeq_score+1,
                 Change=="+3 Unit"~RifflePoolSeq_score+1.5,
                 Change=="+4 Unit"~RifflePoolSeq_score+2,
                 Change=="+5 Unit"~RifflePoolSeq_score+2.5,
                 T~NA_real_),
    SubstrateSorting_score = 
      case_when(Indicator!="SubstrateSorting_score"~SubstrateSorting_score,
                Change=="-5 Unit"~SubstrateSorting_score-2.5,
                Change=="-4 Unit"~SubstrateSorting_score-2,
                Change=="-3 Unit"~SubstrateSorting_score-1.5,
                Change=="-2 Unit"~SubstrateSorting_score-1,
                Change=="-1 Unit"~SubstrateSorting_score-0.5,
                Change=="0"~SubstrateSorting_score,
                Change=="+1 Unit"~SubstrateSorting_score+0.5,
                Change=="+2 Unit"~SubstrateSorting_score+1,
                Change=="+3 Unit"~SubstrateSorting_score+1.5,
                Change=="+4 Unit"~SubstrateSorting_score+2,
                Change=="+5 Unit"~SubstrateSorting_score+2.5,
                T~NA_real_),
    SedimentOnPlantsDebris_score = 
      case_when(Indicator!="SedimentOnPlantsDebris_score"~SedimentOnPlantsDebris_score,
                Change=="-5 Unit"~SedimentOnPlantsDebris_score-1.25,
                Change=="-4 Unit"~SedimentOnPlantsDebris_score-1,
                Change=="-3 Unit"~SedimentOnPlantsDebris_score-0.75,
                Change=="-2 Unit"~SedimentOnPlantsDebris_score-0.5,
                Change=="-1 Unit"~SedimentOnPlantsDebris_score-0.25,
                Change=="0"~SedimentOnPlantsDebris_score,
                Change=="+1 Unit"~SedimentOnPlantsDebris_score+0.25,
                Change=="+2 Unit"~SedimentOnPlantsDebris_score+0.5,
                Change=="+3 Unit"~SedimentOnPlantsDebris_score+0.75,
                Change=="+4 Unit"~SedimentOnPlantsDebris_score+1,
                Change=="+5 Unit"~SedimentOnPlantsDebris_score+1.25,
                T~NA_real_),
    UplandRooted_PA = 
      case_when(Indicator!="UplandRooted_PA"~UplandRooted_PA,
                Change=="-1 Unit"~UplandRooted_PA-1,
                Change=="0"~UplandRooted_PA,
                Change=="+1 Unit"~UplandRooted_PA+1,
                T~NA_real_),
    hydrophytes_2 = 
      case_when(Indicator!="hydrophytes_2"~hydrophytes_2,
                Change=="-1 Unit"~hydrophytes_2-1,
                Change=="0"~hydrophytes_2,
                Change=="+1 Unit"~hydrophytes_2+1,
                T~NA_real_),
    TotalAbund_0_10 = 
      case_when(Indicator!="TotalAbund_0_10"~TotalAbund_0_10,
                Change=="-2 Unit"~TotalAbund_0_10-2,
                Change=="-1 Unit"~TotalAbund_0_10-1,
                Change=="0"~TotalAbund_0_10,
                Change=="+1 Unit"~TotalAbund_0_10+1,
                Change=="+2 Unit"~TotalAbund_0_10+2,
                T~NA_real_),
    BankWidthMean = 
      case_when(Indicator!="BankWidthMean"~BankWidthMean,
                Change=="-9 Unit"~BankWidthMean*.1,
                Change=="-8 Unit"~BankWidthMean*.2,
                Change=="-7 Unit"~BankWidthMean*.3,
                Change=="-6 Unit"~BankWidthMean*.4,
                Change=="-5 Unit"~BankWidthMean*.5,
                Change=="-4 Unit"~BankWidthMean*.6,
                Change=="-3 Unit"~BankWidthMean*.7,
                Change=="-2 Unit"~BankWidthMean*.8,
                Change=="-1 Unit"~BankWidthMean*.9,
                Change=="0"~BankWidthMean,
                Change=="+1 Unit"~BankWidthMean*1.1,
                Change=="+2 Unit"~BankWidthMean*1.2,
                Change=="+3 Unit"~BankWidthMean*1.3,
                Change=="+4 Unit"~BankWidthMean*1.4,
                Change=="+5 Unit"~BankWidthMean*1.5,
                Change=="+6 Unit"~BankWidthMean*1.6,
                Change=="+7 Unit"~BankWidthMean*1.7,
                Change=="+8 Unit"~BankWidthMean*1.8,
                Change=="+9 Unit"~BankWidthMean*1.9,
                T~NA_real_)
  ) %>%
  #Drop impossible values
  filter(DifferencesInVegetation_score>=0, DifferencesInVegetation_score<=3,
         RifflePoolSeq_score>=0, RifflePoolSeq_score<=3,
         SubstrateSorting_score>=0, SubstrateSorting_score<=3,
         SedimentOnPlantsDebris_score>=0, SedimentOnPlantsDebris_score<=1.5,
         UplandRooted_PA>=0, UplandRooted_PA<=1,
         hydrophytes_2>=0, hydrophytes_2<=1,
         TotalAbund_0_10>=0, TotalAbund_0_10<=2,
         BankWidthMean>=0, BankWidthMean<=max(gp_subset$BankWidthMean)
  ) %>%
  na.omit()


gp_subset_crossed2 <- gp_subset_crossed %>%
  bind_cols(
    predict(RF_initial, newdata = gp_subset_crossed, type="prob") %>%
      as_tibble() %>%
      mutate( Class_predicted=case_when(E>=.5~"E",
                                        I>=.5~"I",
                                        P>=.5~"P",
                                        P>E~"ALI",
                                        E>P~"LTP",
                                        P==E & I>P~"NMI", 
                                        P==E & I<=P~"NMI",
                                        T~"Other"))
  ) %>%
  mutate(SamePrediction=RF_Prediction_50==Class_predicted)


sensitivity_plot <- gp_subset_crossed2 %>%
  # filter(Indicator %in% c('DifferencesInVegetation_score')) %>%
  group_by(Indicator, Change) %>%
  summarise(n_tests=length(SamePrediction),
            n_unchanged=sum(SamePrediction)) %>%
  ungroup() %>%
  mutate(PctUnchanged=n_unchanged/n_tests,
         ChangeDir = case_when(
           Change %in% c("-5 Unit","-4 Unit","-3 Unit","-2 Unit","-1 Unit")~"Decreased",
           Change %in% c("+1 Unit","+2 Unit","+3 Unit","+4 Unit","+5 Unit")~"Increased",
           Change %in% c("0")~"Unchanged",
           T~"OTHER"),
        
         Indicator_label = case_when(
           Indicator=="BankWidthMean"~"Bank width",
           Indicator=="SedimentOnPlantsDebris_score"~"Sediment On Plants Debris score",
           Indicator=="UplandRooted_PA"~"UplandRooted_PA",
           Indicator=="hydrophytes_2"~"hydrophytes_2",
           Indicator=="TotalAbund_0_10"~"TotalAbund_0_10",
           Indicator=="SubstrateSorting_score"~"Substrate sorting score",
           Indicator=="DifferencesInVegetation_score"~"Differences in vegetation",
           Indicator=="UplandRootedPlants_score"~"Upland rooted plants",
           T~"OTHER"))


for (metric in c("SubstrateSorting_score", #0.00 1.50 3.00 2.25 0.75
                 "DifferencesInVegetation_score", #0.0 2.0 1.0 2.5 3.0 1.5 0.5
                 "RifflePoolSeq_score"#, #0.0 2.0 3.0 1.0 0.5 1.5 2.5
                 )) {
  print(metric)
  
  sensitivity_plot2 <- sensitivity_plot %>%
    filter(Indicator==metric)
    
  sa_plot <- ggplot(data=sensitivity_plot2,  
    aes(x=Change, y=PctUnchanged))+
    geom_point(aes(color=PctUnchanged==1,
                   size=n_tests))+
    scale_color_brewer(palette="Set1", name="Did any\nclassifications\nchange?", 
                       labels=c("Yes","No"))+
    xlab(paste0("\nChange in indicator",metric,"\n(Unit=0.5)"))+
    scale_size_continuous(name="# samples\nevaluated")+
    scale_y_continuous(limits=c(0,1))+
    ylab("% of samples with unchanged classifications\n")+
    ggtitle(paste0(metric, " Sensitivity Analysis"))
  print(sa_plot)
  ggsave(sa_plot, 
       filename=paste0(sa_dir, "/sensitivity_plot_",metric,".png"), 
       height=6, width=8 )

}

for (metric in c(
  "SedimentOnPlantsDebris_score" #1.00 0.00 1.25 0.50 1.50 0.25 0.75
)) {
  print(metric)
  
  sensitivity_plot2 <- sensitivity_plot %>%
    filter(Indicator==metric)
  
  sa_plot <- ggplot(data=sensitivity_plot2,  
                    aes(x=Change, y=PctUnchanged))+
    geom_point(aes(color=PctUnchanged==1,
                   size=n_tests))+
    scale_color_brewer(palette="Set1", name="Did any\nclassifications\nchange?", 
                       labels=c("Yes","No"))+
    xlab(paste0("\nChange in indicator",metric,"\n(Unit=0.25)"))+
    scale_size_continuous(name="# samples\nevaluated")+
    scale_y_continuous(limits=c(0,1))+
    ylab("% of samples with unchanged classifications\n")+
    ggtitle(paste0(metric, " Sensitivity Analysis"))
  print(sa_plot)
  ggsave(sa_plot, 
         filename=paste0(sa_dir, "/sensitivity_plot_",metric,".png"), 
         height=6, width=8 )
  
}


for (metric in c(
  "UplandRooted_PA",
  "hydrophytes_2",
  "TotalAbund_0_10" 
)) {
  sensitivity_plot2 <- sensitivity_plot %>%
    filter(Indicator==metric)
  
  sa_plot <- ggplot(data=sensitivity_plot2,  
                    aes(x=Change, y=PctUnchanged))+
    geom_point(aes(color=PctUnchanged==1,
                   size=n_tests))+
    scale_color_brewer(palette="Set1", name="Did any\nclassifications\nchange?", 
                       labels=c("Yes","No"))+
    xlab(paste0("\nChange in indicator",metric,"\n(Unit=1)"))+
    scale_size_continuous(name="# samples\nevaluated")+
    scale_y_continuous(limits=c(0,1))+
    ylab("% of samples with unchanged classifications\n")+
    ggtitle(paste0(metric, " Sensitivity Analysis"))
  print(sa_plot)
  ggsave(sa_plot, 
         filename=paste0(sa_dir, "/sensitivity_plot_",metric,".png"), 
         height=6, width=8 )
  
}

for (metric in c("BankWidthMean")) {
  sensitivity_plot2 <- sensitivity_plot %>%
    filter(Indicator==metric)
  
  sa_plot <- ggplot(data=sensitivity_plot2,  
                    aes(x=Change, y=PctUnchanged))+
    geom_point(aes(color=PctUnchanged==1,
                   size=n_tests))+
    scale_color_brewer(palette="Set1", name="Did any\nclassifications\nchange?", 
                       labels=c("Yes","No"))+
    xlab(paste0("\nChange in indicator",metric,"\n(Unit=1)"))+
    scale_size_continuous(name="# samples\nevaluated")+
    scale_y_continuous(limits=c(0,1))+
    ylab("% of samples with unchanged classifications\n")+
    ggtitle(paste0(metric, " Sensitivity Analysis"))
  print(sa_plot)
  ggsave(sa_plot, 
         filename=paste0(sa_dir, "/sensitivity_plot_",metric,".png"), 
         height=6, width=10 )
  
}







############################ Agg perturb figure ################################
gp_subset <- df_refined_results[, c("ParentGlobalID",
                                     "SiteCode",
                                     "Class", 
                                     "RF_Prediction_50", 
                                     current_metrics)]
gp_subset_crossed <- crossing(Change = factor(c(
            "Dec10", "Dec09", "Dec08", "Dec07","Dec06",
            "Dec05", "Dec04", "Dec03", "Dec02","Dec01",
            "Unchanged",
            "Inc01","Inc02","Inc03","Inc04","Inc05",
            "Inc06","Inc07","Inc08","Inc09","Inc10"),
  levels = c("Dec10", "Dec09", "Dec08", "Dec07","Dec06",
             "Dec05", "Dec04", "Dec03", "Dec02","Dec01",
             "Unchanged",
             "Inc01","Inc02","Inc03","Inc04","Inc05",
             "Inc06","Inc07","Inc08","Inc09","Inc10")),
  Indicator=current_metrics) %>%
  crossing(gp_subset) %>%
  mutate(DifferencesInVegetation_score = 
           case_when(Indicator!="DifferencesInVegetation_score"~DifferencesInVegetation_score,
                     Change=="Dec05"~DifferencesInVegetation_score-2.5,
                     Change=="Dec04"~DifferencesInVegetation_score-2,
                     Change=="Dec03"~DifferencesInVegetation_score-1.5,
                     Change=="Dec02"~DifferencesInVegetation_score-1,
                     Change=="Dec01"~DifferencesInVegetation_score-0.5,
                     Change=="Unchanged"~DifferencesInVegetation_score,
                     Change=="Inc01"~DifferencesInVegetation_score+.5,
                     Change=="Inc02"~DifferencesInVegetation_score+1,
                     Change=="Inc03"~DifferencesInVegetation_score+1.5,
                     Change=="Inc04"~DifferencesInVegetation_score+2,
                     Change=="Inc05"~DifferencesInVegetation_score+2.5,
                     T~NA_real_),
         
         SubstrateSorting_score = 
           case_when(Indicator!="SubstrateSorting_score"~SubstrateSorting_score,
                     Change=="Dec05"~SubstrateSorting_score-2.5,
                     Change=="Dec04"~SubstrateSorting_score-2,
                     Change=="Dec03"~SubstrateSorting_score-1.5,
                     Change=="Dec02"~SubstrateSorting_score-1,
                     Change=="Dec01"~SubstrateSorting_score-0.5,
                     Change=="Unchanged"~SubstrateSorting_score,
                     Change=="Inc01"~SubstrateSorting_score+.5,
                     Change=="Inc02"~SubstrateSorting_score+1,
                     Change=="Inc03"~SubstrateSorting_score+1.5,
                     Change=="Inc04"~SubstrateSorting_score+2,
                     Change=="Inc05"~SubstrateSorting_score+2.5,
                     T~NA_real_),
         
         SedimentOnPlantsDebris_score = 
           case_when(Indicator!="SedimentOnPlantsDebris_score"~SedimentOnPlantsDebris_score,
                     Change=="Dec06"~SedimentOnPlantsDebris_score-1.5,
                     Change=="Dec05"~SedimentOnPlantsDebris_score-1.25,
                     Change=="Dec04"~SedimentOnPlantsDebris_score-1,
                     Change=="Dec03"~SedimentOnPlantsDebris_score-0.75,
                     Change=="Dec02"~SedimentOnPlantsDebris_score-0.5,
                     Change=="Dec01"~SedimentOnPlantsDebris_score-0.25,
                     Change=="Unchanged"~SedimentOnPlantsDebris_score,
                     Change=="Inc01"~SedimentOnPlantsDebris_score+0.25,
                     Change=="Inc02"~SedimentOnPlantsDebris_score+0.5,
                     Change=="Inc03"~SedimentOnPlantsDebris_score+0.75,
                     Change=="Inc04"~SedimentOnPlantsDebris_score+1,
                     Change=="Inc05"~SedimentOnPlantsDebris_score+1.25,
                     Change=="Inc06"~SedimentOnPlantsDebris_score+1.5,
                     T~NA_real_),
         # AlgalCover_LiveOrDead_NoUpstream = 
         #   case_when(Indicator!="AlgalCover_LiveOrDead_NoUpstream"~AlgalCover_LiveOrDead_NoUpstream,
         #             Change=="Dec05"~AlgalCover_LiveOrDead_NoUpstream-5,
         #             Change=="Dec04"~AlgalCover_LiveOrDead_NoUpstream-4,
         #             Change=="Dec03"~AlgalCover_LiveOrDead_NoUpstream-3,
         #             Change=="Dec02"~AlgalCover_LiveOrDead_NoUpstream-2,
         #             Change=="Dec01"~AlgalCover_LiveOrDead_NoUpstream-1,
         #             Change=="Unchanged"~AlgalCover_LiveOrDead_NoUpstream,
         #             Change=="Inc01"~AlgalCover_LiveOrDead_NoUpstream+1,
         #             Change=="Inc02"~AlgalCover_LiveOrDead_NoUpstream+2,
         #             Change=="Inc03"~AlgalCover_LiveOrDead_NoUpstream+3,
         #             Change=="Inc04"~AlgalCover_LiveOrDead_NoUpstream+4,
         #             Change=="Inc05"~AlgalCover_LiveOrDead_NoUpstream+5,
         #             T~NA_real_),
         BankWidthMean = 
           case_when(Indicator!="BankWidthMean"~BankWidthMean,
                     
                     Change=="Dec09"~BankWidthMean*.1,
                     Change=="Dec08"~BankWidthMean*.2,
                     Change=="Dec07"~BankWidthMean*.3,
                     Change=="Dec06"~BankWidthMean*.4,
                     Change=="Dec05"~BankWidthMean*.5,
                     Change=="Dec04"~BankWidthMean*.6,
                     Change=="Dec03"~BankWidthMean*.7,
                     Change=="Dec02"~BankWidthMean*.8,
                     Change=="Dec01"~BankWidthMean*.9,
                     Change=="Unchanged"~BankWidthMean,
                     Change=="Inc01"~BankWidthMean*1.1,
                     Change=="Inc02"~BankWidthMean*1.2,
                     Change=="Inc03"~BankWidthMean*1.3,
                     Change=="Inc04"~BankWidthMean*1.4,
                     Change=="Inc05"~BankWidthMean*1.6,
                     Change=="Inc06"~BankWidthMean*1.7,
                     Change=="Inc07"~BankWidthMean*1.8,
                     Change=="Inc08"~BankWidthMean*1.9,
                     Change=="Inc09"~BankWidthMean*2,
                     T~NA_real_),
         UplandRooted_PA =
           case_when(Indicator!="UplandRooted_PA"~UplandRooted_PA,
                     Change=="Dec01"~UplandRooted_PA-1,
                     Change=="Unchanged"~UplandRooted_PA,
                     Change=="Inc01"~UplandRooted_PA+1,
                     T~NA_real_),
         hydrophytes_2 =
           case_when(Indicator!="hydrophytes_2"~hydrophytes_2,
                     Change=="Dec01"~hydrophytes_2-1,
                     Change=="Unchanged"~hydrophytes_2,
                     Change=="Inc01"~hydrophytes_2+1,
                     T~NA_real_),
         RifflePoolSeq_score = 
           case_when(Indicator!="RifflePoolSeq_score"~RifflePoolSeq_score,
                     Change=="Dec05"~RifflePoolSeq_score-2.5,
                     Change=="Dec04"~RifflePoolSeq_score-2,
                     Change=="Dec03"~RifflePoolSeq_score-1.5,
                     Change=="Dec02"~RifflePoolSeq_score-1,
                     Change=="Dec01"~RifflePoolSeq_score-0.5,
                     Change=="Unchanged"~RifflePoolSeq_score,
                     Change=="Inc01"~RifflePoolSeq_score+.5,
                     Change=="Inc02"~RifflePoolSeq_score+1,
                     Change=="Inc03"~RifflePoolSeq_score+1.5,
                     Change=="Inc04"~RifflePoolSeq_score+2,
                     Change=="Inc05"~RifflePoolSeq_score+2.5,
                     
                     T~NA_real_),
    
         TotalAbund_0_10 =
           case_when(Indicator!="TotalAbund_0_10"~TotalAbund_0_10,
                     Change=="Dec02"~TotalAbund_0_10-2,
                     Change=="Dec01"~TotalAbund_0_10-1,
                     Change=="Unchanged"~TotalAbund_0_10,
                     Change=="Inc01"~TotalAbund_0_10+1,
                     Change=="Inc02"~TotalAbund_0_10+2,
                     T~NA_real_)
  ) %>%
  #Drop impossible values
  filter(DifferencesInVegetation_score>=0, DifferencesInVegetation_score<=3,
         SubstrateSorting_score>=0, SubstrateSorting_score<=3,
         SedimentOnPlantsDebris_score>=0, SedimentOnPlantsDebris_score<=1.5,
         UplandRooted_PA>=0, UplandRooted_PA<=1,
         hydrophytes_2>=0, hydrophytes_2<=1,
         TotalAbund_0_10>=0, TotalAbund_0_10<=2,
         RifflePoolSeq_score>=0, RifflePoolSeq_score<=3
  ) %>%
  na.omit()


gp_subset_crossed %>% filter(Change=="Inc06")

gp_subset_crossed2 <- gp_subset_crossed %>%
  bind_cols(
    predict(RF_initial, newdata = gp_subset_crossed, type="prob") %>%
      as_tibble() %>%
      mutate( Class_predicted=case_when(E>=.5~"E",
                                        I>=.5~"I",
                                        P>=.5~"P",
                                        P>E~"ALI",
                                        E>P~"LTP",
                                        P==E & I>P~"NMI", 
                                        P==E & I<=P~"NMI",
                                        T~"Other"))
  ) %>%
  mutate(SamePrediction=RF_Prediction_50==Class_predicted)


sensitivity_plot <- gp_subset_crossed2 %>%
  group_by(Indicator, Change) %>%
  summarise(n_tests=length(SamePrediction),
            n_unchanged=sum(SamePrediction)) %>%
  ungroup() %>%
  mutate(PctUnchanged=n_unchanged/n_tests,
         ChangeDir = case_when(str_sub(Change,1,3)=="Dec"~"Decreased",
                               str_sub(Change,1,3)=="Inc"~"Increased",
                               str_sub(Change,1,3)=="Unc"~"Unchanged",
                               T~"OTHER"),
         Increment = 0.5,
         VarGroup = case_when(Indicator %in% c("BankWidthMean")~"Continuous",
                              Indicator %in% c("DifferencesInVegetation_score",
                                               # "SedimentOnPlantsDebris_score",
                                               "SubstrateSorting_score",
                                               "RifflePoolSeq_score")~"Ord_0_3",
                              Indicator %in% c("hydrophytes_2",
                                               "UplandRooted_PA")~"PA",
                              Indicator %in% c("TotalAbund_0_10",
                                               "SedimentOnPlantsDebris_score",
                                               "perennial_ISAsubregion_abundance_simp2_0_5_10_20")~"Ord_0_5",
                              T~"OTHER")
         ,
         Increment = case_when(VarGroup =="Continuous" & ChangeDir=="Decreased"~ 1-((str_sub(Change, 5,5) %>% as.numeric())/10),
                               VarGroup =="Continuous" & ChangeDir=="Unchanged"~ 1,
                               VarGroup =="Continuous" & ChangeDir=="Increased" & str_sub(Change, 4,5)=="10"~2,
                               VarGroup =="Continuous" & ChangeDir=="Increased"~ 1+((str_sub(Change, 5,5) %>% as.numeric())/10),
                               VarGroup =="Ord_0_5" & ChangeDir=="Decreased"~ -(str_sub(Change, 5,5) %>% as.numeric()),
                               VarGroup =="Ord_0_5" & ChangeDir=="Unchanged"~ 0,
                               VarGroup =="Ord_0_5" & ChangeDir=="Increased"~ str_sub(Change, 5,5) %>% as.numeric(),
                               
                               VarGroup =="Ord_0_3" & ChangeDir=="Decreased"~ -(str_sub(Change, 5,5) %>% as.numeric())/2,
                               VarGroup =="Ord_0_3" & ChangeDir=="Unchanged"~ 0,
                               VarGroup =="Ord_0_3" & ChangeDir=="Increased"~ (str_sub(Change, 5,5) %>% as.numeric())/2,
                               T~-99),
         Increment_label=case_when(VarGroup=="Continuous"~paste0(Increment),
                                   VarGroup %in% c("Ord_0_3","Ord_0_5") & ChangeDir=="Decreased"~paste0(Increment),
                                   VarGroup %in% c("Ord_0_3","Ord_0_5") & ChangeDir=="Increased"~paste0("+", Increment),
                                   VarGroup %in% c("Ord_0_3","Ord_0_5") & ChangeDir=="Unchanged"~"0",
                                   T~"Other"),
         Indicator_label = case_when(Indicator=="AlgalCover_LiveOrDead_NoUpstream"~"Algal cover",
                                     Indicator=="BankWidthMean"~"Bank width",
                                     Indicator=="SedimentOnPlantsDebris_score"~"Sediment On Plants Debris score",
                                     Indicator=="UplandRooted_PA"~"UplandRooted_PA",
                                     Indicator=="hydrophytes_2"~"hydrophytes_2",
                                     Indicator=="TotalAbund_0_10"~"TotalAbund_0_10",
                                     
                                     Indicator=="SubstrateSorting_score"~"Substrate sorting score",
                                     Indicator=="DifferencesInVegetation_score"~"Differences in vegetation",
                                     Indicator=="RifflePoolSeq_score"~"Riffle/pool sequence",
                                     Indicator=="Slope"~"Slope",
                                     Indicator=="UplandRootedPlants_score"~"Upland rooted plants",
                                     Indicator=="hydrophytes_present_0_1_2_3_4_5"~"Hydrophytes",
                                     Indicator=="perennial_ISAsubregion_abundance_simp2_0_5_10_20"~"Perennial indicator taxa",
                                     T~"OTHER"))

sensitivity_plot$Increment_label<-factor(sensitivity_plot$Increment_label,
                                                levels=c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8","0.9", 
                                                         "1", 
                                                         "1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "1.9", "2",
                                                         "-5","-4","-3","-2","-1",
                                                         "0",
                                                         "+1","+2","+3","+4","+5"))

sensitivity_plot_continuous<-ggplot(data=sensitivity_plot %>% 
  filter(Indicator=="DifferencesInVegetation_score"), 
  aes(x=Increment, y=PctUnchanged))+
  geom_point(aes(color=PctUnchanged==1,
                 size=n_tests))+
  facet_wrap(~Indicator_label)+
  scale_color_brewer(palette="Set1", 
                     name="Did any\nclassifications\nchange?", 
                     labels=c("Yes","No"))+
  xlab("Change in indicator level (factor)") +
  scale_size_continuous(name="# samples\nevaluated")+
  scale_y_continuous(limits=c(0,1))+
  # scale_x_discrete(labels=c("-5","-4","-3","-2","-1",
  #                           "0",
  #                           "+1","+2","+3","+4","+5"))+
  ylab("% of samples with unchanged classifications")+
  ggtitle("Continuous indicators")
print(sensitivity_plot_continuous)
ggsave(sensitivity_plot_continuous, 
       filename=paste0(sa_dir, "/sensitivity_plot_continuous.png"), 
       height=6, width=8 )


sensitivity_plot_ordinal<-ggplot(data=sensitivity_plot %>%
                                      filter(VarGroup!="Continuous")%>%
                                   filter(VarGroup!="PA")%>%
                                   filter(VarGroup!="OTHER"),  
                                 aes(x=Increment, y=PctUnchanged))+
  geom_point(aes(color=PctUnchanged==1,
                 size=n_tests))+
  facet_wrap(~Indicator_label)+
  scale_color_brewer(palette="Set1", name="Did any\nclassifications\nchange?", labels=c("Yes","No"))+
  xlab("Change in indicator level (score/count)") +
  scale_size_continuous(name="# samples\nevaluated")+
  scale_y_continuous(limits=c(0,1))+
  # scale_x_discrete(labels=c("-5","-4","-3","-2","-1",
  #                           "0",
  #                           "+1","+2","+3","+4","+5"))+
  ylab("% of samples with unchanged classifications")+
  ggtitle("Scored or count indicators")
print(sensitivity_plot_ordinal)
ggsave(sensitivity_plot_ordinal, 
       filename=paste0(sa_dir, "/sensitivity_plot_ordinal.png"), 
       height=6, width=12 )



################################################################################ 




################# Variable Importance; Partial Dependance Plots ################  
library(pdp)
# Separate datasets
df_MODEL <- df_input %>% filter(Dataset=="Training")
df_TEST <- df_input %>% filter(Dataset=="Testing")


old_data <- df_MODEL[, c("Class", current_metrics)]
set.seed(1111)
RF <- randomForest(Class~.,
                   data=old_data,
                   ntree=numTrees,
                   importance=T,
                   proximity=T)

varimpplot <- varImpPlot(RF, 
                         type=1, 
                         scale=FALSE,
                         main = paste(CURRENT_REGION_DISPLAY,"\nFeature Importance")
                         )
# type: either 1 or 2, specifying the type of importance measure (1=mean decrease in accuracy, 2=mean decrease in node impurity).
# class: for classification problem, which class-specific measure to return.
# scale: For permutation based measures, should the measures be divided their “standard errors”?



# # install.packages("permimp")
# library("permimp")
# ## compute permutation importance
# set.seed(1111)
# RF <- randomForest(Class~.,
#                    data=old_data,
#                    ntree=numTrees,
#                    importance=T,
#                    proximity=T)
# 
# PI_permimp500 <- permimp(RF, progressBar = TRUE, asParty = TRUE)
# ## barplot
# plot(PI_permimp500, 
#      type = "bar", 
#      interval = "quantile",
#      main = paste(CURRENT_REGION_DISPLAY,"\nConditional Permutation Importance")
# )
# ## horizontal boxplot
# plot(PI_permimp500, 
#      type = "box", 
#      horizontal = TRUE,
#      main = paste(CURRENT_REGION_DISPLAY,"\nConditional Permutation Importance")
# )
# 
# 
# partialdatap <- partial(RF, 
#                        pred.var = c(metric), 
#                        type= "classification",
#                        which.class="P",
#                        chull = TRUE)
# partialdatae <- partial(RF, 
#                         pred.var = c(metric), 
#                         type= "classification",
#                         which.class="E",
#                         chull = TRUE)
# partialdatai <- partial(RF, 
#                         pred.var = c(metric), 
#                         type= "classification",
#                         which.class="I",
#                         chull = TRUE)
# 
# ggplot(data=partialdatae, aes(x=DifferencesInVegetation_score, y=yhat))+
#   geom_line(aes(color="E")) + 
#   geom_line(data=partialdatap, aes(x=DifferencesInVegetation_score, 
#                                    y=yhat, 
#                               color="P"))+ 
#   geom_rug(data=partialdatap, aes(color = "P"), 
#            sides = 'b', outside = F, alpha = 1/2, position = "jitter")+
#   geom_rug(data=partialdatai, aes(color = "E"), 
#            sides = 'b', outside=F, alpha = 1/2, position = "jitter")
# 
# 
# partialplot <- autoplot(partialdata,
#                         contour = TRUE,
#                         # pdp.size = 0.1,
#                         rug=TRUE, train = old_data)
# print(partialplot)

# for (metric in current_metrics) {
for ( metric in c( 
                 "BankWidthMean",
                 "SubstrateSorting_score",
                 "DifferencesInVegetation_score",
                 "RifflePoolSeq_score",
                 "SedimentOnPlantsDebris_score",
                 # "ephISAabund_PA",
                 "UplandRooted_PA",
                 "hydrophytes_2",
                 "TotalAbund_0_10" )) {
  print(metric)

  # One var
  partialdata_e <- partial(RF, 
                         pred.var = c(metric), 
                         type ="classification",
                         which.class="E")
  partialdata_p <- partial(RF, 
                           pred.var = c(metric), 
                           type ="classification",
                           which.class="P")
  partialplot <- ggplot(data=partialdata_e, 
        aes(x=eval(parse(text = metric)),
            y=yhat
            ))+ geom_line(aes(color="E")) +
        geom_line(data=partialdata_p, aes(
            x=eval(parse(text = metric)), 
            y=yhat, 
            color="P"
            )) +
        geom_rug(data=partialdata_p, aes(color = "P"), 
                 sides = 'b', outside = F, alpha = 1/2, position = "jitter")+
        geom_rug(data=partialdata_e, aes(color = "E"), 
                 sides = 'b', outside=F, alpha = 1/2, position = "jitter") +
    labs(title=paste("Sensitivity Analysis:", metric),
         subtitle="Partial Dependence Plot",
         legend="PDP", x="")
  print(partialplot)
  ggsave(partialplot, height=5, width=8, units="in", dpi=900,
         filename=paste0(sa_dir, "/pdps/pdp_",metric,".png"))

  # # TODO: pdp only on TEST data?!
  # # Two Variables
  # var1 <- "SedimentOnPlantsDebris_score"
  # partialdata2 <- partial(RF, pred.var = c(var1, metric), chull = TRUE)
  # partialplot2 <- autoplot(partialdata2, contour = TRUE,
  #                          rug=TRUE, train = old_data,
  #                          legend.title = "Partial\ndependence")
  # partialplot2 <- partialplot2 +
  #   labs(title=paste0("Sensitivity Analysis: ", var1, ", ", metric),
  #        subtitle="Partial Dependence Plot",
  #        legend="")
  # print(partialplot2)
  # ggsave(partialplot2, height=5, width=8, units="in", dpi=900,
  #        filename=paste0(sa_dir, "/pdps/pdp2_",var1,"_and_", metric,".png"))

}
################################################################################ 


############################### BOXPLOTS ######################################
# Boxplots for different metrics 
# -per class
# -show values of each metric
# - Compare the distribution across true classes
# -Across predicted classes

results <- read_csv(paste0(parent_path,"/refined_results.csv"))
results$Class <- factor(results$Class , levels = c("P","ALI","I","LTP","E"))

for ( metric in c( 
  "BankWidthMean",
  "SubstrateSorting_score",
  "DifferencesInVegetation_score",
  "RifflePoolSeq_score",
  "SedimentOnPlantsDebris_score",
  # "ephISAabund_PA",
  "UplandRooted_PA",
  "hydrophytes_2",
  "TotalAbund_0_10"
  )) {
  print(metric)
  
  # Change box plot colors by groups
  boxplot_actualclass <-ggplot(results, aes( x=factor(Class, 
                                        level=c('E', 'I', 'P')),
                         y=eval(parse(text = metric)), 
                         fill=Class
                         )) + geom_boxplot() +
    geom_jitter(position=position_jitter(0.02)) +
    labs(#title=paste0( metric),
      subtitle="Actual Class",
      y=metric, x="")  + scale_fill_manual(
        values=c("E"="red", 
                 "ALI"="orange",
                 "I"="yellow", 
                 "LTP"="green",
                 "P"="blue"
  
        ))+ theme(legend.position = "none")
  print(boxplot_actualclass)

  boxplot_predictedlclass <- ggplot(results, aes(
                               x=factor(RF_Prediction_50, 
                                        level=c('E', 'ALI', 'I', 'LTP', 'P')),
                         y=eval(parse(text = metric)), 
                         fill=RF_Prediction_50
  )) +geom_boxplot() +
    geom_jitter(position=position_jitter(0.02)) +
    labs(#title=paste0( metric),
         subtitle="Predicted Class",
         y=metric, x="")  + scale_fill_manual(
           values=c("E"="red", 
                    "ALI"="orange",
                    "I"="yellow", 
                    "LTP"="green",
                    "P"="blue"
        
                   ))+ theme(legend.position = "none")
  print(boxplot_predictedlclass)

  ggsave(paste0(sa_dir, "/boxplots/box1_", metric,".png"),
         arrangeGrob( 
            arrangeGrob(boxplot_actualclass, boxplot_predictedlclass, ncol=2),
            nrow = 1#
            ),
         dpi=300, height=4, width=9)
}
  
  







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
#     ggsave(paste0(sa_dir, "/metric_", metric,".png"),
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
make_models <- function (input_dataset, nickname, candidate_list, numTrees) {
  # Set up output dir
  fpath <- paste0(sa_dir, "/perturb_input/",nickname)
  if (!dir.exists(fpath)){dir.create(fpath)}
  else {print(paste("Creating RF for ", nickname))}
  
  # Create logfile
  log_con <- file(paste0(fpath,"/", "my.log"), open="w")
  cat(paste("Starting: ", nickname), file = log_con, sep="\n") 
  cat(paste(nickname, "model vars:", candidate_list), 
      file=log_con, append = TRUE, sep="\n")
  
  # Separate datasets
  df_MODEL <- df_input %>% filter(Dataset=="Training")
  df_TEST <- df_input %>% filter(Dataset=="Testing")
  
  model_data <- df_MODEL[, c("Class", candidate_list)]
  
  ############################### CREATE RF ################################### 
  set.seed(1111)
  RF <- randomForest(Class~.,
                      data=model_data,
                      ntree=numTrees,
                      importance=T,
                      proximity=T)
  
  # Saving on object in RData format
  # rdata_obj <- paste0("RF_", nickname, ".rds")
  # saveRDS(RF, file = paste0(fpath, "/", rdata_obj))
  #############################################################################

  # Train results
  train_results <- tibble(df_MODEL) %>%
    add_column( RF_Prediction_Majority = RF$predicted) %>%
    bind_cols(
      predict(RF, type="prob") %>% 
        as_tibble() 
    )
  
  # define new data frame
  new_data <- df_TEST[, c("Class", candidate_list)]
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
  write_csv(comb_results, file=paste0(fpath,"/perturbed_results.csv"))
  
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
  
  write_csv(cm_pivot_table, file=paste0(fpath, "/perturb_confusion_matrix.csv"))
  
  return (comb_results)
  
}
############################## Original predictions ############################
# df <- make_models(input_dataset=df_input, 
#             candidate_list = current_metrics,
#             nickname="original_V4", 
#             numTrees=numTrees) 
############################## Reduced predictions ############################
# The following loop creates a new RF but removes one metric each time. Goal is 
# to see how that changes the confusion matrices.

# for (metric in current_metrics) {
for ( metric in c( 
    # "BankWidthMean",
    # "SubstrateSorting_score",
    "DifferencesInVegetation_score"
    # "RifflePoolSeq_score",
    # "SedimentOnPlantsDebris_score",
    # "ephISAabund_PA",
    # "UplandRooted_PA",
    # "hydrophytes_2",
    # "TotalAbund_0_10" 
    )) {
  print(metric)
  
  df_reduced <- make_models(input_dataset=df_input, 
                    candidate_list = setdiff(current_metrics, metric),
                    nickname=paste0("no_",metric), 
                    numTrees=numTrees) 
}



#Make dotplot where you iteratively drop each variable




########### Sensitivity analysis - sites with multiple visits  ################# 
# Site visits that produce multiple classification results – is there a way to see: 
# 1.	which indicator values are changing 
# 2.	which indicator value changes are driving the change in classification 
# 3.	which indicator value changes are driving errors in classification

################################################################################
df_input_multiple <- df_input %>% 
      filter(TotalVisits>1) %>% 
      filter(Notes!='Augmented') %>% 
      group_by(SiteCode)%>%
      mutate(distinct_preds = n_distinct(RF_Prediction_50)) %>%
  select(c("ParentGlobalID","SiteCode", "Class", "Region_detail","TotalVisits",
           "Dataset", current_metrics,"RF_Prediction_50","distinct_preds")) %>%
  mutate(rangeBankWidthMean = max(BankWidthMean) - min(BankWidthMean),
         rangeSubstrateSorting_score = max(SubstrateSorting_score) - min(SubstrateSorting_score),
         rangeDifferencesInVegetation_score = max(DifferencesInVegetation_score) - min(DifferencesInVegetation_score),
         rangeRifflePoolSeq_score = max(RifflePoolSeq_score) - min(RifflePoolSeq_score),
         rangeSedimentOnPlantsDebris_score = max(SedimentOnPlantsDebris_score) - min(SedimentOnPlantsDebris_score),
         # rangeephISAabund_PA = max(ephISAabund_PA) - min(ephISAabund_PA),
         rangeUplandRooted_PA = max(UplandRooted_PA) - min(UplandRooted_PA),
         rangehydrophytes_2 = max(hydrophytes_2) - min(hydrophytes_2),
         rangeTotalAbund_0_10 = max(TotalAbund_0_10) - min(TotalAbund_0_10),
         actual_numeric = case_when(Class =="E"~0, Class=="I"~1, Class=="P"~2, T~999),
         pred_numeric = case_when(RF_Prediction_50=="E"~0, 
                                  RF_Prediction_50=="LTP"~0.5, 
                                  RF_Prediction_50=="I"~1, 
                                  RF_Prediction_50=="ALI"~1.5, 
                                  RF_Prediction_50=="P"~2, 
                                  RF_Prediction_50=="NMI"~5, 
                                  T~999),
         pred_difference = pred_numeric - actual_numeric,
         pred_difference_char = case_when(
            # Class=="E" & RF_Prediction_50 =="E"~"E Correct",
            Class==RF_Prediction_50~"PvIvE Correct",
            Class=="E" & RF_Prediction_50 %in% c("LTP")~"LTP Correct",
            Class=="I" & RF_Prediction_50 %in% c("LTP")~"LTP Correct",
            Class=="I" & RF_Prediction_50 %in% c("ALI")~"ALI Correct",
            Class=="P" & RF_Prediction_50 %in% c("ALI")~"ALI Correct",
            Class=="I" & RF_Prediction_50 %in% c("E","P")~"Small Error",
            Class %in% c("E","P") & RF_Prediction_50=="I"~"Small Error",
            Class %in% c("E") & RF_Prediction_50 %in% c("P","ALI")~"Big Error",
            Class %in% c("P") & RF_Prediction_50 %in% c("E","LTP")~"Big Error",
            T~"OTHER"
           )
         
         )
df_input_multiple$pred_difference_char <- factor(
                        df_input_multiple$pred_difference_char,
                            levels=c("PvIvE Correct","LTP Correct",
                                     "ALI Correct","Small Error","Big Error"))


for (metric in current_metrics) {#c("BankWidthMean") c("TotalAbund_0_10")
  print(metric)

  # sa_data <- df_input_multiple%>%filter(Metric_Perturbed==metric)%>%group_by(ParentGlobalID)
  # sa_data$Original_RF_Prediction_50 <- as.factor(
  #   sa_data$Original_RF_Prediction_50)

  rangeplot <- ggplot(data=df_input_multiple,
                     aes(x=!!sym(metric),
                         y=pred_difference_char,#pred_difference,
                         color=pred_difference_char),
                     # size=2, shape=2
                     )+
    geom_jitter(width = 0.0005) +
    scale_color_manual("",
      values=c(
        "PvIvE Correct"="green",
        # "LTP Correct"="#b502a9",
        # "ALI Correct"="#ccbb02",
        "LTP Correct"="blue",
        "ALI Correct"="blue",

        "Small Error"="orange",
        "Big Error"="red")
    ) +

    ylab("")+
    # coord_flip()+
    labs(title=paste(metric, "Sensitivity Analysis"),
         subtitle="For sites with multiple visits, how classifications change w value",
         legend=""
    )
    # facet_wrap(~ParentGlobalID, ncol = 1)+
    # scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1), name="Performance")+
    # theme_bw()
    # theme(legend.position="bottom")
  print(rangeplot)
  ggsave(rangeplot, height=5, width=8, units="in", dpi=900,
         filename=paste0(sa_dir, "/range2_",metric,".png"))

}

for (metric in current_metrics) {#c("BankWidthMean") c("TotalAbund_0_10")
  print(metric)
  
  # sa_data <- df_input_multiple%>%filter(Metric_Perturbed==metric)%>%group_by(ParentGlobalID)
  # sa_data$Original_RF_Prediction_50 <- as.factor(
  #   sa_data$Original_RF_Prediction_50)
  
  rangeplot <- ggplot(data=df_input_multiple,
                      aes(x=!!sym(metric),
                          y=pred_difference,
                          color=pred_difference_char,
                          # size=abs(pred_difference)
                          ),
                      # size=2, shape=2
  )+
    # scale_size(guide = 'none')+
    # scale_size(range = c(1, 5))+
    geom_jitter(width = 0.005) +
    # geom_density(stat = "identity") +
    # geom_violin(alpha=0.5)+
    scale_color_manual("",
                       values=c(
                         "PvIvE Correct"="green",
                         # "LTP Correct"="#b502a9",
                         # "ALI Correct"="#ccbb02",
                         "LTP Correct"="blue",
                         "ALI Correct"="blue",
                         
                         "Small Error"="orange",
                         "Big Error"="red") 
    ) +
    # scale_x_continuous(limits=c(0,2), breaks=c(0,1,2))+
    # coord_flip()+
    ylab("")+
    labs(title=paste(metric, "Sensitivity Analysis"),
         subtitle="For sites with multiple visits, how classifications change w value",
         legend=""
    )
  print(rangeplot)
  ggsave(rangeplot, height=5, width=8, units="in", dpi=900,
         filename=paste0(sa_dir, "/range_",metric,".png"))
  
}
