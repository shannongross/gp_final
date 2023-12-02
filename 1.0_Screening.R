########################### DATA SCREENING ##################################### 
# The following section screens features using statistical tests
# If features do not meet the screening criteria, then they are 
# removed from subsequent consideration.
#
################################################################################
library(tidyverse)
library(skimr)
library(ggforce)
library(randomForest)
library(grid)
library(gridExtra)
library(caret)
library(dplyr)
library(ggplot2)
library(rstudioapi)

# If TRUE, stores auxillary spreadsheets and plots
save_all_output <- TRUE


################################# GET DATA ##############################
# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# get dataset (full dataset, non-augmented)
df_input <- read_csv(paste0(HOME_DIR,"/input/processed/df_model.csv"))

# Clean up dataset
df_input$Class <- as.factor(df_input$Class)
df_input$Region <- as.factor(df_input$Region)
df_input$HydricSoils_score <- as.numeric(df_input$HydricSoils_score) #TODO: check this

# Get GP metrics
metrics_lookup <- read_xlsx("input/raw/metrics_dictionary.xlsx",
                            sheet = "DATA_DICT") %>%
  filter(MetricSubtype!="Direct") %>%
  filter(GP_final=="TRUE") %>% #SMG TODO check this
  # filter(GP_final!="FALSE") %>%
  filter(MetricCandidate_KF=="TRUE")
metrics_lookup <- metrics_lookup[!duplicated(metrics_lookup), ]


# candidate_list <- (metrics_lookup %>% filter(GP_final=="TRUE"))$Metric
candidate_list <- (metrics_lookup %>% filter(GP_final!="FALSE"))$Metric

#Remove categorical from screening
candidate_list <- setdiff(c(candidate_list), "Strata")

print(paste0("There are ", length(candidate_list)," candidate metrics"))

bio_list <- (metrics_lookup %>% 
    filter(GP_final=="TRUE") %>%
    filter(MetricType %in% c("Bio","Biology")))$Metric #SHOULD ONLY CONTAIN BIO
gis_list <- (metrics_lookup %>% 
    filter(GP_final=="TRUE") %>%
    filter(MetricType%in% c("Geospatial")))$Metric
geomorph_list <- (metrics_lookup %>% 
    filter(GP_final=="TRUE") %>%
    filter(MetricType%in% c("Geomorph")))$Metric
h20_indirect_list <- (metrics_lookup %>% 
    filter(GP_final=="TRUE") %>%
    filter(MetricType%in% c("Hydro")))$Metric

# Check that all candidates have been captured
print(length(candidate_list) == (sum(length(bio_list),length(gis_list),
    length(geomorph_list), length(h20_indirect_list))))

# identifying columns
info_list <- c("ParentGlobalID","CollectionDate","Region",
               "Region_detail","SiteCode","Class","Wet")

# # Separate datasets
# df_test <- df_input %>% filter(Dataset=="Testing")
# df_train <- df_input %>% filter(Dataset=="Training" & Notes=="Original")

# Create df to perform metric screening on (based on training data)
df_screen <- df_input[, c(info_list, candidate_list)]

### ARE THERE ANY NaNs?
df_NaNs_inspect <- df_screen[rowSums(is.na(df_screen)) > 0,]
df_screen <- df_screen %>% na.omit()


############################# CREATE SUMMARY TABLE #############################
# This table will have a row for each candidate metric. It will summarize basic
# statistics about each metric, such as its max/min value, percent dominance, etc
# Which can be used to assess if candidate should proceed to next step in model
# development
################################################################################

# Create summary table of candidate metrics 
predictor_summary <- tibble(
    Predictor = candidate_list) %>%
        mutate(PredGroup = case_when(
        Predictor %in% bio_list ~ "Bio",
        Predictor %in% geomorph_list ~ "Geomorph",
        Predictor %in% h20_indirect_list ~ "H20 (Indirect)",
        Predictor %in% gis_list ~ "GIS",
        T~"error"),
    ) %>% unique()

# Summary table of candidate metrics: What percent of its values are equal to zero?
predictor_summary$PctZero <- sapply(
    predictor_summary$Predictor, function(x){
        xdf <- df_screen %>% select(met=all_of(x))
        sum(xdf$met==0)/length(xdf$met)
        })

# Summary table of candidate metrics: What percent of its values are the same?
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]}

predictor_summary$PctDom<-sapply(
    predictor_summary$Predictor, function(x){
        xdf <- df_screen %>% select(met=all_of(x))
        mode.i <- getmode(xdf$met)
        sum(xdf$met==mode.i)/length(xdf$met)
        })

# Summary table of candidate metrics: Range
predictor_summary$Range<-sapply(
    predictor_summary$Predictor, function(x){
      xdf <- df_screen %>% select(met=all_of(x))
      max(xdf$met) - min(xdf$met)})

# Summary table: Min
predictor_summary$Min<-sapply(
    predictor_summary$Predictor, function(x){
      xdf <- df_screen %>% select(met=all_of(x))
      min(xdf$met)})

# Summary table: Max
predictor_summary$Max<-sapply(
    predictor_summary$Predictor, function(x){
      xdf<-df_screen %>%select(met=all_of(x)) 
      max(xdf$met)})

# Dont proceed to t-tests if all values are the same
predictor_summary <- predictor_summary %>% mutate(
    status = case_when(PctDom==1~"FAIL", T~"PASS"))
predictor_summary_passed <- predictor_summary %>% filter(status == "PASS")
predictor_summary_failed <- predictor_summary %>% filter(status == "FAIL")

# Compare mean values at perennial versus intermittent versus ephemeral 
predictor_summary_passed$PvIvE_F<-sapply(
    predictor_summary_passed$Predictor, function(x){
      xdf <- df_screen %>% select(met=all_of(x), Class)
      myaov <- aov(met~Class, data=xdf) %>% summary()
      mystat <- myaov[[1]][1,4]
      ifelse(is.na(mystat),0,mystat)})

# Compare values at ephemeral versus non-ephemeral 
predictor_summary_passed$EvALI_t_abs<-sapply(
    predictor_summary_passed$Predictor, function(x){
      xdf <- df_screen %>% select(met=all_of(x), Class) %>%
      mutate(EvALI=case_when(Class =="E"~"E",T~"NE"))
    myttest<-t.test(met~EvALI, data=xdf)
    mystat<-myttest$statistic %>% abs()
    if(class(myttest)=="try-error")
      mystat<-0
    else
      mystat<-myttest$statistic %>% abs()
    ifelse(is.na(mystat),0,mystat) })
##SMG: todo: makes sep EvALI_t_abs and EvALI_t

# Compare values at perennial vs non-perennial 
predictor_summary_passed$PvLTP_t_abs<-sapply(
    predictor_summary_passed$Predictor, function(x){
      xdf<-df_screen %>% select(met=all_of(x), Class) %>%
      mutate(PvLTP=case_when(Class=="P"~"P",T~"NP"))
    myttest<-t.test(met~PvLTP, data=xdf)
    mystat<-myttest$statistic %>% abs()
    if(class(myttest)=="try-error")
      mystat<-0
    else
      mystat<-myttest$statistic %>% abs()
    ifelse(is.na(mystat),0,mystat) })

# Compare mean values at flowing intermittent and perennial reaches
predictor_summary_passed$PvIWet_t_abs<-sapply(
  predictor_summary_passed$Predictor, function(x){
    xdf <- df_screen %>% select(met=all_of(x), Class, Wet) %>%
      filter(Wet) %>%
      filter(Class!="E") %>%
      mutate(PvIWet=case_when(Class =="P"~"P",T~"NP"))
    myttest<-try(t.test(met~PvIWet, data=xdf))
    if(class(myttest)=="try-error")
      mystat<-0
    else
      mystat<-myttest$statistic %>% abs()
    ifelse(is.na(mystat),0,mystat) })

# Compare mean values at non-flowing intermittent and ephemeral
predictor_summary_passed$EvIdry_t_abs<-sapply(
  predictor_summary_passed$Predictor, function(x){
    xdf<-df_screen %>% select(met=all_of(x), Class, Wet) %>%
      filter(!Wet) %>%
      filter(Class!="P") %>%
      mutate(EvALI=case_when(Class =="E"~"E",T~"NE"))
    myttest<-try(t.test(met~EvALI, data=xdf))
    if(class(myttest)=="try-error")
      mystat<-0
    else
      mystat<-myttest$statistic %>% abs()
    ifelse(is.na(mystat),0,mystat) })

# Screen using importance from RF
rf_dat <- df_screen %>% select(Class, all_of(candidate_list)) %>%
    mutate_if(is.character, as.factor) #SMG: check on this I DONT THINK ITS NEEDED

set.seed(10)
rf_global <- randomForest(Class ~., data = rf_dat, 
                          ntree=1500, 
                          importance=T, 
                          proximity=T)

rf_global_importance <- rf_global$importance %>%
    as.data.frame() %>% 
    mutate(Predictor=row.names(rf_global$importance))

# PLOT IMPORTANCE ACCORDING TO GINI INDEX
imp <- varImpPlot(rf_global,
    main="RandomForest: Variables in Order of Importance")
imp <- as.data.frame(imp)
imp$name <- rownames(imp) # row names to column
rownames(imp) <- NULL
imp <-imp %>% mutate(cat = case_when(
    name %in% bio_list~"Bio",
    name %in% geomorph_list~"Geomorph",
    # name %in% h20_direct_list ~ "H20 (Direct)",
    name %in% h20_indirect_list ~ "H20 (Indirect)",
    name %in% gis_list~"GIS",
    T~"Other"))

GiniImp <- ggplot(imp, aes(x=reorder(name, MeanDecreaseGini),
    weight=MeanDecreaseGini,
    fill=as.factor(cat))) +
    geom_bar() +
    scale_fill_discrete(name="Variable Group") +
    ylab("MeanDecreaseGini") +
    xlab("Variable Name") +
    labs(title = "Variable Screening: Decreasing Gini",
    caption = "(Relative ranking of node purity, disregard absolute value)") +
    theme(axis.text.x = element_text(angle = 90))
if (save_all_output == TRUE){ggsave(GiniImp, width = 10, height = 7, dpi = 300,
    filename = "output/screening/plots/screen_varImpGini.png")}

# PLOT IMPORTANCE ACCORDING TO ACCURACY
AccImp <- ggplot(imp, aes(x=reorder(name, MeanDecreaseAccuracy),
    weight=MeanDecreaseAccuracy,
    fill=as.factor(cat))) +
    geom_bar() +
    scale_fill_discrete(name="Variable Group") +
    ylab("MeanDecreaseAccuracy") +
    xlab("Variable Name") +
    labs(title = "Variable Screening: Decreasing Accuracy",
    caption = "(Relative estimate of loss in predictive performance when variable is omitted)") +
    theme(axis.text.x = element_text(angle = 90))
if (save_all_output == TRUE){ggsave(AccImp, width = 10, height = 7, dpi = 300,
    filename = "output/screening/plots/screen_varImpAccuracy.png")}

predictor_summary_passes_fails <- merge(predictor_summary_passed, 
    predictor_summary_failed, all=TRUE)

# Add RF relative importance of metrics to summary table
predictor_summary_passed_rf <- inner_join(predictor_summary_passes_fails,
    rf_global_importance %>% select(Predictor,
      rf_MDA=MeanDecreaseAccuracy,
      rf_MDG=MeanDecreaseGini))

# Add ranking of Mean Decrease in Accuracy to summary table
predictor_summary_passed_rf$rf_MDA_rank <- rank(
    predictor_summary_passed_rf$rf_MDA, ties.method = "average")


metric_screening_plot <- predictor_summary_passed_rf %>%
    pivot_longer(cols=c(PvIvE_F, EvALI_t_abs, PvLTP_t_abs, PvIWet_t_abs, 
    EvIdry_t_abs, rf_MDA)) %>%
    mutate(name=factor(name, levels=c("PvIvE_F",
                "EvALI_t_abs",
                "PvLTP_t_abs",
                "EvIdry_t_abs",
                "PvIWet_t_abs",
                "rf_MDA"))) %>%
    ggplot(aes(x=value)) +
    geom_histogram(aes(fill=PredGroup))+
    facet_wrap(~name, scales="free") +
    scale_fill_brewer(palette="Set1")
#TODO COMPARE THIS AGAINT GINI, ACC PLOTS
print(metric_screening_plot)

if (save_all_output == TRUE){ggsave(metric_screening_plot, 
    width = 10, height = 7, dpi = 300,
    filename = "output/screening/plots/metric_screening_plot.png")}

#################################### SUMMARIZE ################################# 
# Summarize which metrics passed screening and which did not
#
################################################################################
summary_copy <- predictor_summary_passed_rf
metric_summary <- summary_copy %>%
    mutate(
        PassPctDom = PctDom<0.95, 
        PassPvIvE_F = PvIvE_F>quantile(summary_copy$PvIvE_F, na.rm=T, probs=.75), 
        PassEvALI_t_abs = PvIvE_F>quantile(summary_copy$EvALI_t_abs, na.rm=T, probs=.75),
        PassPvLTP_t_abs = EvALI_t_abs>quantile(summary_copy$PvLTP_t_abs, na.rm=T, probs=.75),
        PassEvIdry_t_abs = PvLTP_t_abs>quantile(summary_copy$EvIdry_t_abs, na.rm=T, probs=.75),
        PassPvIWet_t_abs = PvIWet_t_abs>quantile(summary_copy$PvIWet_t_abs, na.rm=T, probs=.75),
        Passrf_MDA = rf_MDA>quantile(summary_copy$rf_MDA, na.rm=T, probs=.75),
        PassScreens = 
            PctDom<0.95 & #Must have at least 5% variation
            ##And must be in top quartile of responsiveness measures
            (PassPvIvE_F|PassEvALI_t_abs|PassPvLTP_t_abs|PassEvIdry_t_abs|PassEvIdry_t_abs|PassPvIWet_t_abs|Passrf_MDA)
        ) #TODO: must be gt 2 instead (sim to beta)
#TODO: COMPARE PCTSHADING DATA VS BETA INPUT

# How many candidates passed screening?
metric_summary %>% group_by(PassScreens) %>% tally()
# metric_summary %>% group_by(MetricType, PassScreens) %>% tally()
metric_summary %>% filter(PassScreens)

# Save summary file
if (save_all_output == TRUE){
    write_csv(metric_summary, 
    file=paste0(HOME_DIR,"/output/screening/metric_summary.csv"))
    }
