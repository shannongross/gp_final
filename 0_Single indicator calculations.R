
#Fish without mosquitosish is not included in the metrics, so we need to bring it in again
main_df_step1<-read_csv("NotForGit/Step1/main_df_step1.csv")



# main_df2_withU %>%
#   left_join(main_df_step1 %>%
#               select(SiteCode, CollectionDate, Fish_PA_nomosq) %>%
#               unique()) #
mydf<-main_df_step1%>%
  mutate(
    SI_Fish_PA = case_when(is.na(Fish_PA)~0, Fish_PA==1~1, T~0), #This includes mosquitofish, and was a candidate metric
    SI_Fish_NoMosq_PA = case_when(is.na(Fish_PA_nomosq)~0, Fish_PA_nomosq==1~1, T~0), #This excludes mosuitosifh and was not a candidate metric
    
    SI_BMI_PA = case_when(is.na(TotalAbundance)~0, TotalAbundance>=1~1, T~0),
    SI_BMI_10 = case_when(is.na(TotalAbundance)~0, TotalAbundance>=10~1, T~0),
    SI_EPT_PA = case_when(is.na(EPT_abundance)~0, EPT_abundance>=1~1, T~0),
    SI_EPT_10 = case_when(is.na(EPT_abundance)~0, EPT_abundance>=10~1, T~0),
    
    #This is for the AW, WM and GP models
    SI_BMI_Peren_PA = case_when(is.na(perennial_ISA_abundance)~0, perennial_ISA_abundance>=1~1, T~0),
    SI_BMI_Peren_10 = case_when(is.na(perennial_ISA_abundance)~0, perennial_ISA_abundance>=1~1, T~0),
    
    #This is for the West model
    SI_BMI_PerenInt_PA = case_when(is.na(perintper_ISA_abundance)~0, perintper_ISA_abundance>=1~1, T~0),
    SI_BMI_PerenInt_10 = case_when(is.na(perintper_ISA_abundance)~0, perintper_ISA_abundance>=1~1, T~0),
    
    SI_Algae_10pct = case_when(is.na(AlgalCover_LiveOrDead_NoUpstream)~0,AlgalCover_LiveOrDead_NoUpstream>=3~1, T~0),
    SI_HydricSoil_PA = case_when(is.na(HydricSoils_score)~0,HydricSoils_score==3~1, T~0),
    SI_IOFB_PA = case_when(is.na(ironox_bfscore_NM)~0,ironox_bfscore_NM==1.5~1, T~0),
    
    SI_Hydrophytes_PA =case_when(is.na(hydrophytes_present)~0, hydrophytes_present>=1~1, T~0),
    
  )

SI_variables = c(
  # "SI_Fish_PA", #Drop: Not identified by PDT as a potential single indicator
  "SI_Fish_NoMosq_PA",
  "SI_BMI_PA",
  "SI_EPT_PA",
  "SI_EPT_10",
  "SI_BMI_Peren_PA", #Only for AW, WM, GP models
  "SI_BMI_Peren_10", #Only for AW, WM, GP models
  "SI_BMI_PerenInt_PA", #Only for West model
  "SI_BMI_PerenInt_10", #Only for West model
  "SI_Algae_10pct",
  "SI_HydricSoil_PA",
  "SI_IOFB_PA",
  "SI_Hydrophytes_PA"
  )
