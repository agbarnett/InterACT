# 99_functions.R
# helpful functions for InterACT
# July 2020

# rename for SPICT variables
nice.rename.spict = function(x){
y = case_when(
  x == 'spict_unplan_hosp' ~ 'Unplanned hospital',
  x == 'spict_perform_status' ~ 'Performance poor status',
  x == 'spict_care_others' ~ 'Depends on others for care',
  x == 'spict_weight_loss' ~ 'Weight loss',
  x == 'spict_persist_sympt' ~ 'Persistent symptoms',
  x == 'spict_care_focus' ~ 'Palliative care request'
)
return(y)  
}

# rename for CRISTAL variables
nice.rename.cristal = function(x){
  y = case_when(
    x == 'cristal_admit_ed' ~ 'Admitted via ED',
    x == 'cristal_admit_source' ~ 'From nursing home',
    x == 'age' ~ 'Age',
    x == 'cristal_previous_admit' ~ 'Previous hospitalisation',
    x == 'cristal_icu' ~ 'ICU admission',
    x == 'cristal_cfs_score' ~ 'Frailty score', ## overall CFS score
    x == 'cristal_cfs_score1' ~ 'Frailty score >= 5', ## lower threshold
    x == 'cristal_cfs_score2' ~ 'Frailty score >= 7', ## upper threshold
    x == "cristal_cancer" ~ 'Advanced cancer',
    x == "cristal_proteinuria"  ~ 'Proteinuria',
    x == "cristal_ckd"  ~ 'Chronic kidney disease',
    x == "cristal_ecg" ~ 'Abnormal ECG',
    x == "cristal_ami" ~ 'Acute myocardial infarction',
    x == "cristal_chf"  ~ 'Chronic heart failure',          
    x == "cristal_copd" ~ 'COPD',
    x == "cristal_stroke" ~ 'Stroke',
    x == "cristal_cognitive" ~ 'Cognitive impairment',     
    x == "cristal_liver" ~ 'Liver disease',
    x == "cristal_gcs" ~ 'Glasgow Coma Scale',
    x == "cristal_sbp" ~ 'Systolic blood pressure',
    x == "cristal_resp" ~ 'Respiratory rate',
    x == "cristal_hr" ~ 'Pulse rate',
    x == "cristal_02" ~ 'Oxygen required or saturation < 90%',
    x == "cristal_bgl" ~ 'Hypoglycaemia',
    x == "cristal_seizures" ~ 'Seizures',
    x == "cristal_urine" ~ 'Low urine output'
  )
  return(y)  
}

# rename for CRISTAL variables using acronyms
nice.rename.cristal.acronyms = function(x){
  y = case_when(
    x == 'cristal_admit_ed' ~ 'Admitted via ED',
    x == 'cristal_admit_source' ~ 'From nursing home',
    x == 'age' ~ 'Age',
    x == 'cristal_previous_admit' ~ 'Previous hospitalisation',
    x == 'cristal_icu' ~ 'ICU admission',
    x == 'cristal_cfs_score' ~ 'Frailty score', ## overall CFS score
    x == 'cristal_cfs_score1' ~ 'Frailty score >= 5', ## lower threshold
    x == 'cristal_cfs_score2' ~ 'Frailty score >= 7', ## upper threshold
    x == "cristal_cancer" ~ 'Advanced cancer',
    x == "cristal_proteinuria"  ~ 'Proteinuria',
    x == "cristal_ckd"  ~ 'CKD', # acronym
    x == "cristal_ecg" ~ 'Abnormal ECG',
    x == "cristal_ami" ~ 'AMI', # acronym
    x == "cristal_chf"  ~ 'CHF', # acronym
    x == "cristal_copd" ~ 'COPD',
    x == "cristal_stroke" ~ 'Stroke',
    x == "cristal_cognitive" ~ 'Cognitive impairment',     
    x == "cristal_liver" ~ 'Liver disease',
    x == "cristal_gcs" ~ 'GCS', # acronym
    x == "cristal_sbp" ~ 'SBP', # acronym
    x == "cristal_resp" ~ 'Respiratory rate',
    x == "cristal_hr" ~ 'Pulse rate',
    x == "cristal_02" ~ 'O2 required or saturation < 90%',
    x == "cristal_bgl" ~ 'Hypoglycaemia',
    x == "cristal_seizures" ~ 'Seizures',
    x == "cristal_urine" ~ 'Low urine output'
  )
  return(y)  
}

# rename for care directive
nice.rename.directive = function(x){
  y = case_when(
    x == 'type_care_directive_1' ~	'Active treatment reduced or ceased',
    x == 'type_care_directive_2' ~	'Active treatment continued',
    x == 'type_care_directive_3' ~	'Active treatment increased',
    x == 'type_care_directive_4' ~	'Comfort care initiated or increased',
    x == 'type_care_directive_5' ~	'Resuscitation plan completed or updated',
    x == 'type_care_directive_6' ~	'Advance Care Plan completed or initiated',
    x == 'type_care_directive_7' ~	'Statement of choices completed or initiated',
    x == 'type_care_directive_8' ~	'Other'
  )
  return(y)  
}

# list of names in order (used to arrange results on geom_bar)
care_directives = c('Active treatment reduced or ceased', 'Active treatment continued', 
'Active treatment increased', 'Comfort care initiated or increased', 
'Resuscitation plan completed or updated', 'Advance Care Plan completed or initiated', 
'Statement of choices completed or initiated', 'Other')

# remove Hmisc labels (used by 0_read_data.R)
# https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in seq_along(x)) {
      class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
      attr(x[[i]],"label") <- NULL
    } 
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}
