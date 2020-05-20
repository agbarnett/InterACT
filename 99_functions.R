# 99_functions.R
# helpful functions for InterACT
# May 2020

# rename for SPICT variables
nice.rename.spict = function(x){
y = case_when(
  x == 'spict_unplan_hosp' ~ 'Unplanned hospital',
  x == 'spict_perform_status' ~ 'Performance poor',
  x == 'spict_care_others' ~ 'Depends on others',
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
    x == 'cristal_cfs_score' ~ 'Frailty score >= 6', ## need to refine this for two thresholds
    x == "cristal_cancer" ~ 'Advanced cancer',
    x == "cristal_proteinuria"  ~ 'Proteinuria',
    x == "cristal_ckd"  ~ 'Chronic kidney disease',
    x == "cristal_ecg" ~ 'Abnormal ECG',
    x == "cristal_ami" ~ 'Acute myocardial infarction',
    x == "cristal_chf"  ~ 'Chronic heart failure',          
    x == "cristal_copd" ~ 'COPD',
    x == "cristal_stroke" ~ 'Stroke',
    x == "cristal_cognitive" ~ 'Cognitive impairment',     
    x == "cristal_liver" ~ 'Liver disease'
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
