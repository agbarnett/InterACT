# Synthetic data

We created a synthetic version of the data using the [synthpop](https://www.jstatsoft.org/article/view/v074i11) package. This was to avoid any risk of patients being identified.

These patient-level data used to examine the in-hospital outcomes of:

* ICU admission (outcome 1)
* Time to death or discharge (outcome 2)
* Time to readmission (outcome 3)
* Time to medical emergency team call  (outcome 7)

The data for outcomes 4, 5 and 6 are [here](https://github.com/agbarnett/InterACT/tree/master/data).

The datasets are:

* `cohort_lookup`: list of participant identifiers to link trial data with hospital admissions data.
* `cristal_spict_admission_info`: Main dataset for the analysis of Outcomes 1, 2, 3 and 7, containing patient-level data on initial hospital admission and follow-up.
* `duplicated_screening`: list of participants with multiple cristal/spict screening results recorded at trial entry.
* `excluded_on_censoring`: list of participants who were excluded from the analysis based on trial protocol criteria.
* `trial_public_readmissions`: subset of potential patient readmissions to define outcome 55 (time to readmission).

The data are in R and Excel formats. 

We plan to provide the original data via controlled access on [Health Data Australia](https://researchdata.edu.au/health/).