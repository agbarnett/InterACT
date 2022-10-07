# Synthetic data

We created a synthetic version of the data using the methods described in [Quintana](https://elifesciences.org/articles/53275). This was to avoid any risk of patients being identified.

These patient-level data used to examine the in-hospital outcomes of:

* clinician-led care review discussions
* review of care directive measures
* palliative care referrals

The variables are:

* Outcome (care review, care directive, palliative care)
* Age (years)
* Gender (`pt_sex`)
* CRiSTAL score (`cristal_score`)
* SPICT score (`spict_score`)
* Time between admission and outcome (`time` in fractional days)
* Event (Discharge, Death, Prior, Censored)
* Intervention time (`int_time`, Usual care, Intervention)
* Hospital (blinded)
* Clinical team (`team` blinded)

The data are in R and csv formats. There are 12,738 rows in the data.
