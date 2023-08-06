# 5_make_flow.R
# draws flow chart
# does not work! abandoned

org_cohort <- boxGrob(glue("At-risk hospital admissions in trial database\nn = {n_0}",n_0=format(n0,big.mark=',')))
eligible <- boxGrob(glue("Matched to index hospital admission date\nn = {n_1a}",n_1a=format(n1,big.mark=',')))
included <- boxGrob(glue("Total at-risk admissions analysed: n = {final_n}",
                         " - Hospital 1: n = {n_gcuh}", # blinded
                         " - Hospital 2: n = {n_rbwh}",
                         " - Hospital 3: n = {n_tpch}",
                         final_n = format(n2,big.mark=','),
                         n_gcuh = format(filter(n_cluster_teams,Hospital=='GCUH')[['n_atrisk']],big.mark=','),
                         n_tpch = format(filter(n_cluster_teams,Hospital=='TPCH')[['n_atrisk']],big.mark=','),
                         n_rbwh = format(filter(n_cluster_teams,Hospital=='RBWH')[['n_atrisk']],big.mark=','),
                         .sep = "\n"),
                    just = "left")
excluded_1 <- boxGrob(glue("Excluded:",
                           " - Unknown hospital admission date: n = {unmatchedadmit}",
                           " - Duplicate at-risk identification: n = {duplicateadmit}",
                           unmatchedadmit = n_unmatched,
                           duplicateadmit = n_duplicate_screen,
                           .sep = "\n"),
                      just = "left")

excluded_2 <- boxGrob(glue("Excluded:",
                           " - Intervention establishment phase: n = {establishment}",
                           establishment = n_excluded_establishment,
                           .sep = "\n"),
                      just = "left")

# Move boxes to where we want them
vert <- spreadVertical(org_cohort=org_cohort,eligible = eligible,included = included)


y1 <- coords(vert$eligible)$top +distance(vert$org_cohort, vert$eligible, half = TRUE, center = FALSE)
y2 <- coords(vert$included)$top +distance(vert$eligible, vert$included, half = TRUE, center = FALSE)
excluded_1 <- moveBox(excluded_1,x = 0.8,y = y1)
excluded_2 <- moveBox(excluded_2,x = 0.8,y = y2)

# Connect vertical arrows, skip last box
for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}

# Add a connection to the exclusions
connectGrob(vert$org_cohort, excluded_1, type = "L")
connectGrob(vert$eligible, excluded_2, type = "L")

# Print boxes
vert
excluded_1
excluded_2

#####


source('5_make_flow.R') # moved to file by AGB

# export to figure
jpeg('manuscript/figures/flow.jpg', width=5, height=6, units='in', res=300)
grid.newpage()
source('5_make_flow.R') 
invisible(dev.off())
