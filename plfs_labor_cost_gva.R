# PLFS Labor Cost — FY2023-24 (Q8 2022-23 + Q1-Q3 2023-24)
# industry codes mapped as per 2 digits
# ------------------------------------------------------------------------------------------------
# exclude STATUS CODE 21 POLICY  (helper in h.h. enterprise / unpaid family worker)
# NCO / NIC CONSTRUCTION POLICY (principal status only — usual status ocu_pas/ocu_sas,
# ind_pas/ind_sas. This does NOT touch current weekly status (ocu_cws/acws) anywhere.)
# ------------------------------------------------------------------------------------------------
# NCO_new (occupation code) and NIC_code (industry code) are constructed together, sourced from
# the SAME status (pas or sas) for a given person, following this precedence:
#   1. If ocu_pas is blank              -> use the sas pair   (NCO = ocu_sas, NIC = ind_sas)
#   2. Else if ocu_pas is one of the    -> use the pas pair   (NCO = ocu_pas, NIC = ind_pas)
#      59 data-producing occupation codes (target_occ_codes)
#   3. Else if ocu_sas is one of the    -> use the sas pair   (NCO = ocu_sas, NIC = ind_sas)
#      59 data-producing occupation codes
#   4. Otherwise (default)              -> use the pas pair   (NCO = ocu_pas, NIC = ind_pas)

library(dplyr)
library(tidyr)
library(readxl)

# 0. CONFIG
file_path1             <- "/Users/Work/Desktop/data valuation/labor costs/labor cost_FY2023-24/person level datasets/perv1_2022-23.csv"
file_path2             <- "/Users/Work/Desktop/data valuation/labor costs/labor cost_FY2023-24/person level datasets/perv1_2023-24.csv"
combined_fy_23_24      <- "/Users/Work/Desktop/data valuation/labor costs/labor cost_FY2023-24/person level datasets/perv_fy2023_24.csv"
division_section_path  <- "/Users/Work/Desktop/data valuation/labor costs/labor cost_FY2023-24/nas section_nic division.xlsx"
nco_ir_path            <- "/Users/Work/Desktop/data valuation/labor costs/labor cost_FY2023-24/NCO_IR.xlsx"

# 1. LOAD RAW PERSON-LEVEL DATASETS
perv2022_23 <- read.csv(file_path1, stringsAsFactors = FALSE, colClasses = "character")
perv2023_24 <- read.csv(file_path2, stringsAsFactors = FALSE, colClasses = "character")

# 2. RENAME RAW VARIABLES TO TARGET NAMES
rename_map <- c(
  "FI_perv1"                      = "file_id",
  "B1q2_perv1"                    = "sch",
  "qtr_perv1"                     = "qtr",
  "visit_perv1"                   = "visit",
  "b1q3_perv1"                    = "sec",
  "state_perv1"                   = "st",
  "distcode_perv1"                = "dc",
  "nss_region_perv1"              = "nss_reg",
  "b1q5_perv1"                    = "strm",
  "b1q6_perv1"                    = "sstrm",
  "b1q11_perv1"                   = "ss",
  "b1q12_perv1"                   = "sro",
  "b1q1_perv1"                    = "mfsu",
  "b1q13_perv1"                   = "seg",
  "b1q14_perv1"                   = "sss",
  "b1q15_perv1"                   = "ssu",
  "b4q1_perv1"                    = "srl",
  "b4q4_perv1"                    = "rel",
  "b4q5_perv1"                    = "sex",
  "b4q6_perv1"                    = "age",
  "b4q7_perv1"                    = "marst",
  "b4q8_perv1"                    = "gedu_lvl",
  "b4q9_perv1"                    = "tedu_lvl",
  "b4q10_perv1"                   = "form_edu",
  "b4q11_perv1"                   = "curr_att",
  "b4q12_perv1"                   = "voc",
  "b4pt1q3_perv1"                 = "voc_compl",
  "b4pt1q4_perv1"                 = "voc_fld",
  "b4pt1q5_perv1"                 = "voc_dur",
  "b4pt1q6_perv1"                 = "voc_typ",
  "b4pt1q7_perv1"                 = "voc_fund",
  "b5pt1q3_perv1"                 = "pas",
  "b5pt1q5_perv1"                 = "ind_pas",
  "b5pt1q6_perv1"                 = "ocu_pas",
  "b5pt1q7_perv1"                 = "has_sas",
  "b5pt1q8_perv1"                 = "loc_pas",
  "b5pt1q9_perv1"                 = "etyp_pas",
  "b5pt1q10_perv1"                = "wrkr_pas",
  "b5pt1q11_perv1"                = "job_pas",
  "b5pt1q12_perv1"                = "leave_pas",
  "b5pt1q13_perv1"                = "ssec_pas",
  "b5pt1q14_perv1"                = "ecoprd_pas",
  "b5pt2q3_perv1"                 = "sas",
  "b5pt2q5_perv1"                 = "ind_sas",
  "b5pt2q6_perv1"                 = "ocu_sas",
  "b5pt2q7_perv1"                 = "loc_sas",
  "b5pt2q8_perv1"                 = "etyp_sas",
  "b5pt2q9_perv1"                 = "wrkr_sas",
  "b5pt2q10_perv1"                = "job_sas",
  "b5pt2q11_perv1"                = "leave_sas",
  "b5pt2q12_perv1"                = "ssec_sas",
  "b5pt2q13_perv1"                = "ecoprd_sas",
  "b5pt3q5_perv1"                 = "wrk_365",
  "b5pt3q6_perv1"                 = "dur_pas",
  "b5pt3q7_perv1"                 = "dur_sas",
  "b5pt3q8_perv1"                 = "eff_pas",
  "b5pt3q9_perv1"                 = "dur_unp",
  "b5pt3q10_perv1"                = "evr_wrk",
  "b5pt3q11_perv1"                = "rea_nw",
  "b5pt3q12_perv1"                = "rea",
  "b6q4_3pt1_perv1"               = "das17",
  "b6q5_3pt1_perv1"               = "ind17",
  "b6q6_3pt1_perv1"               = "hr17",
  "b6q9_3pt1_perv1"               = "ern17",
  "b6q4_act2_3pt1_perv1"          = "das27",
  "b6q5_act2_3pt1_perv1"          = "ind27",
  "b6q6_act2_3pt1_perv1"          = "hr27",
  "b6q9_act2_3pt1_perv1"          = "ern27",
  "b6q7_3pt1_perv1"               = "hr7",
  "b6q8_3pt1"                     = "ahr7",
  "b6q4_3pt2"                     = "das16",
  "b6q5_3pt2"                     = "ind16",
  "b6q6_3pt2_perv1"               = "hr16",
  "b6q9_3pt2_perv1"               = "ern16",
  "b6q4_act2_3pt2_perv1"          = "das26",
  "b6q5_act2_3pt2_perv1"          = "ind26",
  "b6q6_act2_3pt2_perv1"          = "hr26",
  "b6q9_act2_3pt2_perv1"          = "ern26",
  "b6q7_3pt2_perv1"               = "hr6",
  "b6q8_3pt2_perv1"               = "ahr6",
  "b6q4_3pt3_perv1"               = "das15",
  "b6q5_3pt3_perv1"               = "ind15",
  "b6q6_3pt3_perv1"               = "hr15",
  "b6q9_3pt3_perv1"               = "ern15",
  "b6q4_act2_3pt3_perv1"          = "das25",
  "b6q5_act2_3pt3_perv1"          = "ind25",
  "b6q6_act2_3pt3_perv1"          = "hr25",
  "b6q9_act2_3pt3_perv1"          = "ern25",
  "b6q7_3pt3_perv1"               = "hr5",
  "b6q8_3pt3"                     = "ahr5",
  "b6q4_3pt4_perv1"               = "das14",
  "b6q5_3pt4_perv1"               = "ind14",
  "b6q6_3pt4_perv1"               = "hr14",
  "b6q9_3pt4_perv1"               = "ern14",
  "b6q4_act2_3pt4_perv1"          = "das24",
  "b6q5_act2_3pt4_perv1"          = "ind24",
  "b6q6_act2_3pt4_perv1"          = "hr24",
  "b6q9_act2_3pt4_perv1"          = "ern24",
  "b6q7_3pt4_perv1"               = "hr4",
  "b6q8_3pt4_perv1"               = "ahr4",
  "b6q4_3pt5_perv1"               = "das13",
  "b6q5_3pt5_perv1"               = "ind13",
  "b6q6_3pt5_perv1"               = "hr13",
  "b6q9_3pt5_perv1"               = "ern13",
  "b6q4_act2_3pt5_act2_perv1"     = "das23",
  "b6q5_act2_3pt5_perv1"          = "ind23",
  "b6q6_act2_3pt5_perv1"          = "hr23",
  "b6q9_act2_3pt5_perv1"          = "ern23",
  "b6q7_3pt5_perv1"               = "hr3",
  "b6q8_3pt5_perv1"               = "ahr3",
  "b6q4_3pt6_perv1"               = "das12",
  "b6q5_3pt6_perv1"               = "ind12",
  "b6q6_3pt6_perv1"               = "hr12",
  "b6q9_3pt6_perv1"               = "ern12",
  "b6q4_act2_3pt6_perv1"          = "das22",
  "b6q5_act2_3pt6_perv1"          = "ind22",
  "b6q6_act2_3pt6_perv1"          = "hr22",
  "b6q9_act2_3pt6_perv1"          = "ern22",
  "b6q7_3pt6_perv1"               = "hr2",
  "b6q8_3pt6_perv1"               = "ahr2",
  "b6q4_3pt7_perv1"               = "das11",
  "b6q5_3pt7"                     = "ind11",
  "b6q6_3pt7_perv1"               = "hr11",
  "b6q9_3pt7_perv1"               = "ern11",
  "b6q4_act2_3pt7_perv1"          = "das21",
  "b6q5_act2_3pt7_perv1"          = "ind21",
  "b6q6_act2_3pt7_perv1"          = "hr21",
  "b6q9_act2_3pt7_perv1"          = "ern21",
  "b6q7_3pt7_perv1"               = "hr1",
  "b6q8_3pt7_perv1"               = "ahr1",
  "b6q5_perv1"                    = "acws",
  "b6q6_perv1"                    = "aind_cws",
  "b6q7_perv1"                    = "ocu_cws",
  "b6q9_perv1"                    = "ern_reg",
  "b6q10_perv1"                   = "ern_self",
  "NSS_perv1"                     = "nss",
  "NSC_perv1"                     = "nsc",
  "mult_perv1"                    = "mult",
  "no_qtr_perv1"                  = "no_qtr"
)

# Applies rename_map to a data frame; leaves any column not in the map
# untouched, and doesn't error if a mapped column is missing from that file.
apply_rename_map <- function(df, map) {
  df %>%
    rename_with(~ ifelse(.x %in% names(map), unname(map[.x]), .x))
}

perv2022_23 <- apply_rename_map(perv2022_23, rename_map)
perv2023_24 <- apply_rename_map(perv2023_24, rename_map)

# 3. FILTER TO FY2023-24 QUARTERS AND TAG SURVEY ROUND
perv2022_q8 <- perv2022_23 %>%
  filter(qtr == "Q8") %>%
  mutate(survey_round = "2022-23")

perv2023_q1_q3 <- perv2023_24 %>%
  filter(qtr %in% c("Q1", "Q2", "Q3")) %>%
  mutate(survey_round = "2023-24")

# 4. COMBINE INTO ONE FY2023-24 DATASET
perv_fy2023_24 <- bind_rows(perv2022_q8, perv2023_q1_q3)

# 5. BUILD + VERIFY THE PERSON-LEVEL COMPOSITE KEY, DEDUPE
#    Unique ID = quarter + visit + sec + mfsu + sss + ssu + srl
perv_fy2023_24 <- perv_fy2023_24 %>%
  mutate(unique_id = paste(qtr, visit, sec, mfsu, sss, ssu, srl, sep = "_"))

# 5a. Target occupation codes (the 59 data-producing occupations).
target_occ_codes <- c(
  "111","121","122","132","133","134","141","143",
  "211","212","213","214","215","216",
  "221","223","225","226",
  "231","235",
  "241","242","243",
  "251","252",
  "261","262","263","264",
  "311","312","313","314","315",
  "321","323","324","325",
  "331","332","333","334","335",
  "343",
  "351",
  "411","413",
  "421","422",
  "431","432",
  "441",
  "622",
  "723","732","742","754",
  "811",
  "921"
)

# ===============================================================================================
# 6. LABOR COST — SHARED BASE (identical regardless of occupation membership; computed once and
#    reused by both the TARGET and NONTARGET branches below)
# ===============================================================================================

# 6a. Principal status subset
principal_status <- perv_fy2023_24 %>%
  mutate(
    pas_num = as.numeric(pas),
    sas_num = as.numeric(sas),
    age_num = as.numeric(age)
  ) %>%
  filter(
    pas_num %in% c(11, 12, 21, 31, 41, 42, 51, 61, 62, 71, 72) |
      sas_num %in% c(11, 12, 21, 31, 41, 42, 51, 61, 62, 71, 72)
  )

# create weight
principal_status <- principal_status %>%
  mutate(
    mult    = as.numeric(mult),
    nss     = as.numeric(nss),
    nsc     = as.numeric(nsc),
    no_qtr  = as.numeric(no_qtr),
    weight  = if_else(nss == nsc, mult / 100, mult / 200) / no_qtr
  )

# pad_left_zero: used to align code widths (e.g. NIC codes) before matching
pad_left_zero <- function(x, width) {
  ifelse(is.na(x) | x == "", x,
         paste0(strrep("0", pmax(width - nchar(x), 0)), x))
}

# 6b. Construct NIC_code from ocu_pas/ocu_sas/ind_pas/ind_sas, per NCO/NIC CONSTRUCTION POLICY
# AFTER (step 6b) — keep ocu_source, don't drop it
principal_status <- principal_status %>%
  mutate(
    ocu_pas_chr = trimws(as.character(ocu_pas)),
    ocu_sas_chr = trimws(as.character(ocu_sas)),
    ocu_source = case_when(
      is.na(ocu_pas) | ocu_pas_chr == "" ~ "sas",
      ocu_pas_chr %in% target_occ_codes  ~ "pas",
      ocu_sas_chr %in% target_occ_codes  ~ "sas",
      TRUE                                ~ "pas"
    ),
    NIC_code = if_else(ocu_source == "pas", as.character(ind_pas), as.character(ind_sas)),
    NIC_code = pad_left_zero(NIC_code, 5)
  ) %>%
  select(-ocu_pas_chr, -ocu_sas_chr)

n_digits <- 2
principal_status <- principal_status %>%
  mutate(NIC_division = substr(NIC_code, 1, n_digits))

# 6c. Division -> Section crosswalk (manual table)
division_section_nas <- read_excel(division_section_path, col_types = "text") %>%
  rename(
    NIC_division = `Division Code`,
    section_name = Category
  ) %>%
  mutate(
    NIC_division = pad_left_zero(trimws(NIC_division), 2),
    section_name = trimws(section_name)
  ) %>%
  distinct(NIC_division, .keep_all = TRUE)  # guard: one section per division

# 6d. Industry code from CURRENT WEEKLY STATUS (aind_cws) — used ONLY for wage estimation
principal_status <- principal_status %>%
  mutate(
    NIC_division_cws = pad_left_zero(trimws(as.character(aind_cws)), 2)
  )

# 7. Convert earning variables to numeric
daily_earn_cols <- c(
  "ern11", "ern12", "ern13", "ern14", "ern15", "ern16", "ern17",
  "ern21", "ern22", "ern23", "ern24", "ern25", "ern26", "ern27"
)
principal_status <- principal_status %>%
  mutate(across(all_of(daily_earn_cols), ~ suppressWarnings(as.numeric(.))))

# 8. Days actually worked in reference week (Block 5, Current Weekly Status)
status_cols <- paste0("das", c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27))
principal_status <- principal_status %>%
  mutate(across(all_of(status_cols), ~ suppressWarnings(as.numeric(.))))

working_codes <- c(41, 42, 51)
principal_status <- principal_status %>%
  mutate(
    day1_worked = as.integer(das11 %in% working_codes | das21 %in% working_codes),
    day2_worked = as.integer(das12 %in% working_codes | das22 %in% working_codes),
    day3_worked = as.integer(das13 %in% working_codes | das23 %in% working_codes),
    day4_worked = as.integer(das14 %in% working_codes | das24 %in% working_codes),
    day5_worked = as.integer(das15 %in% working_codes | das25 %in% working_codes),
    day6_worked = as.integer(das16 %in% working_codes | das26 %in% working_codes),
    day7_worked = as.integer(das17 %in% working_codes | das27 %in% working_codes),
    
    Days_Worked_In_Reference_Week =
      day1_worked + day2_worked + day3_worked + day4_worked +
      day5_worked + day6_worked + day7_worked
  )

# 9. Day-wise wage variables
principal_status <- principal_status %>%
  mutate(
    day1_wage = if_else(is.na(ern11) & is.na(ern21), NA_real_,
                        rowSums(across(c(ern11, ern21)), na.rm = TRUE)),
    day2_wage = if_else(is.na(ern12) & is.na(ern22), NA_real_,
                        rowSums(across(c(ern12, ern22)), na.rm = TRUE)),
    day3_wage = if_else(is.na(ern13) & is.na(ern23), NA_real_,
                        rowSums(across(c(ern13, ern23)), na.rm = TRUE)),
    day4_wage = if_else(is.na(ern14) & is.na(ern24), NA_real_,
                        rowSums(across(c(ern14, ern24)), na.rm = TRUE)),
    day5_wage = if_else(is.na(ern15) & is.na(ern25), NA_real_,
                        rowSums(across(c(ern15, ern25)), na.rm = TRUE)),
    day6_wage = if_else(is.na(ern16) & is.na(ern26), NA_real_,
                        rowSums(across(c(ern16, ern26)), na.rm = TRUE)),
    day7_wage = if_else(is.na(ern17) & is.na(ern27), NA_real_,
                        rowSums(across(c(ern17, ern27)), na.rm = TRUE))
  )

# 10. Daily Wage Rate and Annual Casual Wage
principal_status <- principal_status %>%
  mutate(
    week_total_wage = if_else(
      rowSums(!is.na(across(c(day1_wage, day2_wage, day3_wage,
                              day4_wage, day5_wage, day6_wage,
                              day7_wage)))) == 0,
      NA_real_,
      rowSums(across(c(day1_wage, day2_wage, day3_wage,
                       day4_wage, day5_wage, day6_wage,
                       day7_wage)), na.rm = TRUE)
    ),
    
    
    Daily_Wage_Rate = if_else(
      Days_Worked_In_Reference_Week > 0 & !is.na(week_total_wage),
      week_total_wage / Days_Worked_In_Reference_Week,
      NA_real_
    ),
    
    Monthly_Casual_Wage = Daily_Wage_Rate * 30,
    Annual_Casual_Wage = Monthly_Casual_Wage * 12
  )

# 11. Annual wage for regular/self-employed (ern_reg, ern_self are monthly)
principal_status <- principal_status %>%
  mutate(
    ern_reg  = suppressWarnings(as.numeric(ern_reg)),
    ern_self = suppressWarnings(as.numeric(ern_self)),
    
    Annual_Regular_Wage       = ern_reg * 12,
    Annual_Self_Employed_Wage = ern_self * 12
  )

# 11a. One annual wage variable based on employment type
#      NOTE: status 21 has no acws code of its own in this scheme and is not assigned a wage.
principal_status <- principal_status %>%
  mutate(
    Annual_Wage = case_when(
      acws %in% c("41", "42", "51")             ~ Annual_Casual_Wage,
      acws %in% c("11", "12", "21", "61", "62")  ~ Annual_Self_Employed_Wage,
      acws %in% c("31", "71", "72")              ~ Annual_Regular_Wage,
      TRUE ~ NA_real_
    )
  )

# 20. NCO x NIC_division IR crosswalk (only used by the TARGET branch)
nco_ir <- read_excel(nco_ir_path, col_types = "text") %>%
  mutate(
    IR = suppressWarnings(as.numeric(IR)),
    NCO_code = as.character(as.integer(suppressWarnings(as.numeric(NCO_code))))
  )

# Shared employment-group map (used by both branches' 24b step)
employment_group_map <- c(
  "Regular wage/salaried" = "Regular_Casual",
  "Casual Labour"         = "Regular_Casual",
  "Self-employed"         = "Self_Employed"
)

# ===============================================================================================
# TARGET (DATA-PRODUCING) OCCUPATIONS — the 59-code list
# Every object below is suffixed _target.
# ===============================================================================================

# 13. WAGE dataset (occupation from ocu_cws, current weekly status) — TARGET occupations
#     STATUS 21 IS EXCLUDED HERE (acws != "21").
wage_dataset_acws_target <- principal_status %>%
  mutate(
    ocu_cws = trimws(as.character(ocu_cws)),
    acws    = trimws(as.character(acws)),
    age     = as.numeric(age),
    NIC_division = NIC_division_cws   # overwrite, not a new column
  ) %>%
  mutate(
    employment_type = case_when(
      acws %in% c("41", "42", "51")        ~ "Casual Labour",
      acws %in% c("11", "12", "61", "62")  ~ "Self-employed",
      acws %in% c("31", "71", "72")         ~ "Regular wage/salaried",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    age >= 15,
    ocu_cws %in% target_occ_codes,
    acws != "21",
    !is.na(employment_type),
    !is.na(Annual_Wage),
    !is.na(weight)
  )

# 14. Average annual wage by occupation and employment type
wage_by_nco_acws_target <- wage_dataset_acws_target %>%
  group_by(ocu_cws, employment_type) %>%
  summarise(
    Total_Population_Wages = sum(Annual_Wage * weight, na.rm = TRUE),
    Total_Weight           = sum(weight, na.rm = TRUE),
    Average_Annual_Wage    = if_else(Total_Weight > 0,
                                     Total_Population_Wages / Total_Weight,
                                     NA_real_),
    .groups = "drop"
  )

# 15. Overall average annual wage by occupation
overall_wage_by_nco_target <- wage_by_nco_acws_target %>%
  group_by(ocu_cws) %>%
  summarise(
    Overall_Total_Population_Wages = sum(Total_Population_Wages, na.rm = TRUE),
    Overall_Total_Weight           = sum(Total_Weight, na.rm = TRUE),
    Overall_Average_Annual_Wage    = if_else(Overall_Total_Weight > 0,
                                             Overall_Total_Population_Wages / Overall_Total_Weight,
                                             NA_real_),
    .groups = "drop"
  )

# 16. Wide format — one row per occupation code, one column per employment type
wage_by_nco_wide_target <- wage_by_nco_acws_target %>%
  select(ocu_cws, employment_type, Average_Annual_Wage) %>%
  pivot_wider(names_from = employment_type, values_from = Average_Annual_Wage) %>%
  left_join(overall_wage_by_nco_target, by = "ocu_cws") %>%
  select(
    ocu_cws,
    `Regular wage/salaried`,
    `Casual Labour`,
    `Self-employed`,
    Overall_Average_Annual_Wage
  ) %>%
  arrange(ocu_cws)

# 17. EMPLOYEE COUNT dataset (occupation from ocu_pas/ocu_sas, usual status) — TARGET occupations
#     STATUS 21 IS EXCLUDED HERE (status_code != "21").
employee_dataset_status_code_target <- principal_status %>%
  mutate(
    ocu_pas_chr = trimws(as.character(ocu_pas)),
    ocu_sas_chr = trimws(as.character(ocu_sas)),
    # AFTER — both derived from the SAME ocu_source
    NCO_new = if_else(ocu_source == "pas", as.character(ocu_pas), as.character(ocu_sas)),
    
    status_code = case_when(
      ocu_source == "pas" & pas_num %in% c(11,12,21,31,41,42,51,61,62,71,72) ~ as.character(pas_num),
      ocu_source == "sas" & sas_num %in% c(11,12,21,31,41,42,51,61,62,71,72) ~ as.character(sas_num),
      # fallback: ocu_source's own status code isn't valid, but the OTHER one is —
      # use that status entirely (occupation, industry, and employment type together)
      # so we never mix status columns again.
      pas_num %in% c(11,12,21,31,41,42,51,61,62,71,72) ~ as.character(pas_num),
      sas_num %in% c(11,12,21,31,41,42,51,61,62,71,72) ~ as.character(sas_num),
      TRUE ~ NA_character_
    ),
    
    employment_type = case_when(
      status_code %in% c("41", "42", "51")             ~ "Casual Labour",
      status_code %in% c("11", "12", "61", "62")        ~ "Self-employed",
      status_code %in% c("31", "71", "72")              ~ "Regular wage/salaried",
      TRUE ~ NA_character_
    ),
    
    mult_num = as.numeric(mult),
    age = as.numeric(age)
  ) %>%
  select(-ocu_pas_chr, -ocu_sas_chr) %>%
  filter(
    age >= 15,
    NCO_new %in% target_occ_codes,
    status_code != "21",
    !is.na(employment_type)
  )

# --- Occupation x employment type breakdown (long format) ---
employee_count_by_type_target <- employee_dataset_status_code_target %>%
  group_by(NCO_new, employment_type) %>%
  summarise(
    Sample_Count         = n(),
    Avg_Mult             = mean(mult_num, na.rm = TRUE),
    Avg_Weight           = mean(weight, na.rm = TRUE),
    Total_Employee_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(NCO_new, employment_type)

# --- Total employment count per occupation (all 3 types combined) ---
employee_count_total_by_occ_target <- employee_dataset_status_code_target %>%
  group_by(NCO_new) %>%
  summarise(
    Total_Employment_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# --- Wide format: one row per occupation code, one column per employment type + total ---
employee_count_wide_target <- employee_count_by_type_target %>%
  select(NCO_new, employment_type, Total_Employee_Count) %>%
  pivot_wider(
    names_from  = employment_type,
    values_from = Total_Employee_Count,
    values_fill = 0
  ) %>%
  left_join(employee_count_total_by_occ_target, by = "NCO_new") %>%
  select(
    NCO_new,
    `Regular wage/salaried`,
    `Casual Labour`,
    `Self-employed`,
    Total_Employment_Count
  ) %>%
  arrange(NCO_new)

# 18. Final NCO-level summary
nco_summary_target <- employee_count_by_type_target %>%
  left_join(
    wage_by_nco_acws_target,
    by = c(
      "employment_type" = "employment_type",
      "NCO_new" = "ocu_cws"
    )
  ) %>%
  group_by(NCO_new, employment_type) %>%
  summarise(
    Total_Employee_Count = sum(Total_Employee_Count, na.rm = TRUE),
    Annual_Average_Wage  = Average_Annual_Wage,
    .groups = "drop"
  ) %>%
  group_by(NCO_new) %>%
  mutate(
    Total_Employee_Count_Overall = sum(Total_Employee_Count, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = employment_type,
    values_from = c(Total_Employee_Count, Annual_Average_Wage),
    names_glue = "{.value}_{employment_type}"
  ) %>%
  rename(
    Total_Employee_Count = Total_Employee_Count_Overall
  ) %>%
  left_join(
    overall_wage_by_nco_target %>%
      select(
        NCO_new = ocu_cws,
        Annual_Average_Wage = Overall_Average_Annual_Wage
      ),
    by = "NCO_new"
  ) %>%
  arrange(NCO_new)

#labor cost of occupations
labor_cost_target <- employee_count_wide_target %>%
  left_join(
    wage_by_nco_wide_target,
    by = c("NCO_new" = "ocu_cws")
  ) %>%
  left_join(
    nco_ir,
    by = c("NCO_new" = "NCO_code")
  ) %>%
  mutate(
    Labor_Cost = Total_Employment_Count *
      Overall_Average_Annual_Wage *
      IR
  )

# 19. Occupation x Industry table (NCO x NIC division)
nco_nic_employee_count_target <- employee_dataset_status_code_target %>%
  filter(!is.na(employment_type), !is.na(NCO_new), NCO_new != "",
         !is.na(NIC_division), NIC_division != "") %>%
  group_by(NCO_new, NIC_division) %>%
  summarise(
    Total_Employee_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

nco_nic_wage_target <- wage_dataset_acws_target %>%
  filter(!is.na(employment_type), !is.na(ocu_cws), ocu_cws != "",
         !is.na(NIC_division), NIC_division != "") %>%
  group_by(ocu_cws, NIC_division) %>%
  summarise(
    Total_Wage_Bill      = sum(Annual_Wage * weight, na.rm = TRUE),
    Total_Weight         = sum(weight, na.rm = TRUE),
    Average_Annual_Wage  = Total_Wage_Bill / Total_Weight,
    .groups = "drop"
  )

nco_nic_table_target <- left_join(
  nco_nic_employee_count_target,
  nco_nic_wage_target,
  by = c("NCO_new" = "ocu_cws", "NIC_division" = "NIC_division")
) %>%
  arrange(NCO_new, NIC_division)

# 20. NCO x NIC_division table + IR
nco_division_table_target <- nco_nic_table_target %>%
  rename(Total_Weight_Wage_Basis = Total_Weight) %>%
  left_join(nco_ir, by = c("NCO_new" = "NCO_code"))

# 21. Division-wise breakdown (by NIC_division x NCO_new)
division_nco_summary_target <- nco_nic_table_target %>%
  select(NIC_division, NCO_new, Total_Employee_Count,
         Total_Weight_Wage_Basis = Total_Weight,
         Average_Annual_Wage) %>%
  arrange(NIC_division, NCO_new)

# 22. Division-wise summary, broken out by employment type
division_employee_count_by_type_target <- employee_dataset_status_code_target %>%
  filter(!is.na(employment_type), !is.na(NIC_division), NIC_division != "") %>%
  group_by(NIC_division, employment_type) %>%
  summarise(
    Total_Employee_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

division_wage_by_type_target <- wage_dataset_acws_target %>%
  filter(!is.na(employment_type), !is.na(NIC_division), NIC_division != "") %>%
  group_by(NIC_division, employment_type) %>%
  summarise(
    Total_Wage_Bill      = sum(Annual_Wage * weight, na.rm = TRUE),
    Total_Weight         = sum(weight, na.rm = TRUE),
    Average_Annual_Wage  = Total_Wage_Bill / Total_Weight,
    .groups = "drop"
  )

division_summary_by_type_target <- left_join(
  division_employee_count_by_type_target,
  division_wage_by_type_target,
  by = c("NIC_division", "employment_type")
) %>%
  select(NIC_division, employment_type, Total_Employee_Count, Total_Wage_Bill,
         Total_Weight_Wage_Basis = Total_Weight,
         Average_Annual_Wage) %>%
  arrange(NIC_division, employment_type)

# --- Overall total per division (all 3 types combined) ---
division_employee_count_target <- employee_dataset_status_code_target %>%
  filter(!is.na(employment_type), !is.na(NIC_division), NIC_division != "") %>%
  group_by(NIC_division) %>%
  summarise(
    Total_Employee_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

division_wage_target <- wage_dataset_acws_target %>%
  filter(!is.na(employment_type), !is.na(NIC_division), NIC_division != "") %>%
  group_by(NIC_division) %>%
  summarise(
    Total_Wage_Bill      = sum(Annual_Wage * weight, na.rm = TRUE),
    Total_Weight         = sum(weight, na.rm = TRUE),
    Average_Annual_Wage  = Total_Wage_Bill / Total_Weight,
    .groups = "drop"
  )

division_summary_target <- left_join(
  division_employee_count_target,
  division_wage_target,
  by = "NIC_division"
) %>%
  select(NIC_division, Total_Employee_Count, Total_Wage_Bill,
         Total_Weight_Wage_Basis = Total_Weight,
         Average_Annual_Wage) %>%
  arrange(NIC_division)

# --- Division-level IR weighted by employment ---
division_ir_target <- nco_division_table_target %>%
  filter(
    !is.na(NIC_division),
    NIC_division != "",
    !is.na(IR)
  ) %>%
  group_by(NIC_division) %>%
  summarise(
    Avg_IR_Weighted_Employment =
      sum(IR * Total_Employee_Count, na.rm = TRUE) /
      sum(Total_Employee_Count, na.rm = TRUE),
    .groups = "drop"
  )


# --- Wide format: employee counts AND wages, both type-specific ---
division_employee_wide_target <- division_summary_by_type_target %>%
  select(NIC_division, employment_type, Total_Employee_Count) %>%
  pivot_wider(names_from = employment_type, values_from = Total_Employee_Count, values_fill = 0)

division_wage_wide_target <- division_summary_by_type_target %>%
  select(NIC_division, employment_type, Average_Annual_Wage) %>%
  pivot_wider(names_from = employment_type, values_from = Average_Annual_Wage, names_prefix = "Avg_Wage_")

division_summary_wide_target <- division_employee_wide_target %>%
  left_join(division_wage_wide_target, by = "NIC_division") %>%
  left_join(
    division_summary_target %>% 
      select(NIC_division, Total_Employee_Count, Average_Annual_Wage),
    by = "NIC_division"
  ) %>%
  left_join(
    division_ir_target,
    by = "NIC_division"
  ) %>%
  select(
    NIC_division,
    `Regular wage/salaried`,
    `Casual Labour`,
    `Self-employed`,
    Total_Employee_Count,
    `Avg_Wage_Regular wage/salaried`,
    `Avg_Wage_Casual Labour`,
    `Avg_Wage_Self-employed`,
    Average_Annual_Wage,
    Avg_IR_Weighted_Employment
  ) %>%
  arrange(NIC_division)

write.csv(division_summary_wide_target,  file.path(output_dir, "division_summary_wide.csv"),  row.names = FALSE)

# 24. Section-level rollup, broken out by employment type
section_summary_by_type_target <- division_summary_by_type_target %>%
  left_join(division_section_nas, by = "NIC_division") %>%
  group_by(section_name, employment_type) %>%
  summarise(
    Total_Employee_Count    = sum(Total_Employee_Count, na.rm = TRUE),
    Total_Wage_Bill         = sum(Total_Wage_Bill, na.rm = TRUE),
    Total_Weight_Wage_Basis = sum(Total_Weight_Wage_Basis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Average_Annual_Wage = Total_Wage_Bill / Total_Weight_Wage_Basis
  ) %>%
  arrange(section_name, employment_type)

# --- Overall total per section (all 3 employment types combined) ---
section_summary_target <- division_summary_target %>%
  left_join(division_section_nas, by = "NIC_division") %>%
  group_by(section_name) %>%
  summarise(
    Total_Employee_Count    = sum(Total_Employee_Count, na.rm = TRUE),
    Total_Wage_Bill         = sum(Total_Wage_Bill, na.rm = TRUE),
    Total_Weight_Wage_Basis = sum(Total_Weight_Wage_Basis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Average_Annual_Wage = Total_Wage_Bill / Total_Weight_Wage_Basis
  ) %>%
  arrange(section_name)

# --- Weighted IR at section level (IR is occupation-level, not type-specific) ---
nco_section_table_target <- nco_division_table_target %>%
  filter(!is.na(IR)) %>%
  left_join(division_section_nas, by = "NIC_division")

section_ir_target <- nco_section_table_target %>%
  group_by(section_name) %>%
  summarise(
    Avg_IR_Weighted_Employment = sum(IR * Total_Employee_Count, na.rm = TRUE) / sum(Total_Employee_Count, na.rm = TRUE),
    .groups = "drop"
  )

section_summary_target <- section_summary_target %>%
  left_join(section_ir_target, by = "section_name")

# --- Wide format: employee counts AND wages, both type-specific ---
section_employee_wide_target <- section_summary_by_type_target %>%
  select(section_name, employment_type, Total_Employee_Count) %>%
  pivot_wider(names_from = employment_type, values_from = Total_Employee_Count, values_fill = 0)

section_wage_wide_target <- section_summary_by_type_target %>%
  select(section_name, employment_type, Average_Annual_Wage) %>%
  pivot_wider(names_from = employment_type, values_from = Average_Annual_Wage, names_prefix = "Avg_Wage_")

section_summary_wide_target <- section_employee_wide_target %>%
  left_join(section_wage_wide_target, by = "section_name") %>%
  left_join(
    section_summary_target %>%
      select(section_name, Total_Employee_Count, Average_Annual_Wage,
             Avg_IR_Weighted_Employment),
    by = "section_name"
  ) %>%
  select(
    section_name,
    `Regular wage/salaried`, `Casual Labour`, `Self-employed`,
    Total_Employee_Count,
    `Avg_Wage_Regular wage/salaried`, `Avg_Wage_Casual Labour`, `Avg_Wage_Self-employed`,
    Average_Annual_Wage,
    Avg_IR_Weighted_Employment
  ) %>%
  arrange(section_name)

# 24b. Combined "Regular+Casual" vs "Self-employed" breakdown at section level
# --- Employment count + wage bill, grouped by Regular+Casual vs Self-employed ---
section_summary_by_group_target <- section_summary_by_type_target %>%
  mutate(employment_group = employment_group_map[employment_type]) %>%
  group_by(section_name, employment_group) %>%
  summarise(
    Total_Employee_Count    = sum(Total_Employee_Count, na.rm = TRUE),
    Total_Wage_Bill         = sum(Total_Wage_Bill, na.rm = TRUE),
    Total_Weight_Wage_Basis = sum(Total_Weight_Wage_Basis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Average_Annual_Wage = Total_Wage_Bill / Total_Weight_Wage_Basis
  )

# --- IR weighted by employment_group, at NCO x NIC_division x employment_group granularity ---
nco_nic_employee_count_by_type_target <- employee_dataset_status_code_target %>%
  filter(!is.na(employment_type), !is.na(NCO_new), NCO_new != "",
         !is.na(NIC_division), NIC_division != "") %>%
  mutate(employment_group = employment_group_map[employment_type]) %>%
  group_by(NCO_new, NIC_division, employment_group) %>%
  summarise(
    Total_Employee_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

nco_section_table_by_group_target <- nco_nic_employee_count_by_type_target %>%
  left_join(nco_ir, by = c("NCO_new" = "NCO_code")) %>%
  left_join(division_section_nas, by = "NIC_division") %>%
  filter(!is.na(IR))

section_ir_by_group_target <- nco_section_table_by_group_target %>%
  group_by(section_name, employment_group) %>%
  summarise(
    Avg_IR_Weighted_Employment = sum(IR * Total_Employee_Count, na.rm = TRUE) /
      sum(Total_Employee_Count, na.rm = TRUE),
    .groups = "drop"
  )

# --- Pivot each to wide, then bolt onto section_summary_wide_target ---
section_group_employee_wide_target <- section_summary_by_group_target %>%
  select(section_name, employment_group, Total_Employee_Count) %>%
  pivot_wider(names_from = employment_group, values_from = Total_Employee_Count,
              names_prefix = "Total_Employment_")

section_group_wage_wide_target <- section_summary_by_group_target %>%
  select(section_name, employment_group, Average_Annual_Wage) %>%
  pivot_wider(names_from = employment_group, values_from = Average_Annual_Wage,
              names_prefix = "Avg_Wage_")

section_group_ir_wide_target <- section_ir_by_group_target %>%
  pivot_wider(names_from = employment_group, values_from = Avg_IR_Weighted_Employment,
              names_prefix = "Avg_IR_Weighted_Employment_")

section_summary_wide_target <- section_summary_wide_target %>%
  left_join(section_group_employee_wide_target, by = "section_name") %>%
  left_join(section_group_wage_wide_target, by = "section_name") %>%
  left_join(section_group_ir_wide_target, by = "section_name") %>%
  rename(
    `Total Employment (Regular+Casual)` = Total_Employment_Regular_Casual,
    `Total Employment (Self-employed)`  = Total_Employment_Self_Employed,
    `Annual Avg Wage (Regular+Casual)`  = Avg_Wage_Regular_Casual,
    `Annual Avg Wage (Self-employed)`   = Avg_Wage_Self_Employed,
    `Avg IR Weighted (Regular+Casual)`  = Avg_IR_Weighted_Employment_Regular_Casual,
    `Avg IR Weighted (Self-employed)`   = Avg_IR_Weighted_Employment_Self_Employed
  )

#labour cost
section_labor_cost_target <- section_summary_wide_target %>%
  mutate(
    `Labor Cost (Regular+Casual)` =
      `Total Employment (Regular+Casual)` *
      `Annual Avg Wage (Regular+Casual)` *
      `Avg IR Weighted (Regular+Casual)`,
    
    `Labor Cost (Self-employed)` =
      `Total Employment (Self-employed)` *
      `Annual Avg Wage (Self-employed)` *
      `Avg IR Weighted (Self-employed)`
  )
section_labor_cost_target %>%
  mutate(
    `Labor Cost (Regular+Casual) Cr` = `Labor Cost (Regular+Casual)` / 1e7,
    `Labor Cost (Self-employed) Cr` = `Labor Cost (Self-employed)` / 1e7
  ) %>%
  select(
    section_name,
    `Labor Cost (Regular+Casual) Cr`,
    `Labor Cost (Self-employed) Cr`
  )

#gva - for the purpose of this script, ratios are sourced from NAS 2023-24 base year 2022-23, 
#for replicating this exercise across different years, 
#get ratios of CoE to overall GVA or OS/MI to overall GVA from NAS publications 
gva_ratios <- data.frame(
  section_name = c(
    "Agriculture, forestry & fishing",
    "Mining & quarrying",
    "Manufacturing",
    "Electricity, gas, water supply and other utility services",
    "Construction",
    "Trade, repair, hotels and restaurants",
    "Transport, storage, communication & services related to broadcasting",
    "Financial services",
    "Real estate, ownership of dwelling and professional services",
    "Public administration and defence",
    "Other services"
  ),
  CE_to_GVA = c(
    0.14, 0.20, 0.29, 0.09, 0.53,
    0.26, 0.31, 0.34, 0.42, 0.81, 0.65
  ),
  OS_MI_to_GVA = c(
    0.87, 0.74, 0.55, 0.47, 0.20,
    0.72, 0.79, NA, 0.67, 0, 0.68
  )
)

section_gva_target <- section_labor_cost_target %>%
  left_join(gva_ratios, by = "section_name") %>%
  mutate(
    `GVA (Regular+Casual)` = `Labor Cost (Regular+Casual)` / CE_to_GVA,
    `GVA (Self-employed)` = case_when(
      section_name == "Public administration and defence" ~ 0,
      section_name == "Financial services" ~ NA_real_,
      TRUE ~ `Labor Cost (Self-employed)` / OS_MI_to_GVA
    ),
    `Total GVA` = case_when(
      section_name %in% c("Financial services", "Public administration and defence") ~ `GVA (Regular+Casual)`,
      TRUE ~ `GVA (Regular+Casual)` + `GVA (Self-employed)`
    ),
    `GVA (Regular+Casual) Cr` = `GVA (Regular+Casual)` / 1e7,
    `GVA (Self-employed) Cr`  = `GVA (Self-employed)` / 1e7,
    `Total GVA Cr`            = `Total GVA` / 1e7
  )

# ===============================================================================================
# SUBSECTION ROLLUP ADD-ON
# ------------------------------------------------------------------------------------------------
# Purpose: your NAS crosswalk file has TWO levels of granularity in one sheet:
#   - whole-number rows (S. No. = 1, 2, 3, ... 11)   -> the 11 broad categories
#   - x.y rows          (S. No. = 1.1, 1.2, 6.1, ...) -> subsections, but ONLY for categories
#                                                        1 (Agriculture), 6 (Trade/repair/hotels),
#                                                        and 7 (Transport). The other 8 categories
#                                                        have no subsection rows at all.
#
# This add-on:
#   1. Splits the ONE crosswalk sheet into two maps: division -> section (broad, unchanged logic)
#      and division -> subsection (only for divisions that actually have an x.y row).
#   2. Reruns the section-level rollup logic (steps 21-24b in your script) at the subsection
#      level, using inner_join against the subsection map so divisions with no subsection simply
#      produce no subsection row (rather than an NA row).
#   3. Stitches section rows + their subsection rows (if any) into one ordered, combined table
#      matching the layout you want: category row, then its subsections directly under it.
#
# Assumes: division_section_path now points at a crosswalk file that includes BOTH the whole-
# number rows and the x.y rows (i.e. the file you uploaded), and that principal_status,
# division_summary_by_type_target/nontarget, division_summary_target/nontarget,
# nco_division_table_target, division_section_nas, pad_left_zero(), and employment_group_map
# already exist from your main script (this runs AFTER those steps).
# ===============================================================================================

# -----------------------------------------------------------------------------------------------
# 1. BUILD THE TWO CROSSWALK MAPS FROM THE SAME SOURCE FILE
# -----------------------------------------------------------------------------------------------
division_crosswalk_full <- read_excel(division_section_path, col_types = "text") %>%
  rename(
    S_No         = `S. No.`,
    section_name = Category,
    NIC_division = `Division Code`
  ) %>%
  mutate(
    NIC_division  = pad_left_zero(trimws(NIC_division), 2),
    section_name  = trimws(section_name),
    S_No          = trimws(S_No),
    is_subsection = grepl("\\.", S_No)
  )

# Broad (11-category) map — same logic you already have; re-derived here so it stays in sync
# with division_crosswalk_full instead of a separately-read file.
division_section_nas <- division_crosswalk_full %>%
  filter(!is_subsection) %>%
  distinct(NIC_division, .keep_all = TRUE) %>%
  select(NIC_division, section_name)

# Subsection map — ONLY divisions that appear in an x.y row end up here. Divisions belonging to
# categories with no subsections (Mining, Manufacturing, Electricity, Construction, Financial
# services, Real estate, Public admin, Other services) are simply absent from this table.
division_subsection_nas <- division_crosswalk_full %>%
  filter(is_subsection) %>%
  distinct(NIC_division, .keep_all = TRUE) %>%
  select(NIC_division, subsection_name = section_name, subsection_no = S_No)

# Subsection -> parent category lookup, for ordering the combined table later.
subsection_parent_map <- division_crosswalk_full %>%
  filter(is_subsection) %>%
  distinct(subsection_no = S_No, subsection_name = section_name) %>%
  mutate(parent_no = sub("\\..*$", "", subsection_no))

category_order <- division_crosswalk_full %>%
  filter(!is_subsection) %>%
  distinct(S_No, section_name) %>%
  mutate(S_No_num = as.numeric(S_No)) %>%
  arrange(S_No_num)

# ===============================================================================================
# 2. TARGET BRANCH — subsection-level rollup (mirrors steps 22-24b, grouped by subsection_name)
# ===============================================================================================

# --- broken out by employment type ---
subsection_summary_by_type_target <- division_summary_by_type_target %>%
  inner_join(division_subsection_nas, by = "NIC_division") %>%
  group_by(subsection_name, employment_type) %>%
  summarise(
    Total_Employee_Count    = sum(Total_Employee_Count, na.rm = TRUE),
    Total_Wage_Bill         = sum(Total_Wage_Bill, na.rm = TRUE),
    Total_Weight_Wage_Basis = sum(Total_Weight_Wage_Basis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Average_Annual_Wage = Total_Wage_Bill / Total_Weight_Wage_Basis) %>%
  arrange(subsection_name, employment_type)

# --- overall (all 3 employment types combined) ---
subsection_summary_target <- division_summary_target %>%
  inner_join(division_subsection_nas, by = "NIC_division") %>%
  group_by(subsection_name) %>%
  summarise(
    Total_Employee_Count    = sum(Total_Employee_Count, na.rm = TRUE),
    Total_Wage_Bill         = sum(Total_Wage_Bill, na.rm = TRUE),
    Total_Weight_Wage_Basis = sum(Total_Weight_Wage_Basis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Average_Annual_Wage = Total_Wage_Bill / Total_Weight_Wage_Basis) %>%
  arrange(subsection_name)

# --- IR weighted by subsection (occupation-level IR, not type-specific) ---
nco_subsection_table_target <- nco_division_table_target %>%
  filter(!is.na(IR)) %>%
  inner_join(division_subsection_nas, by = "NIC_division")

subsection_ir_target <- nco_subsection_table_target %>%
  group_by(subsection_name) %>%
  summarise(
    Avg_IR_Weighted_Employment = sum(IR * Total_Employee_Count, na.rm = TRUE) /
      sum(Total_Employee_Count, na.rm = TRUE),
    .groups = "drop"
  )

subsection_summary_target <- subsection_summary_target %>%
  left_join(subsection_ir_target, by = "subsection_name")

# --- wide format (mirrors section_summary_wide_target) ---
subsection_employee_wide_target <- subsection_summary_by_type_target %>%
  select(subsection_name, employment_type, Total_Employee_Count) %>%
  pivot_wider(names_from = employment_type, values_from = Total_Employee_Count, values_fill = 0)

subsection_wage_wide_target <- subsection_summary_by_type_target %>%
  select(subsection_name, employment_type, Average_Annual_Wage) %>%
  pivot_wider(names_from = employment_type, values_from = Average_Annual_Wage, names_prefix = "Avg_Wage_")

subsection_summary_wide_target <- subsection_employee_wide_target %>%
  left_join(subsection_wage_wide_target, by = "subsection_name") %>%
  left_join(
    subsection_summary_target %>%
      select(subsection_name, Total_Employee_Count, Average_Annual_Wage, Avg_IR_Weighted_Employment),
    by = "subsection_name"
  ) %>%
  select(
    subsection_name,
    `Regular wage/salaried`, `Casual Labour`, `Self-employed`,
    Total_Employee_Count,
    `Avg_Wage_Regular wage/salaried`, `Avg_Wage_Casual Labour`, `Avg_Wage_Self-employed`,
    Average_Annual_Wage,
    Avg_IR_Weighted_Employment
  ) %>%
  arrange(subsection_name)

# --- 24b equivalent: Regular+Casual vs Self-employed, at subsection level ---
subsection_summary_by_group_target <- subsection_summary_by_type_target %>%
  mutate(employment_group = employment_group_map[employment_type]) %>%
  group_by(subsection_name, employment_group) %>%
  summarise(
    Total_Employee_Count    = sum(Total_Employee_Count, na.rm = TRUE),
    Total_Wage_Bill         = sum(Total_Wage_Bill, na.rm = TRUE),
    Total_Weight_Wage_Basis = sum(Total_Weight_Wage_Basis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Average_Annual_Wage = Total_Wage_Bill / Total_Weight_Wage_Basis)

nco_nic_employee_count_by_type_target <- employee_dataset_status_code_target %>%
  filter(!is.na(employment_type), !is.na(NCO_new), NCO_new != "",
         !is.na(NIC_division), NIC_division != "") %>%
  mutate(employment_group = employment_group_map[employment_type]) %>%
  group_by(NCO_new, NIC_division, employment_group) %>%
  summarise(Total_Employee_Count = sum(weight, na.rm = TRUE), .groups = "drop")

nco_subsection_table_by_group_target <- nco_nic_employee_count_by_type_target %>%
  left_join(nco_ir, by = c("NCO_new" = "NCO_code")) %>%
  inner_join(division_subsection_nas, by = "NIC_division") %>%
  filter(!is.na(IR))

subsection_ir_by_group_target <- nco_subsection_table_by_group_target %>%
  group_by(subsection_name, employment_group) %>%
  summarise(
    Avg_IR_Weighted_Employment = sum(IR * Total_Employee_Count, na.rm = TRUE) /
      sum(Total_Employee_Count, na.rm = TRUE),
    .groups = "drop"
  )

subsection_group_employee_wide_target <- subsection_summary_by_group_target %>%
  select(subsection_name, employment_group, Total_Employee_Count) %>%
  pivot_wider(names_from = employment_group, values_from = Total_Employee_Count,
              names_prefix = "Total_Employment_")

subsection_group_wage_wide_target <- subsection_summary_by_group_target %>%
  select(subsection_name, employment_group, Average_Annual_Wage) %>%
  pivot_wider(names_from = employment_group, values_from = Average_Annual_Wage,
              names_prefix = "Avg_Wage_")

subsection_group_ir_wide_target <- subsection_ir_by_group_target %>%
  pivot_wider(names_from = employment_group, values_from = Avg_IR_Weighted_Employment,
              names_prefix = "Avg_IR_Weighted_Employment_")

subsection_summary_wide_target <- subsection_summary_wide_target %>%
  left_join(subsection_group_employee_wide_target, by = "subsection_name") %>%
  left_join(subsection_group_wage_wide_target, by = "subsection_name") %>%
  left_join(subsection_group_ir_wide_target, by = "subsection_name") %>%
  rename(
    `Total Employment (Regular+Casual)` = Total_Employment_Regular_Casual,
    `Total Employment (Self-employed)`  = Total_Employment_Self_Employed,
    `Annual Avg Wage (Regular+Casual)`  = Avg_Wage_Regular_Casual,
    `Annual Avg Wage (Self-employed)`   = Avg_Wage_Self_Employed,
    `Avg IR Weighted (Regular+Casual)`  = Avg_IR_Weighted_Employment_Regular_Casual,
    `Avg IR Weighted (Self-employed)`   = Avg_IR_Weighted_Employment_Self_Employed
  )
# ===============================================================================================
# 4. COMBINE CATEGORY + SUBSECTION ROWS INTO ONE ORDERED TABLE (matches your sample layout)
# ===============================================================================================
build_combined_summary <- function(section_wide, subsection_wide, name_col_section = "section_name",
                                   name_col_subsection = "subsection_name") {
  rows <- list()
  for (i in seq_len(nrow(category_order))) {
    cat_no   <- category_order$S_No[i]
    cat_name <- category_order$section_name[i]
    
    cat_row <- section_wide %>%
      filter(.data[[name_col_section]] == cat_name) %>%
      rename(row_name = !!name_col_section) %>%
      mutate(S_No = cat_no, level = "section")
    rows[[length(rows) + 1]] <- cat_row
    
    subs <- subsection_parent_map %>% filter(parent_no == cat_no) %>% arrange(subsection_no)
    if (nrow(subs) > 0) {
      for (j in seq_len(nrow(subs))) {
        sub_row <- subsection_wide %>%
          filter(.data[[name_col_subsection]] == subs$subsection_name[j]) %>%
          rename(row_name = !!name_col_subsection) %>%
          mutate(S_No = subs$subsection_no[j], level = "subsection")
        rows[[length(rows) + 1]] <- sub_row
      }
    }
  }
  bind_rows(rows) %>% relocate(S_No, level, row_name)
}

combined_summary_wide_target <- build_combined_summary(section_summary_wide_target, subsection_summary_wide_target)

# ===============================================================================================
# DATA-PRODUCING OCCUPATION CHARACTERISTICS
# ===============================================================================================

data_producing_characteristics_target <- employee_dataset_status_code_target %>%
  mutate(
    
    # =============================================================================================
    # EDUCATION
    # =============================================================================================
    
    # -----------------------------
    # General education: gedu_lvl
    # -----------------------------
    
    education_general = case_when(
      gedu_lvl == "01"  ~ "Not literate",
      gedu_lvl == "02"  ~ "Literate without formal schooling - EGS/NFEC/AEC",
      gedu_lvl == "03"  ~ "Literate without formal schooling - TLC",
      gedu_lvl == "04"  ~ "Literate without formal schooling - Others",
      gedu_lvl == "05"  ~ "Literate - Below primary",
      gedu_lvl == "06"  ~ "Literate - Primary",
      gedu_lvl == "07"  ~ "Literate - Middle",
      gedu_lvl == "08"  ~ "Literate - Secondary",
      gedu_lvl == "10" ~ "Literate - Higher secondary",
      gedu_lvl == "11" ~ "Literate - Diploma/Certificate course",
      gedu_lvl == "12" ~ "Literate - Graduate",
      gedu_lvl == "13" ~ "Literate - Postgraduate and above",
      TRUE ~ NA_character_
    ),
    
    # -----------------------------
    # Technical education: tedu_lvl
    # -----------------------------
    
    education_technical = case_when(
      tedu_lvl == "01"  ~ "No technical education",
      tedu_lvl == "02"  ~ "Technical degree - Agriculture",
      tedu_lvl == "03"  ~ "Technical degree - Engineering/Technology",
      tedu_lvl == "04"  ~ "Technical degree - Medicine",
      tedu_lvl == "05"  ~ "Technical degree - Crafts",
      tedu_lvl == "06"  ~ "Technical degree - Other subjects",
      tedu_lvl == "07"  ~ "Diploma/Certificate below graduate - Agriculture",
      tedu_lvl == "08"  ~ "Diploma/Certificate below graduate - Engineering/Technology",
      tedu_lvl == "09"  ~ "Diploma/Certificate below graduate - Medicine",
      tedu_lvl == "10" ~ "Diploma/Certificate below graduate - Crafts",
      tedu_lvl == "11" ~ "Diploma/Certificate below graduate - Other subjects",
      tedu_lvl == "12" ~ "Diploma/Certificate graduate and above - Agriculture",
      tedu_lvl == "13" ~ "Diploma/Certificate graduate and above - Engineering/Technology",
      tedu_lvl == "14" ~ "Diploma/Certificate graduate and above - Medicine",
      tedu_lvl == "15" ~ "Diploma/Certificate graduate and above - Crafts",
      tedu_lvl == "16" ~ "Diploma/Certificate graduate and above - Other subjects",
      
      TRUE ~ NA_character_
    ),
    
    # -----------------------------
    # Vocational/technical training: voc
    # -----------------------------
    
    vocational_training = case_when(
      voc == "01" ~ "Formal vocational/technical training",
      voc == "02" ~ "Hereditary",
      voc == "03" ~ "Self-learning",
      voc == "04" ~ "Learning on the job",
      voc == "05" ~ "Others",
      voc == "06" ~ "Did not receive any vocational/technical training",
      TRUE ~ NA_character_
    ),
    
    
    # =============================================================================================
    # TRAINING
    # =============================================================================================
    
    # -----------------------------
    # Field of training: voc_fld
    # -----------------------------
    
    training_field = case_when(
      voc_fld == "01"  ~ "Aerospace and aviation",
      voc_fld == "02"  ~ "Agriculture, non-crop based agriculture, food processing",
      voc_fld == "03"  ~ "Allied manufacturing - gems and jewellery, leather, rubber, furniture and fittings, printing",
      voc_fld == "04"  ~ "Artisan/craftsman/handicraft/creative arts and cottage based production",
      voc_fld == "05"  ~ "Automotive",
      voc_fld == "06"  ~ "Beauty and wellness",
      voc_fld == "07"  ~ "Chemical engineering, hydrocarbons, chemicals and petrochemicals",
      voc_fld == "08"  ~ "Civil engineering - construction, plumbing, paints and coatings",
      voc_fld == "09"  ~ "Electrical, power and electronics",
      voc_fld == "10" ~ "Healthcare and life sciences",
      voc_fld == "11" ~ "Hospitality and tourism",
      voc_fld == "12" ~ "Iron and steel, mining, earthmoving and infra building",
      voc_fld == "13" ~ "IT-ITeS",
      voc_fld == "14" ~ "Logistics",
      voc_fld == "15" ~ "Mechanical engineering - capital goods, strategic manufacturing",
      voc_fld == "16" ~ "Media-journalism, mass communication and entertainment",
      voc_fld == "17" ~ "Office and business related work",
      voc_fld == "18" ~ "Security",
      voc_fld == "19" ~ "Telecom",
      voc_fld == "20" ~ "Textiles and handlooms, apparels",
      voc_fld == "21" ~ "Work related to childcare, nutrition, pre-school and crèche",
      voc_fld == "99" ~ "Other",
      TRUE ~ NA_character_
    ),
    
    # -----------------------------
    # Type of training: voc_typ
    # -----------------------------
    
    training_type = case_when(
      voc_typ == "01" ~ "On the job",
      voc_typ == "02" ~ "Other than on the job - Part time",
      voc_typ == "03" ~ "Other than on the job - Full time",
      TRUE ~ NA_character_
    ),
    
    
    # =============================================================================================
    # EMPLOYMENT
    # =============================================================================================
    
    # -----------------------------
    # Location of workplace
    # -----------------------------
    
    workplace_location = case_when(
      ocu_source == "pas" ~ trimws(as.character(loc_pas)),
      ocu_source == "sas" ~ trimws(as.character(loc_sas)),
      TRUE ~ NA_character_
    ),
    
    workplace_location = case_when(
      workplace_location == "10" ~ "Rural - Own dwelling unit",
      workplace_location == "11" ~ "Rural - Structure attached to own dwelling unit",
      workplace_location == "12" ~ "Rural - Open area adjacent to own dwelling unit",
      workplace_location == "13" ~ "Rural - Detached structure adjacent to own dwelling unit",
      workplace_location == "14" ~ "Rural - Own enterprise/unit/office/shop away from own dwelling",
      workplace_location == "15" ~ "Rural - Employer's dwelling unit",
      workplace_location == "16" ~ "Rural - Employer's enterprise/unit/office/shop outside employer's dwelling",
      workplace_location == "17" ~ "Rural - Street with fixed location",
      workplace_location == "18" ~ "Rural - Construction site",
      workplace_location == "19" ~ "Rural - Others",
      workplace_location == "20" ~ "Urban - Own dwelling unit",
      workplace_location == "21" ~ "Urban - Structure attached to own dwelling unit",
      workplace_location == "22" ~ "Urban - Open area adjacent to own dwelling unit",
      workplace_location == "23" ~ "Urban - Detached structure adjacent to own dwelling unit",
      workplace_location == "24" ~ "Urban - Own enterprise/unit/office/shop away from own dwelling",
      workplace_location == "25" ~ "Urban - Employer's dwelling unit",
      workplace_location == "26" ~ "Urban - Employer's enterprise/unit/office/shop outside employer's dwelling",
      workplace_location == "27" ~ "Urban - Street with fixed location",
      workplace_location == "28" ~ "Urban - Construction site",
      workplace_location == "29" ~ "Urban - Others",
      workplace_location == "99" ~ "No fixed workplace",
      
      TRUE ~ NA_character_
    ),
    
    # -----------------------------
    # Enterprise type
    # -----------------------------
    
    enterprise_type = case_when(
      ocu_source == "pas" ~ trimws(as.character(etyp_pas)),
      ocu_source == "sas" ~ trimws(as.character(etyp_sas)),
      TRUE ~ NA_character_
    ),
    
    enterprise_type = case_when(
      enterprise_type == "01" ~ "Proprietary - Male",
      enterprise_type == "02" ~ "Proprietary - Female",
      enterprise_type == "03" ~ "Partnership - Members from same household",
      enterprise_type == "04" ~ "Partnership - Members from different households",
      enterprise_type == "05" ~ "Government/Local body",
      enterprise_type == "06" ~ "Public Sector Enterprise",
      enterprise_type == "07" ~ "Autonomous Body",
      enterprise_type == "08" ~ "Public/Private limited company",
      enterprise_type == "10" ~ "Co-operative society",
      enterprise_type == "11" ~ "Trust/Other non-profit institution",
      enterprise_type == "12" ~ "Employer's household",
      enterprise_type == "19" ~ "Others",
      TRUE ~ NA_character_
    )
  )


# ===============================================================================================
# EDUCATION ANALYSIS
# ===============================================================================================

education_pivot_target <- data_producing_characteristics_target %>%
  select(
    NCO_new,
    weight,
    education_general,
    education_technical,
    vocational_training
  ) %>%
  pivot_longer(
    cols = c(
      education_general,
      education_technical,
      vocational_training
    ),
    names_to = "Subsection",
    values_to = "Category"
  ) %>%
  filter(
    !is.na(Category),
    Category != ""
  ) %>%
  group_by(
    NCO_new,
    Subsection,
    Category
  ) %>%
  summarise(
    Employment_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    NCO_new,
    Subsection,
    Category
  )
education_wide_target <- education_pivot_target %>%
  mutate(
    Column_Name = paste(Subsection, Category, sep = "_")
  ) %>%
  select(
    NCO_new,
    Column_Name,
    Employment_Count
  ) %>%
  pivot_wider(
    names_from = Column_Name,
    values_from = Employment_Count,
    values_fill = 0
  )


# ===============================================================================================
# TRAINING ANALYSIS
# ===============================================================================================

training_pivot_target <- data_producing_characteristics_target %>%
  select(NCO_new, weight, training_field, training_type) %>%
  mutate(
    training_field = coalesce(training_field, "No training / not applicable"),
    training_type  = coalesce(training_type,  "No training / not applicable")
  ) %>%
  pivot_longer(
    cols = c(
      training_field,
      training_type
    ),
    names_to = "Subsection",
    values_to = "Category"
  ) %>%
  filter(
    !is.na(Category),
    Category != ""
  ) %>%
  group_by(
    NCO_new,
    Subsection,
    Category
  ) %>%
  summarise(
    Employment_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    NCO_new,
    Subsection,
    Category
  )
training_wide_target <- training_pivot_target %>%
  mutate(
    Column_Name = paste(Subsection, Category, sep = "_")
  ) %>%
  select(
    NCO_new,
    Column_Name,
    Employment_Count
  ) %>%
  pivot_wider(
    names_from = Column_Name,
    values_from = Employment_Count,
    values_fill = 0
  )

# ===============================================================================================
# EMPLOYMENT ANALYSIS
# ===============================================================================================

employment_pivot_target <- data_producing_characteristics_target %>%
  select(
    NCO_new,
    weight,
    workplace_location,
    enterprise_type
  ) %>%
  pivot_longer(
    cols = c(
      workplace_location,
      enterprise_type
    ),
    names_to = "Subsection",
    values_to = "Category"
  ) %>%
  filter(
    !is.na(Category),
    Category != ""
  ) %>%
  group_by(
    NCO_new,
    Subsection,
    Category
  ) %>%
  summarise(
    Employment_Count = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    NCO_new,
    Subsection,
    Category
  )
employment_wide_target <- employment_pivot_target %>%
  mutate(
    Column_Name = paste(Subsection, Category, sep = "_")
  ) %>%
  select(
    NCO_new,
    Column_Name,
    Employment_Count
  ) %>%
  pivot_wider(
    names_from = Column_Name,
    values_from = Employment_Count,
    values_fill = 0
  )

# ===============================================================================================
# WRITE WIDE TABLES AS CSV
# ===============================================================================================
output_dir <- "/Users/Work/Desktop/data valuation/diagnosing"

write.csv(
  education_wide_target,
  file.path(output_dir, "education_analysis_NCO_wide.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  training_wide_target,
  file.path(output_dir, "training_analysis_NCO_wide.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  employment_wide_target,
  file.path(output_dir, "employment_analysis_NCO_wide.csv"),
  row.names = FALSE,
  na = ""
)

subset_occ_codes <- c("413", "252", "251", "422", "331", "351", "311")

# Check the codes exist and are the right type
setdiff(subset_occ_codes, unique(as.character(data_producing_characteristics_target$NCO_new)))

subset_df <- data_producing_characteristics_target %>%
  mutate(NCO_new = trimws(as.character(NCO_new))) %>%
  filter(NCO_new %in% subset_occ_codes)

build_wide <- function(df, vars, na_label = "Not reported / not applicable") {
  df %>%
    select(NCO_new, weight, all_of(vars)) %>%
    mutate(across(all_of(vars), ~ coalesce(na_if(as.character(.x), ""), na_label))) %>%
    pivot_longer(all_of(vars), names_to = "Subsection", values_to = "Category") %>%
    group_by(NCO_new, Subsection, Category) %>%
    summarise(Employment_Count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(Column_Name = paste(Subsection, Category, sep = "_")) %>%
    select(NCO_new, Column_Name, Employment_Count) %>%
    pivot_wider(names_from = Column_Name, values_from = Employment_Count, values_fill = 0)
}

education_wide_subset  <- build_wide(subset_df, c("education_general", "education_technical", "vocational_training"))
training_wide_subset   <- build_wide(subset_df, c("training_field", "training_type"))
employment_wide_subset <- build_wide(subset_df, c("workplace_location", "enterprise_type"))

# Sanity checks: all 7 codes present, and row totals match across tables
sapply(list(education_wide_subset, training_wide_subset, employment_wide_subset), nrow)

subset_df %>% group_by(NCO_new) %>% summarise(total = sum(weight, na.rm = TRUE))
training_wide_subset %>% transmute(NCO_new, total = rowSums(across(-NCO_new)) / 2)  # 2 vars per NCO

write.csv(education_wide_subset,  file.path(output_dir, "education_analysis_NCO_wide_subset.csv"),  row.names = FALSE, na = "")
write.csv(training_wide_subset,   file.path(output_dir, "training_analysis_NCO_wide_subset.csv"),   row.names = FALSE, na = "")
write.csv(employment_wide_subset, file.path(output_dir, "employment_analysis_NCO_wide_subset.csv"), row.names = FALSE, na = "")