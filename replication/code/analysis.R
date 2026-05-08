# ============================================================
# Communication Technology and Immigrant Assimilation
# Clean Analysis Script — using usa_00013 variables only
# Variables confirmed: YEAR, AGE, SEX, MARST, BPL, BPLD,
# CITIZEN, YRIMMIG, EDUC, EDUCD, EMPSTAT, OCC,
# INCTOT, POVERTY, PERWT, SPEAKENG, STATEFIP, GQ
# ============================================================

# ============================================================
# 0. PACKAGES
# ============================================================

library(haven)
library(dplyr)
library(ggplot2)
library(ipumsr)
library(fixest)
library(readr)
library(tidyr)
library(countrycode)
library(broom)
library(stringr)

# ============================================================
# 1. LOAD DATA
# ============================================================

setwd("~/Downloads/")

ddi    <- read_ipums_ddi("usa_00013.xml")
df_raw <- read_ipums_micro(ddi)
wdi_raw <- read_csv("WB_WDI_IT_MLT_MAIN_P2_WIDEF.csv")

View(df_raw)

# ============================================================
# 2. CLEAN IPUMS
# ============================================================

# Step 1: extract BPL labels BEFORE zapping
bpl_labels <- df_raw %>%
  select(YEAR, SERIAL, PERNUM, BPL) %>%
  mutate(country_name = as.character(as_factor(BPL))) %>%
  select(YEAR, SERIAL, PERNUM, country_name)

# Step 2: full cleaning pipeline
df <- df_raw %>%
  zap_labels() %>%
  filter(BPL >= 150) %>%
  filter(YEAR %in% c(1970, 1980, 1990, 2000)) %>%
  filter(AGE >= 18, AGE <= 65) %>%
  filter(GQ %in% c(1, 2)) %>%
  filter(EMPSTAT %in% c(1, 2)) %>%
  mutate(
    INCTOT  = na_if(INCTOT,  9999999),
    INCTOT  = na_if(INCTOT,  9999998),
    YRIMMIG = na_if(YRIMMIG, 0)
  ) %>%
  filter(!is.na(YRIMMIG)) %>%
  filter(YRIMMIG >= 1950, YRIMMIG <= 1995) %>%
  mutate(YSM = YEAR - YRIMMIG) %>%
  filter(YSM >= 0) %>%
  mutate(
    AGE2       = AGE^2,
    married    = as.integer(MARST %in% c(1, 2)),
    male       = as.integer(SEX == 1),
    college    = as.integer(EDUCD >= 101),
    english    = case_when(
      SPEAKENG %in% c(4, 5, 6) ~ 1L,
      SPEAKENG %in% c(1, 2, 3) ~ 0L,
      TRUE                     ~ NA_integer_
    ),
    log_income = if_else(INCTOT > 0, log(INCTOT), NA_real_)
  ) %>%
  mutate(
    ysm_bin = case_when(
      YSM >= 0  & YSM <  5 ~ "0_5",
      YSM >= 5  & YSM < 10 ~ "5_10",
      YSM >= 10 & YSM < 15 ~ "10_15",
      YSM >= 15 & YSM < 20 ~ "15_20",
      YSM >= 20 & YSM < 25 ~ "20_25",
      YSM >= 25             ~ "25plus",
      TRUE                  ~ NA_character_
    ),
    ysm_bin = factor(ysm_bin,
                     levels = c("0_5","5_10","10_15",
                                "15_20","20_25","25plus"))
  ) %>%
  filter(!is.na(ysm_bin)) %>%
  # Join labels extracted before zapping
  left_join(bpl_labels, by = c("YEAR", "SERIAL", "PERNUM")) %>%
  mutate(
    iso3 = countrycode(country_name,
                       origin      = "country.name",
                       destination = "iso3c",
                       warn        = FALSE),
    continent = countrycode(iso3,
                            origin      = "iso3c",
                            destination = "continent",
                            warn        = FALSE)
  ) %>%
  filter(!is.na(iso3))

cat("Rows after cleaning:", nrow(df), "\n")
cat("Unique countries:", n_distinct(df$iso3), "\n")

# ============================================================
# 3. CLEAN WDI TELEPHONE DATA
# ============================================================

wdi <- wdi_raw %>%
  select(REF_AREA, matches("^\\d{4}$")) %>%
  pivot_longer(
    cols      = -REF_AREA,
    names_to  = "wb_year",
    values_to = "phone_rate"
  ) %>%
  mutate(
    wb_year    = as.integer(wb_year),
    phone_rate = as.numeric(phone_rate)
  ) %>%
  filter(!is.na(phone_rate))

cat("WDI cleaned. Rows:", nrow(wdi), "\n")

# ============================================================
# 4. CONSTRUCT TREATMENT VARIABLE
# ============================================================
# Average phone subscriptions per 100 in origin country
# over the 5 years BEFORE arrival year.
# Treatment = 1 if average >= 5.

arrival_combos <- df %>% distinct(iso3, YRIMMIG)

tel_preperiod <- arrival_combos %>%
  left_join(
    wdi %>% rename(iso3 = REF_AREA),
    by = "iso3",
    relationship = "many-to-many"
  ) %>%
  filter(wb_year >= YRIMMIG - 5,
         wb_year <  YRIMMIG) %>%
  group_by(iso3, YRIMMIG) %>%
  summarise(
    tel_avg = mean(phone_rate, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3) %>%
  mutate(treated = as.integer(tel_avg >= 5))

cat("Treatment constructed.\n")
cat("Treated obs:", sum(tel_preperiod$treated == 1), "\n")
cat("Control obs:", sum(tel_preperiod$treated == 0), "\n")

# ============================================================
# 5. MERGE
# ============================================================

df_final <- df %>%
  left_join(tel_preperiod, by = c("iso3", "YRIMMIG")) %>%
  filter(!is.na(treated))

df_final <- df_final %>%
  mutate(
    cohort_bin = case_when(
      YRIMMIG >= 1950 & YRIMMIG <= 1954 ~ "1950-54",
      YRIMMIG >= 1955 & YRIMMIG <= 1959 ~ "1955-59",
      YRIMMIG >= 1960 & YRIMMIG <= 1964 ~ "1960-64",
      YRIMMIG >= 1965 & YRIMMIG <= 1969 ~ "1965-69",
      YRIMMIG >= 1970 & YRIMMIG <= 1974 ~ "1970-74",
      YRIMMIG >= 1975 & YRIMMIG <= 1979 ~ "1975-79",
      YRIMMIG >= 1980 & YRIMMIG <= 1984 ~ "1980-84",
      YRIMMIG >= 1985 & YRIMMIG <= 1989 ~ "1985-89",
      YRIMMIG >= 1990 & YRIMMIG <= 1994 ~ "1990-94",
      YRIMMIG >= 1995                   ~ "1995-99"
    )
  )

df_final <- df_final %>%
  filter(!(iso3 == "IRN" & country_name == "Persian Gulf States, n.s."))

cat("Final sample. Rows:", nrow(df_final), "\n")
cat("Unique countries:", n_distinct(df_final$iso3), "\n")
cat("Treated share:",
    round(mean(df_final$treated), 3), "\n")

# ============================================================
# 6. SUMMARY STATISTICS — TABLE 1
# ============================================================

make_sumstats <- function(data, group_label) {
  data %>%
    summarise(
      group        = group_label,
      english_mean = weighted.mean(english,    PERWT, na.rm = TRUE),
      english_sd   = sd(english,               na.rm = TRUE),
      loginc_mean  = weighted.mean(log_income, PERWT, na.rm = TRUE),
      loginc_sd    = sd(log_income,            na.rm = TRUE),
      age_mean     = weighted.mean(AGE,        PERWT, na.rm = TRUE),
      age_arr      = weighted.mean(AGE - YSM,  PERWT, na.rm = TRUE),
      ysm_mean     = weighted.mean(YSM,        PERWT, na.rm = TRUE),
      married_mean = weighted.mean(married,    PERWT, na.rm = TRUE),
      college_mean = weighted.mean(college,    PERWT, na.rm = TRUE),
      tel_mean     = weighted.mean(tel_avg,    PERWT, na.rm = TRUE),
      n_obs        = n(),
      n_countries  = n_distinct(iso3)
    )
}

tab1 <- bind_rows(
  df_final %>% filter(treated == 0) %>% make_sumstats("Control"),
  df_final %>% filter(treated == 1) %>% make_sumstats("Treated"),
  df_final                           %>% make_sumstats("Full Sample")
)

print(tab1)
write_csv(tab1, "table1_sumstats.csv")

# ============================================================
# 7. MAIN REGRESSIONS — TABLE 2
# ============================================================

# Income
reg_income <- feols(
  log_income ~
    i(ysm_bin, treated, ref = "0_5") +
    AGE + AGE2 + married + male + college
  | iso3^cohort_bin + STATEFIP^YEAR,
  data    = df_final %>% filter(!is.na(log_income)),
  weights = ~PERWT,
  cluster = ~iso3
)

# English
reg_english <- feols(
  english ~
    i(ysm_bin, treated, ref = "0_5") +
    AGE + AGE2 + married + male + college
  | iso3^cohort_bin + STATEFIP^YEAR,
  data    = df_final %>% filter(!is.na(english)),
  weights = ~PERWT,
  cluster = ~iso3
)

summary(reg_income)
summary(reg_english)

# --- Extract coefficients ---
extract_coefs <- function(reg, outcome_label) {
  tidy(reg, conf.int = TRUE) %>%
    filter(str_detect(term, "ysm_bin")) %>%
    mutate(
      bin = str_extract(term,
                        "5_10|10_15|15_20|20_25|25plus"),
      bin = factor(bin,
                   levels = c("5_10","10_15","15_20",
                              "20_25","25plus"),
                   labels = c("[5,10)","[10,15)","[15,20)",
                              "[20,25)","[25,+)")),
      outcome = outcome_label
    ) %>%
    select(outcome, bin, estimate, std.error,
           p.value, conf.low, conf.high)
}

coef_income  <- extract_coefs(reg_income,  "Log Income")
coef_english <- extract_coefs(reg_english, "English Proficiency")

table2 <- bind_rows(coef_income, coef_english)
print(table2)
write_csv(table2, "table2_coefficients.csv")

# ============================================================
# 7b. CONTINUOUS TREATMENT SPECIFICATION
# ============================================================

df_final <- df_final %>%
  mutate(tel_avg_std = (tel_avg - mean(tel_avg, na.rm = TRUE)) /
                        sd(tel_avg,   na.rm = TRUE))

reg_income_cont <- feols(
  log_income ~
    i(ysm_bin, tel_avg_std, ref = "0_5") +
    AGE + AGE2 + married + male + college
  | iso3^cohort_bin + STATEFIP^YEAR,
  data    = df_final %>% filter(!is.na(log_income)),
  weights = ~PERWT,
  cluster = ~iso3
)

reg_english_cont <- feols(
  english ~
    i(ysm_bin, tel_avg_std, ref = "0_5") +
    AGE + AGE2 + married + male + college
  | iso3^cohort_bin + STATEFIP^YEAR,
  data    = df_final %>% filter(!is.na(english)),
  weights = ~PERWT,
  cluster = ~iso3
)

summary(reg_income_cont)
summary(reg_english_cont)

coef_income_cont  <- extract_coefs(reg_income_cont,  "Log Income (Continuous)")
coef_english_cont <- extract_coefs(reg_english_cont, "English Proficiency (Continuous)")

table2_cont <- bind_rows(coef_income_cont, coef_english_cont)
print(table2_cont)
write_csv(table2_cont, "table2_continuous.csv")

p_income_cont <- plot_event_study(
  coef_income_cont,
  "Effect of Origin-Country Telephone Access on Log Income (Continuous)",
  "Coefficient Estimate per 1 SD\nof Phone Rate (Log Points)",
  "result1_cont.png"
)

p_english_cont <- plot_event_study(
  coef_english_cont,
  "Effect of Origin-Country Telephone Access on English Proficiency (Continuous)",
  "Coefficient Estimate per 1 SD\nof Phone Rate (Percentage Points)",
  "result2_cont.png"
)

print(p_income_cont)
print(p_english_cont)

# ============================================================
# 8. EVENT-STUDY PLOTS — EXHIBITS 3 AND 4
# ============================================================

plot_event_study <- function(coef_df, title_str,
                             ylab_str, filename) {
  
  ref_row <- tibble(
    bin       = factor("[0,5)",
                       levels = c("[0,5)","[5,10)","[10,15)",
                                  "[15,20)","[20,25)","[25,+)")),
    estimate  = 0, conf.low = 0, conf.high = 0
  )
  
  plot_df <- coef_df %>%
    select(bin, estimate, conf.low, conf.high) %>%
    mutate(bin = factor(bin,
                        levels = c("[0,5)","[5,10)","[10,15)",
                                   "[15,20)","[20,25)","[25,+)"))) %>%
    bind_rows(ref_row) %>%
    arrange(bin)
  
  p <- ggplot(plot_df,
              aes(x = bin, y = estimate, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = "steelblue", alpha = 0.15) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(color = "steelblue", size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, color = "steelblue",
                  linewidth = 0.6) +
    labs(
      title   = title_str,
      x       = "Years Since Migration",
      y       = ylab_str,
      caption = paste0(
        "95% CI shown. SE clustered at country-of-birth level.\n",
        "Reference bin: [0,5) years. ",
        "Country-of-birth and Census year FE included."
      )
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 8, color = "gray40")
    )
  
  ggsave(filename, p, width = 7, height = 4.5, dpi = 300)
  cat("Saved:", filename, "\n")
  return(p)
}

p_income <- plot_event_study(
  coef_income,
  "Effect of Origin-Country Telephone Access on Log Income",
  "Coefficient Estimate (Log Points)",
  "result1.png"
)

p_english <- plot_event_study(
  coef_english,
  "Effect of Origin-Country Telephone Access on English Proficiency",
  "Coefficient Estimate (Percentage Points)",
  "result2.png"
)

print(p_income)
print(p_english)

# ============================================================
# 9. HETEROGENEITY — TABLE 3 (with safe wrapper)
# ============================================================

run_reg <- function(data, outcome) {
  feols(
    as.formula(paste(outcome,
      "~ i(ysm_bin, treated, ref = '0_5') + AGE + AGE2 + married + male + college",
      "| iso3^cohort_bin + STATEFIP^YEAR")),
    data    = data,
    weights = ~PERWT,
    cluster = ~iso3
  )
}

get_25plus <- function(reg) {
  tidy(reg) %>%
    filter(str_detect(term, "25plus")) %>%
    select(estimate, std_error = std.error, p_value = p.value)
}

# Safe version that skips subgroups with too few observations
run_subgroup_safe <- function(data_inc, data_eng, label) {
  
  # Check minimum observations
  if (nrow(data_inc) < 100 || nrow(data_eng) < 100) {
    cat("Skipping", label, "— too few observations\n")
    return(tibble(
      subgroup      = label,
      estimate_inc  = NA, std_error_inc = NA,
      p_value_inc   = NA, n_inc         = nrow(data_inc),
      estimate_eng  = NA, std_error_eng = NA,
      p_value_eng   = NA, n_eng         = nrow(data_eng)
    ))
  }
  
  # Check treated variation exists in subgroup
  if (n_distinct(data_inc$treated) < 2 ||
      n_distinct(data_eng$treated) < 2) {
    cat("Skipping", label, "— no treatment variation\n")
    return(tibble(
      subgroup      = label,
      estimate_inc  = NA, std_error_inc = NA,
      p_value_inc   = NA, n_inc         = nrow(data_inc),
      estimate_eng  = NA, std_error_eng = NA,
      p_value_eng   = NA, n_eng         = nrow(data_eng)
    ))
  }
  
  tryCatch(
    bind_cols(
      tibble(subgroup = label),
      get_25plus(run_reg(data_inc, "log_income")) %>%
        rename_with(~ paste0(., "_inc")),
      get_25plus(run_reg(data_eng, "english")) %>%
        rename_with(~ paste0(., "_eng"))
    ),
    error = function(e) {
      cat("Error in", label, ":", conditionMessage(e), "\n")
      tibble(
        subgroup      = label,
        estimate_inc  = NA, std_error_inc = NA,
        p_value_inc   = NA, n_inc         = nrow(data_inc),
        estimate_eng  = NA, std_error_eng = NA,
        p_value_eng   = NA, n_eng         = nrow(data_eng)
      )
    }
  )
}

# Final Table 3 — Panel B with only reliable subgroups
table3_clean <- bind_rows(
  
  # Panel A: Education
  run_subgroup_safe(
    df_final %>% filter(college == 1, !is.na(log_income)),
    df_final %>% filter(college == 1, !is.na(english)),
    "College or above"
  ),
  run_subgroup_safe(
    df_final %>% filter(college == 0, !is.na(log_income)),
    df_final %>% filter(college == 0, !is.na(english)),
    "Less than college"
  ),
  
  # Panel B: Region — only well-identified subgroups
  run_subgroup_safe(
    df_final %>% filter(continent == "Europe", !is.na(log_income)),
    df_final %>% filter(continent == "Europe", !is.na(english)),
    "Europe"
  ),
  run_subgroup_safe(
    df_final %>% filter(continent == "Asia", !is.na(log_income)),
    df_final %>% filter(continent == "Asia", !is.na(english)),
    "Asia"
  ),
  
  # Panel C: Occupation proxy — use EMPSTAT
  # employed full time vs part time/unemployed
  run_subgroup_safe(
    df_final %>% filter(EMPSTAT == 1, !is.na(log_income)),
    df_final %>% filter(EMPSTAT == 1, !is.na(english)),
    "Employed"
  ),
  run_subgroup_safe(
    df_final %>% filter(EMPSTAT == 2, !is.na(log_income)),
    df_final %>% filter(EMPSTAT == 2, !is.na(english)),
    "Unemployed"
  )
)

print(table3_clean)
write_csv(table3_clean, "table3_heterogeneity_clean.csv")

# ============================================================
# 9b. HETEROGENEITY — TABLE 3 CONTINUOUS TREATMENT
# ============================================================

run_reg_cont <- function(data, outcome) {
  feols(
    as.formula(paste(outcome,
      "~ i(ysm_bin, tel_avg_std, ref = '0_5') + AGE + AGE2 + married + male + college",
      "| iso3^cohort_bin + STATEFIP^YEAR")),
    data    = data,
    weights = ~PERWT,
    cluster = ~iso3
  )
}

get_25plus_cont <- function(reg) {
  tidy(reg) %>%
    filter(str_detect(term, "25plus")) %>%
    select(estimate, std_error = std.error, p_value = p.value)
}

run_subgroup_cont <- function(data_inc, data_eng, label) {
  if (nrow(data_inc) < 100 || nrow(data_eng) < 100) {
    cat("Skipping", label, "— too few observations\n")
    return(tibble(subgroup = label,
                  estimate_inc = NA, std_error_inc = NA, p_value_inc = NA,
                  estimate_eng = NA, std_error_eng = NA, p_value_eng = NA))
  }
  tryCatch(
    bind_cols(
      tibble(subgroup = label),
      get_25plus_cont(run_reg_cont(data_inc, "log_income")) %>%
        rename_with(~ paste0(., "_inc")),
      get_25plus_cont(run_reg_cont(data_eng, "english")) %>%
        rename_with(~ paste0(., "_eng"))
    ),
    error = function(e) {
      cat("Error in", label, ":", conditionMessage(e), "\n")
      tibble(subgroup = label,
             estimate_inc = NA, std_error_inc = NA, p_value_inc = NA,
             estimate_eng = NA, std_error_eng = NA, p_value_eng = NA)
    }
  )
}

table3_cont <- bind_rows(
  run_subgroup_cont(
    df_final %>% filter(college == 1, !is.na(log_income)),
    df_final %>% filter(college == 1, !is.na(english)),
    "College or above"
  ),
  run_subgroup_cont(
    df_final %>% filter(college == 0, !is.na(log_income)),
    df_final %>% filter(college == 0, !is.na(english)),
    "Less than college"
  ),
  run_subgroup_cont(
    df_final %>% filter(continent == "Europe", !is.na(log_income)),
    df_final %>% filter(continent == "Europe", !is.na(english)),
    "Europe"
  ),
  run_subgroup_cont(
    df_final %>% filter(continent == "Asia", !is.na(log_income)),
    df_final %>% filter(continent == "Asia", !is.na(english)),
    "Asia"
  ),
  run_subgroup_cont(
    df_final %>% filter(EMPSTAT == 1, !is.na(log_income)),
    df_final %>% filter(EMPSTAT == 1, !is.na(english)),
    "Employed"
  ),
  run_subgroup_cont(
    df_final %>% filter(EMPSTAT == 2, !is.na(log_income)),
    df_final %>% filter(EMPSTAT == 2, !is.na(english)),
    "Unemployed"
  )
)

print(table3_cont)
write_csv(table3_cont, "table3_heterogeneity_cont.csv")

# ============================================================
# 10. ROBUSTNESS: VARY THRESHOLD
# ============================================================

run_threshold <- function(thresh) {
  df_rob <- df_final %>%
    mutate(treated = as.integer(tel_avg >= thresh))
  reg <- feols(
    log_income ~ i(ysm_bin, treated, ref = "0_5") +
      AGE + AGE2 + married + male + college | iso3^cohort_bin + STATEFIP^YEAR,
    data    = df_rob %>% filter(!is.na(log_income)),
    weights = ~PERWT,
    cluster = ~iso3
  )
  tidy(reg) %>%
    filter(str_detect(term, "25plus")) %>%
    mutate(threshold = thresh) %>%
    select(threshold, estimate, std.error, p.value)
}

rob <- bind_rows(
  run_threshold(3),
  run_threshold(5),
  run_threshold(10)
)
cat("\nRobustness — varying threshold:\n")
print(rob)
write_csv(rob, "robustness_threshold.csv")


# ============================================================
# PRINT ALL TABLE NUMBERS IN LATEX-READY FORMAT
# ============================================================

cat("\n========== TABLE 1: SUMMARY STATISTICS ==========\n")
tab1 %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(width = Inf)

cat("\n========== TABLE 2: MAIN COEFFICIENTS ==========\n")
table2 %>%
  mutate(
    estimate  = round(estimate,  4),
    std.error = round(std.error, 4),
    p.value   = round(p.value,   3),
    conf.low  = round(conf.low,  4),
    conf.high = round(conf.high, 4),
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  ) %>%
  print(width = Inf)

cat("\n========== TABLE 3: HETEROGENEITY ==========\n")
table3_clean %>%
  mutate(
    stars_inc = case_when(
      p_value_inc < 0.01 ~ "***",
      p_value_inc < 0.05 ~ "**",
      p_value_inc < 0.10 ~ "*",
      TRUE               ~ ""
    ),
    stars_eng = case_when(
      p_value_eng < 0.01 ~ "***",
      p_value_eng < 0.05 ~ "**",
      p_value_eng < 0.10 ~ "*",
      TRUE               ~ ""
    )
  ) %>%
  print(width = Inf)

cat("\n========== ROBUSTNESS ==========\n")
rob %>%
  mutate(
    estimate  = round(estimate,  4),
    std.error = round(std.error, 4),
    p.value   = round(p.value,   3)
  ) %>%
  print(width = Inf)