library(tidyverse)
library(gtsummary)
library(dplyr)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")


# Exercise

tbl_summary(
	nlsy,
	by = sex_cat,
	digits = list(income ~ 3,
								sleep_wkdy ~ 1,
								sleep_wknd ~1),
	include = c(region_cat, sex_cat, race_eth_cat, income,
						 sleep_wkdy, sleep_wknd),
	label = list(
		region_cat ~ "Region",
		race_eth_cat ~ "Race/ethnicity",
		income ~ "Income",
		sleep_wkdy ~ "Hours slept on Weekday",
		sleep_wknd ~ "Hours slept on Weekend"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote() |>
	modify_table_styling(columns = label,
											 rows = label == "Race/ethnicity",
											 footnote = "How NLSY classified participants: https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
	) %>%
	modify_header(label = "**Variable**", p.value = "**P**") %>%
	list(income ~ "(p{10}, p{90})",
				sleep_wkdy ~ "min = {min}; max ={max}",
				sleep_wknd ~ "min = {min}; max ={max}")

