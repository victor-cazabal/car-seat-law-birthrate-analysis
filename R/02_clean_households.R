# ---- cleaning, recodes, household filtering ------------------------------

#fix ADJINC scale (older years came as millions)
clean0 <- raw_all %>%
  mutate(ADJINC = ifelse(YEAR %in% 2006:2012, ADJINC/1e6, ADJINC)) %>%
  #unique household ID across years
  mutate(SERIALNO = paste0(YEAR, SERIALNO)) %>%
  #education recode
  mutate(Education = factor(case_when(
    YEAR <= 2007 & SCHL <= 11 ~ "HS",
    YEAR <= 2007 & SCHL %in% c(12,13) ~ "C",
    YEAR <= 2007 & SCHL %in% c(14,15,16) ~ "M",
    YEAR >  2007 & SCHL <= 19 ~ "HS",
    YEAR >  2007 & SCHL %in% c(20,21) ~ "C",
    YEAR >  2007 & SCHL %in% c(22,23,24) ~ "M",
    TRUE ~ NA_character_
  ), levels = c("HS","C","M"), ordered = FALSE)) %>%
  #hispanic binary
  mutate(Hispanic = case_when(
    HISP == 1 ~ "No",
    HISP %in% 2:24 ~ "Yes",
    TRUE ~ NA_character_
  )) %>%
  #race recode
  mutate(Race = case_when(
    RAC1P == 1 ~ "White",
    RAC1P == 2 ~ "Black",
    RAC1P == 6 ~ "Asian",
    RAC1P %in% c(3,4,5,7,8,9) ~ "Other",
    TRUE ~ NA_character_
  )) %>%
  #vehicle availability binary
  mutate(Vehicle = case_when(
    VEH == 0 ~ "No",
    VEH >= 1 ~ "Yes",
    TRUE ~ NA_character_
  )) %>%
  #state codes to strings
  mutate(ST = case_when(
    ST == "09" ~ "CT",
    ST == "9"  ~ "CT",
    ST == "12" ~ "FL",
    TRUE       ~ ST
  )) %>%
  # 2017 dollars
  mutate(HINCP_2017 = inflate_to_2017(HINCP, YEAR)) %>%
  #drop noisy columns 
  select(-ends_with("_label"), -PUMA, -PWGTP, -WGTP, -HINCP, -ADJINC)

stopifnot("Education" %in% names(clean0))

#remove group quarters / in-law households
clean1 <- clean0 %>%
  mutate(remove_relp = case_when(
    YEAR %in% c(2006,2007) & RELP %in% c(13,14,6) ~ TRUE,
    YEAR >= 2008 & RELP %in% c(17,16,9,8) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  group_by(SERIALNO) %>%
  mutate(hh_remove = any(remove_relp)) %>%
  ungroup() %>%
  filter(!hh_remove) %>%
  select(-remove_relp, -hh_remove)

#keep households with at least one woman 18–35
clean2 <- clean1 %>%
  group_by(SERIALNO) %>%
  filter(any(SEX == 2 & AGEP >= 18 & AGEP <= 35)) %>%
  ungroup()

#remove households with non-biological children
clean3 <- clean2 %>%
  group_by(SERIALNO) %>%
  mutate(has_nonbio_child = any(
    (YEAR %in% 2006:2007 & RELP == 11) |
      (YEAR >= 2008        & RELP %in% c(3,4,14))
  )) %>%
  ungroup() %>%
  filter(!has_nonbio_child) %>%
  select(-has_nonbio_child)

#keep only households where we can identify the target mother and no ambiguous children
clean4 <- clean3 %>%
  group_by(SERIALNO) %>%
  mutate(
    has_target_mother = any(
      (RELP == 0 & SEX == 2 & AGEP >= 18 & AGEP <= 35) |
        (RELP == 0 & SEX == 1 & any(RELP == 1 & SEX == 2 & AGEP >= 18 & AGEP <= 35))
    ),
    has_invalid_child = any(AGEP <= 17 & RELP != 2)
  ) %>%
  ungroup() %>%
  group_by(SERIALNO) %>%
  filter(any(has_target_mother) & !any(has_invalid_child)) %>%
  ungroup() %>%
  select(-has_target_mother, -has_invalid_child)

stopifnot("Education" %in% names(clean4))

#collapse to one row per household, compute income quintile, male presence
households <- clean4 %>%
  group_by(SERIALNO) %>%
  summarize(
    children_ages = list(sort(AGEP[RELP == 2])),
    HINCP_2017    = dplyr::first(HINCP_2017),
    # Use an *ordered* factor temporarily so max() is well-defined
    highest_educ_ord = if (all(is.na(Education))) NA else
      max(factor(Education, levels = c("HS","C","M"), ordered = TRUE), na.rm = TRUE),
    YEAR          = dplyr::first(YEAR),
    ST            = dplyr::first(ST),
    male_present  = as.integer(any(SEX == 1 & AGEP >= 18)),
    .groups = "drop"
  ) %>%
  mutate(
    income_quintile = ntile(HINCP_2017, 5),
    # Convert to a regular (unordered) factor for modeling
    Education = factor(as.character(highest_educ_ord),
                       levels = c("HS","C","M"), ordered = FALSE)
  ) %>%
  select(-highest_educ_ord)

#eligible women 18–35, joined to household summary
eligible <- clean4 %>% filter(SEX == 2, AGEP >= 18, AGEP <= 35) %>%
  select(-Education)

wide_sample <- eligible %>%
  left_join(households, by = "SERIALNO") %>%
  rename(woman_age = AGEP) %>%
  select(
    SERIALNO, woman_a