# ---- build woman–year panel, features, flags -----------------------------

panel <- wide_sample %>%
  #rows to expand per woman (ages 18..current age)
  mutate(panel_len = woman_age - 18 + 1) %>%
  uncount(weights = panel_len, .remove = TRUE, .id = "yr_idx") %>%
  mutate(
    woman_age_panel = 18 + yr_idx - 1,
    panel_year      = YEAR - (woman_age - woman_age_panel)
  ) %>%
  select(-yr_idx) %>%
  #infer birth in panel_year by checking if (YEAR - child_age) equals panel_year
  mutate(
    InferredBirth = pmap_int(list(children_ages, YEAR, panel_year), function(ages, yr, py) {
      if (length(ages) == 0) return(0L)
      births <- yr - ages
      as.integer(py %in% births)
    }),
    #ages of prior kids in this panel year (exclude newborns)
    prior_ages = pmap(list(children_ages, YEAR, panel_year), function(ages, yr, py) {
      ages_now <- ages - (yr - py)
      ages_now[ages_now > 0]
    }),
    #coarse age combo of prior kids
    ageCombo = map_chr(prior_ages, function(prior) {
      if (!length(prior)) return("none")
      if (length(prior) >= 4) return("4+")
      bins <- cut(prior, breaks = c(-Inf,2,5,Inf), labels = c("0-2","3-5","6+"), right = TRUE)
      paste(sort(as.character(bins)), collapse = ",")
    }),
    #two-children-both-bound flag using seat-law thresholds
    twoChildrenBothBound = pmap_int(list(prior_ages, panel_year, ST), function(prior, py, st) {
      thr <- seat_threshold(st, py)
      as.integer(length(prior) == 2 && all(prior <= thr))
    }),
    #tidy names for modeling
    Age            = woman_age_panel,
    SurveyYear     = YEAR,
    IncomeQuintile = factor(income_quintile),
    MalePresent    = factor(male_present)
  ) %>%
  mutate(ageCombo = forcats::fct_relevel(ageCombo, "none")) %>%
  select(-children_ages, -income_quintile, -male_present, -prior_ages,