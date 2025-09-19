# ---- download ACS PUMS (CT + FL) -----------------------------------------

get_pums_year <- function(year) {
  #relationship var + income adjustment var changed names historically
  rel_var <- ifelse(year < 2010, "REL",  "RELP")
  adj_var <- ifelse(year < 2008, "ADJUST","ADJINC")
  
  vars <- c(
    "AGEP","SEX","RAC1P","HISP","SCHL","HINCP","VEH",
    rel_var,"SERIALNO","PUMA",adj_var,"ST","PWGTP","WGTP"
  )
  
  df <- tidycensus::get_pums(
    variables = vars,
    state     = STATES,
    survey    = "acs1",
    year      = year,
    recode    = TRUE
  )
  
  #normalize names so downstream code always uses RELP / ADJINC
  if (year < 2010) df <- dplyr::rename(df, RELP  = REL)
  if (year < 2008) df <- dplyr::rename(df, ADJINC = ADJUST)
  
  df$YEAR <- year
  
  df %>%
    mutate(
      across(c(AGEP, HINCP, VEH, RELP, SCHL, SEX, HISP, RAC1P, ADJINC), as.numeric)
    )
}

raw_all <- dplyr::bind_rows(lapply(YEARS, get_pums_year))