# ---- OLS and Logistic models -----------------------------

#ensure results/ exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

#fit models
ols <- lm(
  InferredBirth ~ twoChildrenBothBound + ST + SurveyYear + ageCombo +
    Race + Age + Education + IncomeQuintile + MalePresent,
  data = panel
)

logit <- glm(
  InferredBirth ~ twoChildrenBothBound + ST + SurveyYear + ageCombo +
    Race + Age + Education + IncomeQuintile + MalePresent,
  data = panel,
  family = binomial()
)

library(modelsummary); library(gt)

#create a polished table of summary
msum <- modelsummary::modelsummary(
  list("OLS (Linear Probability)" = ols,
       "Logit"                    = logit),
  statistic  = c("({std.error})","p = {p.value}"),
  gof_omit   = "IC|Log|Adj|Within|Pseudo|AIC|BIC",
  stars      = TRUE,
  output     = "gt"
) |>
  gt::tab_header(title = "Birth Outcome Models",
                 subtitle = "Comparing OLS (LPM) vs Logistic Regression") |>
  gt::tab_source_note(md("Note: Outcome is **InferredBirth** (0/1). Stars indicate significance."))

#save HTML
gt::gtsave(msum, "results/model_summary.html")





