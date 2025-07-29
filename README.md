# Car Seats as Contraception: Replication Study

**Reproducing and evaluating the “Car Seats as Contraception” OLS‑based analysis on U.S. birth rates**

---

## Background  
The original study claims that stricter car‑seat laws—by raising the visible costs of child‑rearing—reduce the probability of subsequent births. Unusually, the authors modeled a binary birth outcome (`inferred_birth`) using OLS rather than logistic regression. This repo reproduces their approach, adds a proper logistic model, and compares results.

---

## Tools & Methods  
- **Data source:** ACS PUMS (2006–2017) for CT & FL via `tidycensus`  
- **Key packages:**  
  - Data wrangling: `dplyr`, `purrr`, `tidycensus`  
  - Regression: base `lm()`, `glm(family=binomial)`, `fixest::feols` & `fixest::feglm`  
  - Tidy output: `broom`  
- **Workflow:**  
  1. Download & merge PUMS data  
  2. Clean and recode demographics, adjust income for inflation  
  3. Filter to households with an 18–35 yo woman and biological children only  
  4. Expand into woman–year panel, compute `inferred_birth` and `twoChildrenBothBound` flags  
  5. Fit weighted OLS and survey‑weighted logistic models  
  6. Run fixed‑effects OLS and logistic with `fixest`

---

## Repository Structure

- **slides/** &ndash; Keynote slides (exported PDF)  
- **data/** &ndash; Scripts for downloading & cleaning raw ACS PUMS  
- **analysis.R** &ndash; Main R script: data wrangling, panel creation, modeling  
- **results/** &ndash; Exported model summaries & key figures  
- **README.md** &ndash; (this file) 

---

## Key Findings  
- **OLS (binary outcome)** and **logistic** both show the `twoChildrenBothBound` variable is _not_ statistically significant (p > 0.05).  
- Directions of effect are similar, but OLS underestimates uncertainty on a binary response.  
- Proper logistic modeling is preferable for binary outcomes—OLS can bias standard errors and inference.

---
