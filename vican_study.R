
# VICAN Study: Part B

vican <- read.csv2(
  "data/BDD_VICAN.csv",
  na.strings = c("", "NA", "#NULL!")
)

head(vican)
vican[c(1, 4, 18, 103), ]


names(vican)

dim(vican)

colSums(is.na(vican))


# recode the variables using an appropriate format, Convert categorical variables into factors
vican$FC_Caisse <- factor(vican$FC_Caisse)
vican$MS_CodCancer <- factor(vican$MS_CodCancer)
vican$Q5_sd4_r1 <- factor(vican$Q5_sd4_r1)
vican$Q5_sd5 <- factor(vican$Q5_sd5)
vican$Q5_sd10_r2 <- factor(vican$Q5_sd10_r2)
vican$Q5_anxiete <- factor(vican$Q5_anxiete)
vican$Q5_depression <- factor(vican$Q5_depression)
vican$Q5_jobv5.36_R1 <- factor(vican$Q5_jobv5.36_R1)
vican$MS_CSP_ENQ_3C_R1 <- factor(vican$MS_CSP_ENQ_3C_R1)
vican$Q5_med23.1 <- factor(vican$Q5_med23.1)
vican$Q5_Pain <- factor(vican$Q5_Pain)

# Convert fatigue scores to numeric
vican$Q5_eortc_fatigue_r1 <- as.numeric(
  gsub(",", ".", vican$Q5_eortc_fatigue_r1)
)

# Group medical sequelae into three categories
vican$Q5_med23.1_rec <- NA

vican$Q5_med23.1_rec[vican$Q5_med23.1 %in% c("1", "2")] <- "Important sequelae"
vican$Q5_med23.1_rec[vican$Q5_med23.1 %in% c("3", "4")] <- "Moderate sequelae"
vican$Q5_med23.1_rec[vican$Q5_med23.1 == "5"] <- "No sequelae"

vican$Q5_med23.1_rec <- factor(
  vican$Q5_med23.1_rec,
  levels = c("Important sequelae", "Moderate sequelae", "No sequelae")
)

# Display frequencies
table(vican$Q5_med23.1_rec, useNA = "ifany")

#. Identify clinically significant fatigue
vican$fatigue_category <- ifelse(
  is.na(vican$Q5_eortc_fatigue_r1),
  NA,
  ifelse(vican$Q5_eortc_fatigue_r1 >= 40,
         "Clinically significant fatigue",
         "No clinically significant fatigue")
)

vican$fatigue_category <- factor(
  vican$fatigue_category,
  levels = c("No clinically significant fatigue",
             "Clinically significant fatigue")
)

# Display frequencies
table(vican$fatigue_category, useNA = "ifany")


# Age statistics

# a. Overall study population
mean_all <- mean(vican$FC_AgeDiag_R0)
sd_all <- sd(vican$FC_AgeDiag_R0)
var_all <- var(vican$FC_AgeDiag_R0)
n_all <- length(vican$FC_AgeDiag_R0)

ci_all <- mean_all + c(-1, 1) * qt(0.975, df = n_all - 1) *
  sd_all / sqrt(n_all)

cat("Overall population\n")
cat("Mean age:", mean_all, "\n")
cat("95% CI:", ci_all, "\n")
cat("Sample variance:", var_all, "\n")
cat("Standard deviation:", sd_all, "\n\n")

# b. Patients with breast cancer (MS_CodCancer = 1)
breast <- vican$FC_AgeDiag_R0[vican$MS_CodCancer == "1"]

mean_breast <- mean(breast)
sd_breast <- sd(breast)
var_breast <- var(breast)
n_breast <- length(breast)

ci_breast <- mean_breast + c(-1, 1) * qt(0.975, df = n_breast - 1) *
  sd_breast / sqrt(n_breast)

cat("Breast cancer group\n")
cat("Mean age:", mean_breast, "\n")
cat("95% CI:", ci_breast, "\n")
cat("Sample variance:", var_breast, "\n")
cat("Standard deviation:", sd_breast, "\n")

# Plot age distribution by cancer location

pdf("C:/Users/Mihiretu J/Documents/AMU/Assignments/Assignment 1/Q10_Age_by_Cancer.pdf",
    width = 10, height = 7)

boxplot(FC_AgeDiag_R0 ~ MS_CodCancer,
        data = vican,
        xlab = "Cancer location",
        ylab = "Age at diagnosis (years)",
        main = "Age distribution by cancer location",
        las = 2,
        col = "lightblue")

dev.off()

#Comment on the age distribution
#The median age different among cancers groups. The groups 4, 6, and 82 have high median ages, while group 9 has the lowest median age. Age variation is not the same for different groups, and possible outliers can be seen in group 1.

# Describe physical quality of life

# Calculate summary statistics
mean(vican$Q5_pcs12_r1)
sd(vican$Q5_pcs12_r1)
median(vican$Q5_pcs12_r1)
IQR(vican$Q5_pcs12_r1)

# Plot the distribution
hist(vican$Q5_pcs12_r1,
     main = "Distribution of Physical Quality of Life",
     xlab = "Physical quality of life score",
     col = "lightblue")

# Univariable analysis

results <- data.frame(
  Variable = character(),
  Test = character(),
  P_value = numeric()
)

# Numerical explanatory variables: Spearman correlation
num_vars <- c("FC_AgeDiag_R0", "Q5_mcs12_r1",
              "Q5_eortc_fatigue_r1")

for (v in num_vars) {
  test <- cor.test(vican[[v]], vican$Q5_pcs12_r1,
                   method = "spearman", use = "complete.obs")
  results <- rbind(results, data.frame(
    Variable = v, Test = "Spearman correlation",
    P_value = test$p.value
  ))
}

# Categorical explanatory variables
cat_vars <- c("FC_Caisse", "MS_CodCancer", "Q5_sd4_r1",
              "Q5_sd5", "Q5_sd10_r2", "Q5_anxiete",
              "Q5_depression", "Q5_jobv5.36_R1",
              "MS_CSP_ENQ_3C_R1", "Q5_med23.1_rec", "Q5_Pain")

for (v in cat_vars) {
  formula <- as.formula(paste("Q5_pcs12_r1 ~", v))
  fit <- aov(formula, data = vican)
  p <- summary(fit)[[1]][["Pr(>F)"]][1]
  
  results <- rbind(results, data.frame(
    Variable = v, Test = "One-way ANOVA",
    P_value = p
  ))
}

results$P_value <- signif(results$P_value, 3)
results

# Multivariable 
#Q13a.Select variables with univariable p-value < 0.20
model_final <- lm(
  Q5_pcs12_r1 ~ MS_CodCancer +
    Q5_sd4_r1 +
    Q5_sd10_r2 +
    Q5_anxiete +
    Q5_depression +
    Q5_jobv5.36_R1 +
    MS_CSP_ENQ_3C_R1 +
    Q5_med23.1_rec +
    Q5_Pain +
    FC_AgeDiag_R0 +
    Q5_mcs12_r1 +
    Q5_eortc_fatigue_r1,
  data = vican
)

# Display model results
summary(model_final)

# Q13b. Graphical diagnostics for linear regression

# 1. Residuals vs Fitted
par(mfrow = c(1, 1))
plot(model_final, which = 1)

# 2. Normal Q-Q plot
plot(model_final, which = 2)

# 3. Scale-Location plot
plot(model_final, which = 3)

# 4. Residuals vs Leverage
plot(model_final, which = 5)
#Interpretation/comment
#The diagnostic plots show possible non-linearity, unequal variance, and deviations from normality. Two observations have high leverage. Therefore, the model results should be interpreted with caution.

# Q13c. Table of multivariable regression results

model_summary <- summary(model_final)$coefficients
ci <- confint(model_final)

final_results <- data.frame(
  Variable = rownames(model_summary),
  Coefficient = round(model_summary[, "Estimate"], 2),
  CI_lower = round(ci[, 1], 2),
  CI_upper = round(ci[, 2], 2),
  P_value = signif(model_summary[, "Pr(>|t|)"], 3),
  row.names = NULL
)

final_results

#Q13(d). Interpretation of the final model
#The model explained approximately 49.1% of the variation in physical quality of life. After adjustment, older age, depression, pain, and greater fatigue were associated with poorer physical quality of life. Compared with the reference groups, having moderate or no sequelae was associated with better physical quality of life.  These findings indicate associations, and interpreted cautiously because some regression assumptions were not fully satisfied.

# Export the final dataset
write.csv2(
  vican,
  "output/BDD_VICAN_Final_Dataset.csv",
  row.names = FALSE,
  na = ""
)