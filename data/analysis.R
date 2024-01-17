load("data/NHANES15lab5.RData")

# Full data
dat.full <- analytic.with.miss

# Exposure
dat.full$bmi <- with(dat.full, ifelse(bmi>25, "Overweight", 
                                      ifelse(bmi<=25, "Not overweight", NA)))
dat.full$bmi <- as.factor(dat.full$bmi)
dat.full$bmi <- relevel(dat.full$bmi, ref = "Overweight")

# Drop unnecessary variables 
dat.full$born <- NULL
dat.full$physical.work <- NULL

# Rename the weight variable into interview.weight
names(dat.full)[names(dat.full) == "weight"] <- "interview.weight"


# Complete case data 
analytic.data <- dat.full[complete.cases(dat.full),]
dim(analytic.data)
#> [1] 6316   15

fileConn <- file("E:/GitHub/EpiShinyLiveS/data/analyticData2.txt")
dput(analytic.data, fileConn)
close(fileConn)


library(ggplot2)
library(survey)
library(tableone)
library(tibble)
##Approach by Zanutto (2006)
##Step 1
# Specify the PS model to estimate propensity scores
ps.formula <- as.formula(I(bmi=="Not overweight") ~ gender + age + race + income + education + 
                           married + cholesterol + diastolicBP + systolicBP)
ps.fit <- glm(ps.formula, data = analytic.data, family = binomial("logit"))
analytic.data$ps <- predict(ps.fit, type = "response", newdata = analytic.data)
ggplot(analytic.data, aes(x = ps, fill = bmi)) + 
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Propensity Scores", 
       x = "Propensity Score", 
       y = "Density",
       fill = "BMI Category") +
  theme_minimal()


## Step 2
analytic.data$usweight <- with(analytic.data, ifelse(I(bmi=="Not overweight"), 
                                                     1/ps, 1/(1-ps)))
summary(analytic.data$usweight)
analytic.data$usweight <- as.numeric(analytic.data$usweight)
# usweight_1st <- quantile(analytic.data$usweight, probs = 0.001)
# usweight_99th <- quantile(analytic.data$usweight, probs = 1-0.001)
# analytic.data$usweight[analytic.data$usweight < usweight_1st] <- usweight_1st
# analytic.data$usweight[analytic.data$usweight > usweight_99th] <- usweight_99th

ggplot(analytic.data, aes(x = usweight, fill = bmi)) + 
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Weights Based on Propensity Score by BMI Category", 
       x = "Propensity Score", 
       y = "Density",
       fill = "BMI Category") +
  theme_minimal()

## Step 3

# Covariates
vars <- c("gender", "age", "race", "income", "education", "married", "cholesterol", 
          "diastolicBP", "systolicBP")
design.unstab <- svydesign(ids = ~ID, weights = ~usweight, data = analytic.data)
# Balance checking with truncated unstabilized weight
tab.unstab <- svyCreateTableOne(vars = vars, strata = "bmi", data = design.unstab, test = F)
print(tab.unstab, smd = T)

tab.unstab0 <- CreateTableOne(vars = vars, strata = "bmi", data = analytic.data, test = F)
print(tab.unstab0, smd = T)
compare <- as.data.frame(cbind(ExtractSmd(tab.unstab), ExtractSmd(tab.unstab0)))
names(compare) <- c("Unweighted","Weighted")
compare$Weighted <- as.numeric(as.character(compare$Weighted))
compare 
compare <- rownames_to_column(compare, var = "Variable")
compare <- compare[order(compare$Weighted),]
compare$Variable <- factor(compare$Variable, levels = compare$Variable)
long_smd_data <- compare %>%
  pivot_longer(cols = -Variable, names_to = "Weighting", values_to = "SMD")
long_smd_data$Variable <- factor(long_smd_data$Variable, levels = compare$Variable)
ggplot(long_smd_data, aes(x = SMD, y = Variable, color = Weighting, group = Weighting)) + 
  geom_path(aes(linetype = Weighting), linewidth = 1) +  # Use geom_path() and map linetype to Weighting
  geom_point(size = 3) +
  scale_color_manual(values = c("Unweighted" = "red", "Weighted" = "blue")) +
  scale_linetype_manual(values = c("Unweighted" = "solid", "Weighted" = "solid")) +
  theme_minimal() +
  labs(title = "Comparison of Standardized Mean Differences (SMD) Sorted by Weighted SMD",
       x = "SMD",
       y = "Variable",
       color = "Weighting",
       linetype = "Weighting") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))


## step 4

analytic.data$new.usweight <- with(analytic.data, interview.weight * usweight)
summary(analytic.data$new.usweight)


w.design <- svydesign(id = ~psu, strata = ~strata, weights = ~new.usweight, 
                       data = analytic.data, nest = TRUE)
# Outcome model
out.formula <- as.formula(I(diabetes == "Yes") ~ bmi)
fit <- svyglm(out.formula, design = w.design, family = binomial("logit"))
jtools::summ(fit, exp = TRUE, confint = TRUE, digits = 3)
