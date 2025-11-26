############################################################
# Prepare Data for PCA
############################################################
X <- df_model %>%  select(-country, -life_expectancy_at_birth_total_years) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(everything(), as.numeric))

############################################################
# PCA (Correlation Matrix)
############################################################
pca_fit <- princomp(X, cor = TRUE, scores = TRUE)

# Scree plot
eig_values <- pca_fit$sdev^2
plot(eig_values, type = "b", pch = 19,
     main = "Figure 1: Scree Plot of Principal Components",
     xlab = "Principal Component", ylab = "Eigenvalue")
abline(h = 1, col = "red", lty = 2)  # Kaiser rule reference

############################################################
# Permutation Test 
############################################################
set.seed(772713)
# Observed eigenvalues
eig_obs <- eigen(cor(X), symmetric = TRUE, only.values = TRUE)$values

# Permutation distribution (500 permutations)
eig_perm <- replicate(500, eigen(cor(as.data.frame(lapply(X, sample))),
                                 symmetric = TRUE, only.values = TRUE)$values)
# 95% permutation cutoff for each PC
eig_cutoff <- apply(eig_perm, 1, quantile, probs = 0.95)

# Plot
plot(eig_obs, type = "b", pch = 19, col = "black",
     xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Figure 2: Permutation Test – Observed vs. 95% Threshold")
lines(eig_cutoff, type = "b", pch = 4, col = "red", lty = 2)
legend("topright",legend = c("Observed Eigenvalues", "95% Permutation Threshold"),
       col = c("black", "red"), pch = c(19, 4), lty = c(1, 2), cex = 0.85)


############################################################
# Bootstrap Test for Eigenvalue Significance (Kaiser Rule)
############################################################
boot_kaiser <- function(X, B = 500) {
  eig_obs <- eigen(cor(X), only.values = TRUE)$values
  eig_boot <- replicate(B, eigen(cor(X[sample(nrow(X), replace = TRUE),]), only.values = TRUE)$values)
  ci95 <- apply(eig_boot, 1, quantile, c(0.025, 0.975))
  data.frame(
    PC = paste0("PC", 1:length(eig_obs)),
    eig_obs = eig_obs,
    ci_low = ci95[1,],
    ci_high = ci95[2,],
    retain = ci95[1,] > 1
  )
}

bk <- boot_kaiser(X, B = 500)

# Plot bootstrap CIs
figure3 <- ggplot(bk, aes(x = PC, y = eig_obs, color = retain)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "grey50")) +
  labs(title = "Figure 3: Bootstrap 95% CI for PCA Eigenvalues (Kaiser Test)",
       x = "Principal Component", y = "Eigenvalue", color = "Retain?") +
  theme_minimal()

############################################################
# Bootstrap Test for Variance Explained (VAF ≥ 70%)
############################################################
set.seed(772713)
vaf_boot <- replicate(500, {
  xb <- X[sample(nrow(X), replace = TRUE), ]
  sum(princomp(xb, cor = TRUE)$sdev[1:5]^2) / ncol(X)
})

vaf_df <- data.frame(VAF = vaf_boot)

vaf_result <- list(
  vaf_obs = mean(vaf_boot),
  ci_low = quantile(vaf_boot, 0.025),
  ci_high = quantile(vaf_boot, 0.975)
)

#Plot bootsratp Distribution Cumulative Variance Explained
figure4 <- ggplot(vaf_df, aes(x = VAF)) +
  geom_histogram(color = "white", fill = "steelblue", bins = 30) +
  geom_vline(xintercept = vaf_result$vaf_obs, color = "red", size = 1.2) +
  geom_vline(xintercept = 0.70, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Figure 4: Bootstrap Distribution of Cumulative Variance Explained (PC1–PC5)",
       x = "Cumulative Variance Explained",
       y = "Frequency") +
  theme_minimal(base_size = 11)


############################################################
# PCA Loadings (for Interpretation)
############################################################
L_clean <- as.data.frame(unclass(pca_fit$loadings))[,1:5] %>%
  tibble::rownames_to_column("Variable") %>%
  pivot_longer(starts_with("Comp"),
               names_to = "Component",
               values_to = "Loading") %>%
  mutate(Loading = round(Loading, 3)) %>%    # round loadings
  filter(abs(Loading) >= 0.30) %>%           # keep only meaningful loadings
  arrange(Component, desc(abs(Loading)))     # sort nicely

kable(L_clean,
      caption = "Principal Component Loadings",
      align = "lcr",
      booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 8) %>%
  column_spec(1, width = "12.75cm") %>%   # wrap variable names
  column_spec(2, width = "3cm") %>%   # wrap component names
  row_spec(0, bold = TRUE)


############################################################
# Principal Component Regression (PCR)
############################################################
PC_scores <- as.data.frame(pca_fit$scores[,1:5]) %>%
  mutate(life_expectancy = df_model$life_expectancy_at_birth_total_years)

set.seed(772713)
fold_id <- sample(rep(1:10, length.out = nrow(PC_scores)))
Xsc <- as.matrix(PC_scores[,1:5]); y <- PC_scores$life_expectancy

rmse_k <- sapply(1:5, function(k){
  pred <- rep(NA, length(y))
  for(f in 1:10){
    tr <- fold_id != f; te <- !tr
    fit <- lm(y[tr] ~ Xsc[tr,1:k,drop=FALSE])
    pred[te] <- cbind(1, Xsc[te,1:k,drop=FALSE]) %*% coef(fit)
  }
  sqrt(mean((y - pred)^2))
})

select_n <- which.min(rmse_k)
pcr_final_lm <- lm(y ~ Xsc[,1:select_n,drop=FALSE])
summary(pcr_final_lm)

# Compute metrics here
adj_r2 <- summary(pcr_final_lm)$adj.r.squared
rmse_pcr <- rmse_k[select_n]

# Get clean coefficient table
coef_table <- broom::tidy(pcr_final_lm)

coef_table$term <- gsub("Xsc\\[, 1:select_n, drop = FALSE\\]Comp\\.", "PC", coef_table$term)

coef_table <- coef_table %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t value` = statistic,
    `Pr(>|t|)` = p.value
  )

metrics <- tibble(
  Term = c("Adjusted R²", "Cross-Validated RMSE"),
  Estimate = c(adj_r2, rmse_pcr),
  `Std. Error` = NA,
  `t value` = NA,
  `Pr(>|t|)` = NA
)

pcr_summary_table <- bind_rows(coef_table, metrics)

knitr::kable(
  pcr_summary_table,
  caption = paste("Table 2: PCR Model Summary (", select_n, " Components)", sep=""),
  digits = 3,
  align = "lrrrr"
)


############################################################
# Elastic Net Regression
############################################################
X_en <- X %>% as.matrix()
y_en <- df_model$life_expectancy_at_birth_total_years

set.seed(772713)
cv_en <- cv.glmnet(X_en, y_en, alpha = 0.5, standardize = TRUE)

lambda_opt <- cv_en$lambda.min
en_final <- glmnet(X_en, y_en, alpha = 0.5, lambda = lambda_opt)

# Compute Elastic Net RMSE using cross-validation (FAIR comparison)
rmse_en_cv <- sqrt(min(cv_en$cvm))

#figure 5 & 6
plot(cv_en)
abline(v = log(lambda_opt), col = "red", lwd = 2)
mtext("Figure 5: Elastic Net Cross-Validation Curve",
      side = 3, line = 2, cex = 1.1, font = 2)

# Fit full EN path (for visualization)
en_path <- glmnet(X_en, y_en, alpha = 0.5)

# Plot shrinkage path
plot(en_path, xvar = "lambda", label = FALSE)
abline(v = log(lambda_opt), col = "red", lwd = 2, lty = 2)
mtext("Figure 6:Elastic Net Coefficient Shrinkage Path",
      side = 3, line = 2, cex = 1.1, font = 2)

############################################################
# Model Performance Comparison
############################################################
comparison_table <- data.frame(
  Model = c("Elastic Net (CV)", "PCR (CV)"),
  RMSE = c(round(rmse_en_cv, 3), round(rmse_pcr, 3))
)

kable(comparison_table, caption = "Model Performance Comparison")