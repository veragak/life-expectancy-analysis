############################################################
# Predict Life Expectancy for New Countries
############################################################
df_pred_clean <- df_pred %>%
  select(all_of(colnames(X_en))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), median(., na.rm = TRUE), .))) %>%
  as.matrix()

prediction_table <- data.frame(
  Country = df_pred$country,
  Predicted_Life_Expectancy = round(as.numeric(predict(en_final, newx = df_pred_clean)), 2)
)

kable (prediction_table, caption = "Predicted Life Expectancy for New Countries")
