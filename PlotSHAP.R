library(tidyverse)
library(xgboost)
library(shapviz)
library(patchwork)
library(showtext)

font_add("Cons", "constan.ttf")
showtext_auto()

set.seed(1027)

calculate_r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

roles <- c("male", "female", "local", "non-local", "experience", "non-experience")
rolelabs <- c("Male", "Female", "Local", "Non-local", "Issue Experience(s)", "No Issue Experiences")
df <- readxl::read_excel("explain.xlsx", sheet = 1) %>%
  mutate(role = factor(role, levels = roles, labels = rolelabs)) %>%
  rename(`Role Strategies` = role)

train_indices <- sample(seq_len(nrow(df)), size = nrow(df) * 0.7)
X <- df %>% select(-item, -round, -score)
X_matrix <- data.matrix(X)
X_scaled <- scale(X_matrix)
Y <- df$score

X_train <- X[train_indices, ]
X_train_matrix <- X_matrix[train_indices, ]
X_train_scaled <- X_scaled[train_indices, ]
Y_train <- Y[train_indices]

X_test <- X[-train_indices, ]
X_test_matrix <- X_matrix[-train_indices, ]
X_test_scaled <- X_scaled[-train_indices, ]
Y_test <- Y[-train_indices]

fit.unscaled.unlogged <- xgb.train(
  params = list(learning_rate = 0.1), 
  data = xgb.DMatrix(X_train_matrix, label = Y_train),
  nrounds = 65
)
calculate_r2(Y_train, predict(fit.unscaled.unlogged, X_train_matrix))
calculate_r2(Y_test, predict(fit.unscaled.unlogged, X_test_matrix))

#fit.scaled.unlogged <- xgb.train(
#  params = list(learning_rate = 0.1), 
#  data = xgb.DMatrix(X_train_scaled, label = Y_train),
#  nrounds = 65
#)
#calculate_r2(Y_train, predict(fit.scaled.unlogged, X_train_scaled))
#calculate_r2(Y_test, predict(fit.scaled.unlogged, X_test_scaled))

fit.unscaled.logged <- xgb.train(
  params = list(learning_rate = 0.1), 
  data = xgb.DMatrix(X_train_matrix, label = log(Y_train)),
  nrounds = 65
)
calculate_r2(log(Y_train), predict(fit.unscaled.logged, X_train_matrix))
calculate_r2(log(Y_test), predict(fit.unscaled.logged, X_test_matrix))

#fit.scaled.logged <- xgb.train(
#  params = list(learning_rate = 0.1), 
#  data = xgb.DMatrix(X_train_scaled, label = log(Y_train)),
#  nrounds = 65
#)
#calculate_r2(log(Y_train), predict(fit.scaled.logged, X_train_scaled))
#calculate_r2(log(Y_test), predict(fit.scaled.logged, X_test_scaled))

fit <- fit.unscaled.unlogged

shp <- shapviz(fit, X_pred = X_matrix, X = X_matrix)

p_left <- sv_importance(shp, show_numbers = TRUE, alpha = 0.7) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Cons",
                            face = "bold",
                            size = 24))
p_left@layers$geom_text$aes_params$family <- "Cons"
p_left@layers$geom_text$aes_params$size <- 6

p_right <- sv_importance(shp, kind = "bee", size = 0.001, alpha= 0.5) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Cons",
                            face = "bold",
                            size = 24))

p_SHAP <- p_left + p_right + plot_layout(ncol = 2, widths = c(1, 2))
ggsave("./Figures/SHAP.png",
       p_SHAP,
       width = 17.21,
       height = 10,
       units = "cm")

p_roleSHAP <- sv_dependence(shp, v = "Role Strategies", color_var = NULL, 
              color = "#fca50a", size = 0.001, alpha= 0.5) +
  scale_x_continuous(breaks = 1:6, labels = rolelabs) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3,0, 3), labels = c(-3,0, 3)) +
  labs(x = NULL) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Cons",
                            face = "bold",
                            size = 24))
ggsave("./Figures/RoleSHAP.png",
       p_roleSHAP,
       width = 17.21,
       height = 5,
       units = "cm")

#save.image("model.fit.RData")
