library(tidyverse)
penguins <- palmerpenguins::penguins

# params --------
data <- penguins %>%
  select(-island, -sex, -year) %>%
  filter(species != "Adelie") %>%
  mutate(species = factor(species))

formula <- species ~ .

max_time = 1000

W_init = 1




# function body -----

time = 1

model_frame <- model.frame(formula, data)

Y_class <- model.response(model_frame) %>%
  unique()


Y_class_to_number <- c(1, -1)
names(Y_class_to_number) <- Y_class




X_matrix <- model.matrix(formula, model_frame)
X_ncol <- ncol(X_matrix)


get_W <- function(W_init, X_ncol) {
  if(W_init == 1) {
    W = rep(1, X_ncol)
  } else {
    W = W_init
  }
}

W = get_W(W_init, X_ncol)

Y_pred <- if_else(sign(X_matrix %*% W) == 1,
                  Y_class[[1]],
                  Y_class[[2]])


errors_dt <- model_frame[Y_pred != model_frame$species, ]

n_errors <- nrow(errors_dt)

log_errors <- n_errors



update_W <- function(W, errors_dt) {
  errors_sample_id <- rownames(errors_dt) %>%
    sample(size = 1)
  x <- X_matrix[errors_sample_id, ]
  y <- model_frame[errors_sample_id, ]$species %>%
    Y_class_to_number[.]
  W <- W + y*x
  return(W)
}

while (n_errors > 0 || time < max_time) {
  W = update_W(W, errors_dt)
  Y_pred <- if_else(sign(X_matrix %*% W) == 1,
                    Y_class[[1]],
                    Y_class[[2]])
  errors_dt <- model_frame[Y_pred != model_frame$species, ]
  n_errors <- nrow(errors_dt)
  log_errors <- c(log_errors, n_errors)
  time <- time + 1
}

return(list(
  "pred" = Y_pred,
  "W" = W,
  time = time,
  log_errors = log_errors
))
