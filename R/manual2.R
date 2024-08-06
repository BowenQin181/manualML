library(tidyverse)

dt_demo <- tibble(
  x1 = runif(200, max = 1000),
  x2 = runif(200, max = 1000),
  Y = if_else(1000 + x1 - 2*x2 > 1000, "A", "B")
)

dt_demo %>%
  ggplot(aes(x1, x2, colour = Y)) +
  geom_point()

data = dt_demo
formula = Y ~ .
max_time = 100
W = 1




time = 0  # 记录运行了几次
model_frame <- model.frame(formula, data)

# 查看Y的分类
Y_class <- model.response(model_frame) %>%
  unique()
Y_class_to_number <- c(1, -1)
names(Y_class_to_number) <- Y_class


X_matrix <- model.matrix(formula, model_frame)
X_ncol <- ncol(X_matrix)

W <- rep(W, X_ncol)


# Y_pred <- if_else(sign(X_matrix %*% W) == 1,
#                   Y_class[[1]],
#                   Y_class[[2]])
#
# errors_dt <- model_frame[Y_pred != model_frame[, 1], ]
# n_errors <- nrow(errors_dt)

n_errors <- 11111

log_errors <- c()


# update_W <- function(W, errors_dt) {
#   errors_sample_id <- rownames(errors_dt) %>%
#     sample(size = 1)
#   x <- X_matrix[errors_sample_id, ]
#   y <- model_frame[errors_sample_id, ]$species %>%
#     Y_class_to_number[.]
#   W <- W + y*x
#   return(W)
# }

while (n_errors > 0 && time < max_time) {

  Y_pred <- if_else(sign(X_matrix %*% W) == 1,
                    Y_class[[1]],
                    Y_class[[2]])

  errors_dt <- model_frame[Y_pred != model_frame[, 1], ]
  n_errors <- nrow(errors_dt)
  log_errors <- c(log_errors, n_errors)
  time <- time + 1


  # update W
  errors_sample_id <- rownames(errors_dt) %>%
    sample(size = 1)
  x <- X_matrix[errors_sample_id, ]
  y <- model_frame[errors_sample_id, 1] %>%
    Y_class_to_number[.]
  W <- W + y*x



  # Y_pred <- if_else(sign(X_matrix %*% W) == 1,
  #                   Y_class[[1]],
  #                   Y_class[[2]])
  # errors_dt <- model_frame[Y_pred != model_frame[, 1], ]
  # n_errors <- nrow(errors_dt)
  # log_errors <- c(log_errors, n_errors)
  # time <- time + 1
}

return(list(
  "pred" = Y_pred,
  "W" = W,
  time = time,
  log_errors = log_errors
))

