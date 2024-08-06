
mypla <- function(data, formula, max_time, W = 1) {
  time = 1  # 记录运行了几次
  model_frame <- model.frame(formula, data)

  # 查看Y的分类
  Y_class <- model.response(model_frame) %>%
    unique()
  Y_class_to_number <- c(1, -1)
  names(Y_class_to_number) <- Y_class


  X_matrix <- model.matrix(formula, model_frame)
  X_ncol <- ncol(X_matrix)

  W <- rep(W, X_ncol)


  Y_pred <- if_else(sign(X_matrix %*% W) == 1,
                    Y_class[[1]],
                    Y_class[[2]])

  errors_dt <- model_frame[Y_pred != model_frame[, 1], ]
  n_errors <- nrow(errors_dt)

  log_errors <- n_errors


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
    # update W
    errors_sample_id <- rownames(errors_dt) %>%
      sample(size = 1)
    x <- X_matrix[errors_sample_id, ]
    y <- model_frame[errors_sample_id, 1] %>%
      Y_class_to_number[.]
    W <- W + y*x


    Y_pred <- if_else(sign(X_matrix %*% W) == 1,
                      Y_class[[1]],
                      Y_class[[2]])
    errors_dt <- model_frame[Y_pred != model_frame[, 1], ]
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

}


lll <- mypla(dt_demo, Y ~ ., max_time = 1000)
lll$log_errors
