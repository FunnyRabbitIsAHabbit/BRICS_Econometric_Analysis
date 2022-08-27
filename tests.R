# library(urca)
# 
x <- matrix(nrow = 100, ncol = 4)
x[, 1] <- rnorm(100)
x[, 2] <- rnorm(100)
x[, 3] <- rnorm(100)
x[, 4] <- rnorm(100)
x <- as.data.frame(x)
# 
# ca.jo(x,
#       type = "eigen",
#       ecdet = "const",
#       K = 2,
#       spec = "longrun") -> y
# z <- summary(y)
# z@cval[3, 2] <- 57
# bool_test <- as.data.frame(z@cval > z@teststat)
# bool_test
# count_rank <- 0
# for (row in NROW(bool_test):1) {
#     a <- bool_test[row, ]
#     condition <- which(TRUE == a)
#     if (length(condition)) {
#         print(count_rank)
#         count_rank <- 0
#         break
#     }
#     count_rank <- count_rank + 1
# }

m <- matrix(nrow = 100, ncol = 4)
m[, 1] <- rnorm(100)
m[, 2] <- rnorm(100)
m[, 3] <- x[, 3]
x[, 4] -> m[, 4]
m <- as.data.frame(m)
merge(x, m, by = c("V3", "V4"))
