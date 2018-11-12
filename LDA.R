# Tests for ML PSET2 - Linear Discriminant Analysis

library(mvtnorm)

# Create fake data
c0 <- mvtnorm::rmvnorm(20000, mean = c(2, 1),
                 sigma = diag(c(2, 1)))
c1 <- mvtnorm::rmvnorm(10000, mean = c(1, 1),
                 sigma = diag(c(2, 1)))

data = data.frame(rbind(c0, c1))
colnames(data) <- c("x1", "x2")
data$class <- c(rep(0, 20000), rep(1, 10000))
head(data)


# Plot my solution (decision boundary is the vertical line x0 = .1137)
library(ggplot2)

means <- data.frame(x1 = c(2, 1), x2=c(1, 1))

# Histogram
ggplot() +
  geom_histogram(data = data[data$class==0, ], aes(x=x1), color = "blue", alpha = .3, bins = 50) +
  geom_histogram(data = data[data$class==1, ], aes(x=x1), color = "red", alpha = .3, bins = 50) +
  geom_vline(xintercept = .1137)
# YES!

# Plot
ggplot(data = data, aes(x=x1, y=x2, color=as.factor(class))) +
  geom_point(alpha = .05) +
  geom_vline(xintercept = .1137) +
  geom_point(data = means, aes(x=x1, y=x2), inherit.aes = FALSE, size = 1)

# WTF --- MASS::lda performs some sort of standardization beforehand...
library(MASS)
fit_lda <- MASS::lda(class ~ x1 + x2, data=data)
fit_lda

