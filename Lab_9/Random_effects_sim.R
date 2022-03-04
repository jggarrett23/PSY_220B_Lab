
# Fixed Model
grand_mean <- .8
a <- c(.5,-.25,-.25)
b <- c(0,1,-1)

y_hat <- c()

for (i in seq_along(a)){
  for (j in seq_along(b)){
    y_hat <-append(y_hat,grand_mean + a[i] + b[j])
  }
}


y_hat <- rep(y_hat,100)

a_levels <- rep(c(rep(1,3),rep(2,3),rep(3,3)),10)
b_levels <- rep(1:3,30)

# study 1
y.1 <- y_hat + rnorm(length(y_hat),6)

# replication
y.2 <- y_hat + rnorm(length(y_hat))

df <- data.frame(y.1=y.1, y.2=y.2, a=factor(a_levels), b=factor(b_levels))

model <- lm(y.1~a*b, data=df, contrasts = list(a = contr.sum, b=contr.sum))

betas <- coef(model)

a.coef_ests <- as.vector(c(betas[2],betas[3],-betas[2]-betas[3]))
b.coef_ests <- as.vector(c(betas[4],betas[5],-betas[4]-betas[5]))


# Random Model


