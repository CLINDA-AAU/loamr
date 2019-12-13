

sigma2B <- 0.1
sigma2A <- 0.1
sigma2E <- 0.5

sim <- 1000

for (i in 1:sim) {
  fit <- LOAM(simMD(subjects = 20, observers = 15))
  A[i] <- fit$parts$sigma2A
  B[i] <- fit$parts$sigma2B
  E[i] <- fit$parts$sigma2E
  cat(sim,"/",i, "\r")
}

extra <- tibble(B = sigma2B, A = sigma2A, E = sigma2E) %>% gather(variable, value)

tibble(A,B,E) %>% gather(variable, value) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  geom_vline(data=extra, aes(xintercept = value), color="red") +
  facet_wrap(~variable, scales = "free") + labs("1000 simulations of sigma^2")


































