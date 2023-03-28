model{

  # Likelihood
  s ~ dbin(p, n)
  logit(p) <- theta

  # Priors
  theta ~ dnorm(mu, pow(sigma,-2)) # Remember parameterized in terms of tau (precision)

}
