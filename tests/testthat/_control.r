library(mrgsolve)
mod <- mread(".cpp")
# theta_names <- c(THETA1 = "KA", THETA2 = "VM", THETA3 = "KM", THETA4 = "V")
# parm <- c(THETA1 = 1, THETA2 = 20, THETA3 = 2, THETA4 = 50)
# mod  <- param(mod,parm)
evnt <- ev(amt = 100, ii = 24, addl = 1)
# To simulate single subject use zero_re(mod) in combination with nid=1
out <- mod %>% ev(evnt) %>% mrgsim(end = 48, delta = 0.1, nid = 10)
plot(out)
