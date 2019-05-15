

w = 1

E1 = seq(-10, 10, by=.001)

E2 = seq(10, -10, by=-.001)

x_vals = E1 - E2

e_plus_function <- function(w, E1, E2) {
  e_plus <- (1/2)*(E1 + E2) + (1/2)*sqrt((E1 - E2)**2 + 4*(abs(w))**2)
  return(e_plus)
}

e_minus_function <- function(w, E1, E2) {
  e_minus <- (1/2)*(E1 + E2) - (1/2)*sqrt((E1 - E2)**2 + 4*(abs(w))**2)
  return(e_minus)
}

plot(x_vals, e_plus_function(w, E1, E2), type="l", #ylim=c(0,10)
     ylim=c(e_minus_function(w, -10, 10),e_plus_function(w, -10, 10)), col="blue",
     main="Avoided Crossing", xlab="E1 - E2", ylab="Energy Level")
lines(x_vals, e_minus_function(w, E1, E2), col="red")