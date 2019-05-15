#Spring Constants
#k1 and k2 refer to k
k1 = 6.1
k2 = 6.1

#K12 refers to K
K12 = 2.14

# k refers to k
k = k1

# j refers to K
j = K12

#Masses
# n refers to mass 2 (NEEDS to be in kg)
n = .4505

# m refers to mass 1 (NEEDS to be in kg)
# this is the mass that is changing
m = seq(0, 1.5, .01)

#Difference in mass
x = m

frequency_high_function <- function(j, m, n, k) {
  ang_frequency_high <- sqrt((sqrt((-j*m - j*n - k*m - k*n)**2 - (4*m*n)*(2*j*k + k**2))
                  / (m*n)) + ((j/m) + (j/n) + (k/m) + (k/n)))/(sqrt(2))
  return(ang_frequency_high/(2*pi))
}

frequency_low_function <- function(j, m, n, k) {
  ang_freqeuncy_low <- sqrt((-sqrt((-j*m - j*n - k*m - k*n)**2 - (4*m*n)*(2*j*k + k**2))
                         / (m*n)) + ((j/m) + (j/n) + (k/m) + (k/n)))/(sqrt(2))
  return(ang_freqeuncy_low/(2*pi))
}

cat("The low frequency is", frequency_low_function(j, m, n, k), "\n")
cat("The high frequency is", frequency_high_function(j, m, n, k))

experimental_x = seq(.250,.650,.050)

blue_cart_hi = c(5, 5, .850, .830, .791, .786, .757, .747, .747)
blue_cart_lo = c(.654, .635, .635, .615, .596, .562, .542, .532, .522)

red_cart_hi = c(.967, .889, .850, .811, .771, .761, .732, .728, .728)
red_cart_lo = c(.635, .635, .635, .615, .596, .595, .576, .562, .542)


plot(x, frequency_high_function(j, m, n, k), type="l", ylim = c(0,1.5),
     col="blue", main="Coupled Oscillator (Mass 2 fixed at 450.5g)", xlab="Mass 1", ylab="Freqeuncy Values")
lines(x, frequency_low_function(j, m, n, k), col="red")
legend("topright", legend=c("Frequency Low", "Frequency High"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

points(experimental_x, blue_cart_hi, col = 'blue')
points(experimental_x, blue_cart_lo, col = 'blue')

points(experimental_x, red_cart_hi, col = 'red')
points(experimental_x, red_cart_lo, col = 'red')