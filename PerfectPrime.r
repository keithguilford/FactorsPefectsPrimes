is.prime <- function(x) {
  if (x <= 1) {
    return(FALSE) # Numbers <= 1 are not prime
  }
  if (x == 2) {
    return(TRUE) # 2 is a prime number
  }
  if (x %% 2 == 0) {
    return(FALSE) # Even numbers greater than 2 are not prime
  }
  # Check divisors from 3 to the square root of x
  for (i in seq(3, floor(sqrt(x)), by = 2)) {
    if (x %% i == 0) {
      return(FALSE) # Found a divisor, not prime

    }
  }
  return(TRUE) # No divisors found, x is prime
}


all.factors = function(x){
  output = c()
  
  for(i in 1:x){
    if (x%%i == 0){
      output = c(output,i)
    }
  }
  return(output)
}


is.perfect = function(x){
  
  if(sum(all.factors(x))-x==x){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}
