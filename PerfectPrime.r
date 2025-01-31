
is.prime <- function(x) {
  if (x <= 1) {
    return('Less than 1') # Numbers <= 1 are not prime
  }
  if (x == 2) {
    return('Equal to 2') # 2 is a prime number
  }
  if (x %% 2 == 0) {
    return('Even') # Even numbers greater than 2 are not prime
  }
  # Check divisors from 3 to the square root of x
  for (i in seq(3, x-1, by = 2)) {
    if (x %% i == 0) {
      return(paste('False', x, 'is diviable by ',i)) # Found a divisor, not prime
      print(i)
    }
  }
  return('Is Prime') # No divisors found, x is prime
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
