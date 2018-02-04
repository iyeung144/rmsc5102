# In each draw, 7 numbers will be drawn out of 49 numbers. 
# The first 6 numbers are the Drawn Numbers 
# and the 7th number is the Extra Number. 
# HW2 q2.r

# Step 1: initialize vector to hold seven result numbers 
# and 49 initial numbers to be drawn.
m6 <- rep(0,7)
balls <- seq(1,49,1)

# Step 2: run the drawing seven times
for(i in 1:length(m6)) {
  
  # Step 3: generate a uniform random number
  x <- runif(1,0,1) 
  
  # Step 4: scale the random number to match with the 
  # length of pool numbers take the ceiling of
  # the random number to find the number drawn
  k <- ceiling(length(balls)*x)
  
  # Step 5: record the number drawn
  m6[i] <- balls[k]
  
  # Step 6: remove the number from the pool and return to Step 2
  balls <- balls[-k]
}

# print out the result as shown in TV
cat("Numbers drew in sequence: ",paste(m6, collapse =", "), "\n")
result <- sort(m6[1:length(m6)-1])
cat("Mark Six drawn: ",paste(result, collapse =", ")," + ", 
    m6[length(m6)], "\n")
cat("Balls not drawn: ",paste(balls, collapse =", "))



