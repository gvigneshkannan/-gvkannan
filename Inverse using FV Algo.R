
library(psych)
library(matlib)
n<-readline(prompt = "Enter the rows: ")
c<-readline(prompt = "Enter the columns: ")

for(i in 1:n)
  for(j in 1:c)
    A[i,j] <- readline("Enter the data:")

if(no_row != no_col)
  print("Rows and Columns are Not equal")
else
  e <- eigen(A)
  b[1] <- A
  p[1] <- tr(b[1])
  I <- diag(n)
  for(i in 2:n)
      {b[i] <- A%*%(b[i-1]-p[i-1]*I)
       p[i] <- tr(b[i])
  }
  n <- no
  I_A <- (b[n-1] - p[n-1]*I)
  print(I_A)
