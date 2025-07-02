source("header.R")

# Subsetting -------------------------------------------------------------------
# Define a vector, `x`, as integers ranging from 1 to 10.
x <- 1:10
print(x)

## Subsetting using integers ---------------------------------------------------
# Say we want to pull out the 6th element in the vector.
# Use square brackets with `6` inside to pull out the 6th element
x[6]

# Exercise 1. Extract the 9th element of `x`


# Exercise 2. Extract the 1st and 5th elements of `x` (simultaneously).


## Subsetting using reals ------------------------------------------------------
# Exercise 3. What happens when you use a real number to subset `x`? Try 6.1.


## Subsetting using logicals ---------------------------------------------------
# Let's convert `x` into a logical (TRUE/FALSE) vector based on 
# whether each integer is even.
y <- x %% 2 == 0 # The `%%` is the "remainder" operator.
print(y)

# Exercise 4. Can you extract the even elements of the `x` vector, using `y`?


# Exercise 5. Now extract the odd elements of the `x` vector, using `y`.


## Subsetting using factors ----------------------------------------------------
# Let's convert the `y` vector into a factor, where each even number is labelled
# "even", and each odd number is labelled "odd"
z <- vector(length = 10L) # first need to initialize the vector
z[y] <- "even" # set the even elements to be "even"
z[!y] <- "odd" # set the odd elements to be "odd"
class(z) # z is currently a character vector
z <- factor(z) # convert z to a factor

# Exercise 6. What happens when you coerce `z` to class integer? 
# Hint: use `as.integer()`


# Exercise 7. What do you notice about the order of the results from the previous
# question, with respect to the levels of `z`?
levels(z)


# Exercise 8. Subset the following vector with `z`. What happens?
effect <- c("even number", "odd number")


## Assigning values using subsetting -------------------------------------------
# You can also assign values to a specific value of a vector using the same 
# square brackets
# e.g., if we wanted to replace the last value of `x` with 11:
x[10] <- 11
print(x)

# Exercise 9. Change the 2nd value of `x` to 100.


# Exercise 10. Change the first 4 elements of `x` to 0.


## Subsetting matrices ---------------------------------------------------------
# Matrices have two dimensions.
mat <- matrix(1:20, nrow = 4, ncol = 5)
print(mat)

# You can subset specific values by [row number, column number]:
# e.g., if we wanted the value in the 3rd row, 4th column:
mat[3, 4]

# You can subset an individual row by leaving the column blank:
# e.g., if we wanted the first row
mat[1, ]

# Exercise 11. Applying similar logic, how would you subset the 2nd column?


# Exercise 12. Pull out the value in the first row, third column.


# Exercise 13. Pull out the values in the first row, fourth column and fourth 
# row, 5th column simultaneously.


# For loops --------------------------------------------------------------------
# Let's start with a vector including the first 10 letters of the alphabet
let <- letters[1:10]

# The basic structure of a for loop is as follows:
# for (i in list) {
#   do_something(i)
# }

# Let's say we want to print each element of `let`.
# There are two common ways of iterating through for loops in R.

# 1. 
for (i in let) {
  # Each iteration, `i` takes the next value of `let`.
  # Printing `i` will print the letters themselves.
  print(i) 
}

# 2. 
for (i in 1:length(let)) {
  # Each iteration, `i` takes the next value in the sequence of `1:length(let)`
  # `let` has length 10, so this is 1:10.
  # Printing `i` will print the current integer value
  print(i)
  # Printing `let[i]` will print the i^th of `let`, which gives us the same
  # behaviour as above.
  print(let[i])
}

# The second will be the typical use in JAGS.

# Let's try another vector
months <- month.abb
print(months)

# Exercise 9. Using a for loop, can you print the months in order?
# (use the syntax from the second loop above)


# Exercise 10. Alter your loop such that the months are printed in reverse order


## Practice using for loops and subsetting with a dataframe --------------------
# Set a seed for reproducibility
set.seed(19)

# Create tibble of data
data <- tibble(
  # Get a random order of the integers 1:10
  x = sample(1:10, 10, replace = FALSE),
  # Have a column y that is filled with NAs
  y = rep(NA, 10)
)

# Exercise 11. Using a for loop, replace the current values of `y` with 
# the value of `x - 1`


# Exercise 12. Now replace the current values of `y` with the month abbreviation
# the value of x corresponds to.


