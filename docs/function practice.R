#practice

#adds up the number of birds and dogs

#defined function
bird = 3
dog = 56
birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

#they are the same
birddog_sum(dog = 5,bird = 2)
birddog_sum(2,5)

#create a function to double values

double_val <- function(a){
  print(2 * a)
}

double_val(5)


#write a function with conditionals
# example is coverting animals ages

age_calc <- function(animal, age) {

if (animal == "dog") {
  print(age * 7)
  } else if (animal == "goat") {
  print(age * 4.7)
  } else print("Fake animal")
}

# try using for 8 year old dog

age_calc(animal = "dog", age = 8)


# try with cow

age_calc(animal = "cow", age = 8)


#write an updated version of the animal age function with error messages

animal_age_stop <- function(animal, age) {
  
  if (!animal %in% c("goat", "dog")) {
    stop("That animal is not real, nor not needed in function")
  }
    if (is.numeric(age) == FALSE) {
      stop("Age must be a number")
    }
}

  if (age <= 0 | age > 50) {
    warning("Are tou sure about that?")
  }

animal_age_stop("dog", 2)







#functions meet for loops

#all the dataframes in the function are called df ___> argument df
df_means <- function(df) {
  for (i in 1:ncol(df)) {
    if(is.numeric(df[[i]])) {
      column_name <- colnames(df[i])
      col_mean <- mean(df[[i]], na.rm = TRUE)
      print(paste("The mean value of", column_name, "is", round(col_mean,2)))
      #double bracket for column values, single is the row, since all columns we want, we iterate using i
    }
  }
}

df_means(df = palmerpenguins::penguins)



#creating a function


#An example would be from 0 to 100 m
#growth rate would be n

#population @ time

#growth rates would be the columns and the time index would be the rows

#two dimensional matrix @ a constant

#time 1 column and pop size would be a second and those would be the observations


#Logistic Growth Example


#first define the func name and the variables
#Logisatic Growth Equation
Logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K - N0)/N0) * exp(-r * time))
  print(Nt)
}

#$check the code
Logistic_growth(N0 = 100, K = 6000, r = .27, time = 40)

#work on an example dealing with only time

time_vec <- seq(from = 0, to = 35, by = .01)

#apply the logistic growth function to the vector
pop_35 <- Logistic_growth(N0 = 100, K = 6000, r = .27, time = time_vec)


#turn to df
#combining time steps and population size into a df
pop_35 <- data.frame(time_vec, pop_35)
pop_35

library(tidyverse)
#plot it
ggplot(data = pop_35, aes(x = time_vec, y = pop_35)) + geom_line(size = .5)

#alternatively with an internal for loop

# need to use the previous code that we made into this

#pre-allocate storage for the vector

pop_35_vec <-vector(mode = "numeric", length = length(time_vec))


#loop for stepping through time steps
for (i in seq_along(time_vec)){
  population <- Logistic_growth(N0 = 100, K = 6000, r = .27, time = time_vec[i])
  #store this 
  pop_35_vec[i] <- population
}


#now building to estimating across growth rates
#creating a series of growth rates:
r_seq <- seq(from = 0.2, to = 0.4, by = .01)

#create a matrix because before it was a vector and store output values
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for (j in seq_along(r_seq)){
  for (i in seq_along(time_vec)) {
    population <- Logistic_growth(N0 = 100, K = 6000, 
                                  r = r_seq[j], time = time_vec[i])
    out_matrix[i, j] <- population
  }
}
# to view view(out_matrix)

# data wrangling to plot

#adding time as a variable
out_df <- data.frame(out_matrix, time = time_vec)

#update column names for growth rates
colnames(out_df) <- c(paste0("gr_", r_seq),"time")

#pivot longer to make it tidy
out_df_long <- out_df %>% 
  pivot_longer(cols = -time, 
                names_to = "growth_rate",
                values_to = "population")

#plot it
ggplot(data = out_df_long,
       aes(x = time, y = population)) + 
  geom_line(aes(color = growth_rate)) + 
  theme_minimal()




#











dosage <- c(10,100,1000)
dose_index <- vector(mode = "character", length = length(dosage))

for (i in seq_along(dosage)) {
  k <- paste0("zinc_",dosage[i])
  dose_index[i] <- k
  print(dose_index)
}

file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1,2,3,4,5)

for (i in seq_along(file_prefix)) {
  for (j in seq_along(file_suffix)) {
    print(paste0(file_prefix[i],"_",file_suffix[j]))
  }
}


#tring to create a loop within a loop
odds <- c(1,3,5)
evens <- c(2,4,6,8)
randos <- c(3,4,5,6)

for (i in seq_along(odds)) {
  for (j in seq_along(evens)) {
    for (k in seq_along(randos)) {
      print(odds[i] * evens[j] * randos[k])
    }
  }
}
