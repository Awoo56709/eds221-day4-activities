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
