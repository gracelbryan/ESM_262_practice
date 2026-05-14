# Assignment 4: Simpson Biodiversity Index
# Grace Bryan
# May 13, 2026

# simpson_index.R

# Function to calculate Simpson's Biodiversity Index (D)

# Inputs: 
#   species_vector 
#     a vector containing species names or IDs

# Outputs: 
#   A numeric value between 0 and 1 representing 
#   Simpson's Index

# Description:
#   Simpson's Index measures the probability that two 
#   individuals randomly selected from a sample belong
#   to the same species.

# Formula used:
#   D = sum(n * (n -1)) / (N * (N - 1))

#   where:
#     n = number of individuals of each species
#     N = total number of individuals

# Lower values indicate lower diversity.

simpson_index <- function(species_vector) {
  
  # Convert to factor to count species
  species_factor <- factor(species_vector)
  
  # Count individuals in each species
  counts <- table(species_factor)
  
  # Total number of individuals
  N <- sum(counts)
  
  # Prevents division by zero
  if (N < 2) {
    return(NA)
  }
  
  # Calculate Simpson's Index
  D <- sum(counts * (counts - 1)) / (N * (N - 1))
  
  return(D)
}


summarize.diversity = function(species) {
  
  species = as.factor(species)
  
  # use simple simpson form
  tmp = (summary(species)/sum(summary(species))) ** 2
  # change to simpson index - more intuitive
  diversity = 1.0-sum(tmp)
  
  # number of species
  nspecies = length(summary(species))
  
  # which is the most frequent
  tmp = which.max(summary(species))
  dominant = names(summary(species)[tmp])
  
  # output from function
  return(list(num=nspecies, simpson=diversity, dominant=dominant))
}
  