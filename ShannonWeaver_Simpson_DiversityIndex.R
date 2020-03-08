
species <- c(100, 250, 150, 300, 165, 230)
speciesnames <- list("banana", "apple", "grape", "pear", "pineapple", "satsuma")
names(species) <- speciesnames


# Shannon-Weaver (H) (also in "vegan" package)
H <- function(species) {
  shanvec <- c()
  for (i in 1:length(species)) {
    p <- species[i]/sum(species)
    plog <- log(p)
    shan <- -(p*plog)
    shanvec <- append(shanvec, shan)
  }
  H <- sum(shanvec)
  return(H)
}


# Inverted Simpson (D)
D <- function(species) {
  simpvec <- c()
  for (i in 1:length(species)) {
    p <- species[i]/sum(species)
    simpvec <- append(simpvec, p^2)
  }
  D <- 1/(sum(simpvec))
  return(D)
}

# Sorenson's coefficient
S1_species <- c(100, 250, 150, 300, 165, 230)
S2_species <- c(200, 150, 300, 200, 100)
S2_speciesnames <- list("banana", "orange", "apple", "pear", "pineapple")
S1_speciesnames <- list("banana", "apple", "grape", "pear", "pineapple", "satsuma")

names(S1_species) <- S1_speciesnames
names(S2_species) <- S2_speciesnames

S <- function(S1_species, S2_species) {
  containsvec <- c()
  for (i in 1:length(S1_species)) {
    if(names(S1_species[i]) %in% names(S2_species)) {
      containsvec <- append(containsvec, 1)
    }
  }
  C <- length(containsvec)
  S <- (2*C)/(length(S1_species)+length(S2_species))
  return(S)
}

# Sorenson's Coefficient
S(S1_species, S2_species)
# Inverted Simpson Index (D)
D(species)
# Shannon-Weaver Index (H)
H(species)
