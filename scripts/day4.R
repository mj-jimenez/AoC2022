rm(list = ls())
library(tidyverse)

# --- Functions ---
create_vector <- function(x) {
  start_end <- as.numeric(unlist(strsplit(x, split = "-")))
  start_end[1]:start_end[2]
}

shorter_vector <- function(x, y) {
  v <- list(x, y)
  v[[which.min(sapply(v, length))]]
}

# --- Data ---
assignments <- read.table("data/day4.txt", sep = ",")

# --- Code ---
# Ranges table
ranges <- assignments %>% rename(Elf1 = V1, Elf2 = V2)

# --- Answers ---
# How many assignment pairs does one range fully contain the other?
full_overlap <- apply(ranges, 1, FUN = function(x) {
  vx <- create_vector(x["Elf1"])
  vy <- create_vector(x["Elf2"])
  shorter <- shorter_vector(vx, vy)
  return(identical(sort(intersect(vx, vy)), shorter))
})

sum(full_overlap)

# How many assignment pairs do the ranges overlap
overlap <- apply(ranges, 1, FUN = function(x) {
  vx <- create_vector(x["Elf1"])
  vy <- create_vector(x["Elf2"])
  return(length(intersect(vx, vy)) > 0)
})

sum(overlap)
