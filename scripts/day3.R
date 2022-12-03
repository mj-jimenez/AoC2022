rm(list = ls())

# --- Functions ---
divide_string <- function(x) {
  len <- nchar(x)
  c(substr(x, 1, len/2), substr(x, (len/2)+1, len))
}

which_duplicated <- function(x) {
  items <- lapply(x, function(y) unlist(strsplit(y, "")))
  unique(Reduce(intersect, items))
}

# --- Data ---
contents <- scan("data/day3.txt", what = character())

# --- Code ---
# Item priority
priority <- c(letters, toupper(letters))

# --- Answers ---
# Sum of the priorities of duplicated items
dup_items <- sapply(contents, FUN = function(x) {
  which(priority %in% which_duplicated(divide_string(x)))
})

sum(dup_items)

# Sum of the priorities of the badges
n_groups <- length(contents)/3
elf_groups <- split(contents, factor(sort(c(1:length(contents))%%n_groups)))
badges <- sapply(elf_groups, FUN = function(x) {
  which(priority %in% which_duplicated(x))
})

sum(unlist(badges))
