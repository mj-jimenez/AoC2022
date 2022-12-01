rm(list = ls())

# --- Data ---
calories <- scan("data/day1_1.txt", what = numeric(), blank.lines.skip = FALSE)

# --- Code ---
# NA values (indicate separation from an elf to another)
na_values <- which(is.na(calories))

# Start and end positions for each elf in the calories vector
ranges <- data.frame(start = c(1, na_values + 1),
                     end = c(na_values - 1, length(calories)))

# Split calories to a vector with each elf's calories
elf_calories <- sapply(1:nrow(ranges), FUN = function(i) {
  thisline <- ranges[i, ]
  sum(calories[seq(thisline$start, thisline$end)])
})

# --- Answers ---
# Max calories carried by 1 elf
max(elf_calories)

# Total calories carried by the top 3 elves
sum(sort(elf_calories, decreasing = TRUE)[1:3])
