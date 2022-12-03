rm(list = ls())
library(tidyverse)

# --- Data ---
strategy <- read.table("data/day2.txt", header = FALSE)

# --- Code ---
# Correspondences
eq <- data.frame(abc = c("A", "B", "C"), ldw = c("Loss", "Draw", "Win"),
                 xyz = c("X", "Y", "Z"))

# Possible results
results <- expand.grid(eq$xyz, eq$xyz) %>%
  rename(Opponent = Var1, Me = Var2) %>%
  mutate(ldw = case_when(Opponent == Me ~ "Draw",
                         (Opponent == "X" & Me == "Z") | 
                           (Opponent == "Y" & Me == "X") |
                           (Opponent == "Z" & Me == "Y") ~ "Loss",
                         TRUE ~ "Win")) %>%
  merge(eq) %>% select(Opponent, Me, xyz) %>% rename(Result = xyz)

# Rock, Paper, Scissors' scores
shapes <- data.frame(Me = eq$xyz, Shape = c(1, 2, 3))

# Outcome scores
outcomes <- data.frame(ldw = c("Loss", "Draw", "Win"), Outcome = c(0, 3, 6)) %>%
  merge(eq) %>% select(xyz, Outcome) %>% rename(Result = xyz)

# Merge all tables
merged <- results %>% merge(shapes) %>% merge(outcomes)

# Reformat input table
colnames(strategy) <- c("abc", "Unknown")
strategy <- strategy %>% merge(eq) %>% 
  rename(Opponent = xyz) %>% select(Opponent, Unknown)

# --- Answers ---
# Total score assuming unknown is Rock, Paper, Scissors
strategy %>% rename(Me = Unknown) %>% merge(merged) %>% 
  mutate(Final = Shape + Outcome) %>% pull(Final) %>% sum()

# Total score assuming unknown is Win, Loss, Draw
strategy %>% rename(Result = Unknown) %>% merge(merged) %>% 
  mutate(Final = Shape + Outcome) %>% pull(Final) %>% sum()