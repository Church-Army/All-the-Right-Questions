
library(dplyr)
library(stringr)

psyhcic_test <- function(name, guess, actual){
  stopifnot(length(guess) == length(actual))
  
  tibble(name = name, guess = guess, actual = actual)
}

testing <-
  rbind(
  psyhcic_test("Dan Palmer",
               c("9C", "4H", "3C", "JC", "4D", "4D", "7C", "JD", "AD", "8H"),
               c("10S", "9D", "10D", "6H", "8H", "KD", "QD", "KH", "QC", "AC")),
  psyhcic_test("Alex Dobson",
               c("3H", "10S", "10D", "2D", "4C", "9H", "9D", "6S", "4S", "KD"),
               c("6C", "10C", "2S", "4D", "AH", "AS", "6D", "6S", "JS", "JH")),
  psyhcic_test("Mystery Candidate 1",
               c("7D", "2C", "JH", "JH", "KC", "AD", "7H", "QC", "4D", "4H"),
               c("10S", "9D", "10C", "5S", "2D", "AS", "2H", "2S", "9C", "AH")),
  psyhcic_test("Mystery Candidate 2",
               c("JC", "3H", "7S", "8S", "9S", "7H", "8C", "AH", "6S", "QH"),
               c("8D", "9H", "9C", "AD", "4H", "AS", "6C", "10C", "JC", "4S"))
  
)

testing <- 
  mutate(testing,
         across(c(guess, actual),
                .fns = 
                  list(
                    rank = \(x){
                      str_sub(x, 1, -2) |> 
                        replace_values("A" ~ "Ace", "J" ~ "Jack", "Q" ~ "Queen", "K" ~ "King")
                    },
                    suit = \(x){
                      str_sub(x, -1,-1) |> 
                        recode_values("C" ~ "Clubs", "H" ~ "Hearts", "S" ~ "Spades", "D" ~ "Diamonds")
                    }
                  )))

testing <- 
  mutate(testing,
         rank_correct = guess_rank == actual_rank,
         suit_correct = guess_suit == actual_suit,
         suit_and_rank_correct = rank_correct & suit_correct)

saveRDS(here("2026", "data", "psychic-testing_2026.rds"))
