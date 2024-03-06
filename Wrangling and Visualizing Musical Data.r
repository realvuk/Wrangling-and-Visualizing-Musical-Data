
# Loading individual Tidyverse packages
# .... YOUR CODE FOR TASK 1 ....
library(dplyr)
library(readr)
library(ggplot2)
# Reading in the McGill Billboard chord data
bb <- read_csv('datasets/bb_chords.csv')

# Taking a look at the first rows in bb
# .... YOUR CODE FOR TASK 1 ....
head(bb)

# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("Read in data correctly.", {
        expect_is(bb, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/bb_chords.csv" into bb')
    })
    
    test_that("Read in data correctly.", {
        bb_correct <- read_csv('datasets/bb_chords.csv')
        expect_equivalent(bb, bb_correct, 
            info = 'bb should contain the data in "datasets/bb_chords.csv"')
    })
})

# Counting the most common chords
bb_count <- bb %>%
            count(chord, sort = TRUE)

# Displaying the top 20 chords
# .... YOUR CODE FOR TASK 2 ....
bb_count %>% slice(1:20)

run_tests({
    test_that("bb_count is correct", {
        correct_bb_count <- bb %>%
          count(chord, sort = TRUE)
        expect_equivalent(bb_count, correct_bb_count, 
            info = "bb_count should contain the count of each type of chord.")
    })
})

# Creating a bar plot from bb_count
bb_count %>%
  slice(1:20) %>%
  mutate(share = n / sum(n),
         chord = reorder(chord, share)) %>%
  ggplot(aes(chord, share, fill = chord)) +
  geom_col() +
  coord_flip() +
  ylab('Share of total chords') +
  xlab('Chord') +
  theme(legend.position="none")

run_tests({
    test_that("bb_count has some data in it", {
    expect_true(length(bb_count) > 0, 
        info = "Looks like you're missing data in `bb_count`.")
    })
})

# Wrangling and counting bigrams
bb_bigram_count <- bb %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, sort = TRUE)
# Displaying the first 20 rows of bb_bigram_count
# .... YOUR CODE FOR TASK 4 ....

run_tests({
    test_that("bb_bigram_count is correct", {
      correct_bb_bigram_count <- bb %>%
      mutate(next_chord = lead(chord),
             next_title = lead(title),
             bigram = paste(chord, next_chord)) %>%
      filter(title == next_title) %>%
      count(bigram, sort = TRUE)
    expect_equivalent(bb_bigram_count, correct_bb_bigram_count, 
        info = "`bb_bigram_count` should contain the count of each type of bigram. Don't forget to sort by bigram frequency!")
    })
})

# Creating a column plot from bb_bigram_count

bb_bigram_count %>%
  slice(1:20) %>%
  mutate(share = n / sum(n),
         bigram = reorder(bigram, share)) %>%
  ggplot(aes(bigram, share, fill = bigram)) +
  geom_col() +
  coord_flip() +
  xlab('Next Chord') +
  ylab('Share of total chords') +
    theme(legend.position="none")

run_tests({
    test_that("bb_bigram_count has some data in it", {
    expect_true(length(bb_bigram_count) > 0, 
        info = "Looks like you're missing data in `bb_bigram_count`.")
    })
})

# Finding the 30 artists with the most songs in the corpus
bb_30_artists <- bb %>%
  select(artist, title) %>%
  unique() %>%
  count(artist, sort = TRUE)

# Displaying 30 artists with the most songs in the corpus
bb_30_artists %>%
  slice(1:30)

run_tests({
    test_that("bb artists counted and sorted", {
      correct_bb_30_artists <- bb %>%
        select(artist, title) %>%
        unique() %>%
        count(artist, sort = TRUE)
    expect_equivalent(bb_30_artists, correct_bb_30_artists, 
        info = "`bb_30_artists` should contain the number of soungs (not chords) by each artist in the corpus. Don't forget to sort!")
    })
})

tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

# Creating a new dataframe bb_tagged that includes a new column instrument from tags
bb_tagged <- bb %>%
  inner_join(tags)

# Displaying the new data frame
bb_tagged

run_tests({
    test_that("bb artists counted and sorted", {
      correct_bb_tagged <- bb %>%
        inner_join(tags)
    expect_equivalent(bb_tagged, correct_bb_tagged, 
        info = "`bb_tagged` should be a successful join of `bb` and `tags` that only contains records cointained in both dataframes.")
    })
})

# The top 20 most common chords
top_20 <- bb_count$chord[1:20]

# Comparing the frequency of the 20 most common chords in piano- and guitar-driven songs
bb_tagged %>%
  filter(chord %in% top_20) %>%
  count(chord, instrument, sort = TRUE) %>%
  ggplot(aes(chord, n, fill = chord)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  ylab('Total chords') +
  xlab('Chord') +
  theme(legend.position="none")

run_tests({
    test_that("bb_tagged has some data in it", {
    expect_true(length(bb_tagged) > 0, 
        info = "Looks like you're missing data in `bb_tagged`.")
    })
})

# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, instrument, sort = TRUE) %>%
  filter(bigram %in% top_20_bigram) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  ylab('Total bigrams') +
  xlab('Bigram') +
  theme(legend.position="none")

run_tests({
    test_that("bb_bigram_count has some data in it", {
    expect_true(length(bb_bigram_count) > 0, 
        info = "Looks like you're missing data in `bb_bigram_count`.")
    })
})

# Set to TRUE or FALSE to reflect your answer
hypothesis_valid <- TRUE

# Set to TRUE or FALSE to reflect your answer
more_data_needed <- TRUE

run_tests({
    test_that("hypothesis is true", {
    expect_true(hypothesis_valid, 
        info = "Are you sure the hypothesis isn't valid?!")
    })
    test_that("more_data_needed is true", {
    expect_true(more_data_needed, 
        info = "Are you sure we don't need more data?!")
    })
})


