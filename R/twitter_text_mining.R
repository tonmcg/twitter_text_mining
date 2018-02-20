packages <- c("lubridate", "ggplot2", "dplyr", "readr", "tidytext", "stringr", "tidyr", "scales", "purrr", "broom")

install.packages(packages)

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

tweets_tony <- read_csv("data/tweets/tweets.csv")
tweets <- tweets_tony %>%
  mutate(person = "Tony") %>%
  mutate(timestamp = ymd_hms(timestamp))

ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE)

## Look at Frequency of Words

library(tidytext)
library(stringr)

replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# first filter out all retweets
# filter out all unwanted characters defined by the regex above (but leave in hashtags!)
# take out stop words
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


frequency <- tidy_tweets %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_tweets %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

library(tidyr)

frequency_narrow <- frequency %>%
  select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Tony)

frequency_narrow

library(scales)

ggplot(frequency_narrow, aes(Tony, Tony)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


# Word Usage Over Time
word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  arrange(desc(Tony))

word_ratios

word_ratios %>%
  top_n(15, Tony) %>%
  ungroup() %>%
  mutate(word = reorder(word, Tony)) %>%
  ggplot(aes(word, Tony, fill = Tony < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Tony)") +
  scale_fill_discrete(name = "", labels = c("Tony"))

# Changes in Words Over Time
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  ungroup() %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time

nested_data <- words_by_time %>%
  nest(-word, -person)

nested_data

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, .,
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))


top_slopes <- slopes %>%
  filter(adjusted.p.value < 0.1) %>%
  select(-statistic, -p.value)

top_slopes


words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  ggplot(aes(time_floor, count/time_total, color = word, lty = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")
