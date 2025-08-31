# ===================================================
# The Semantics of Non-Consent — H1/H2 Analysis
# Lodovica Sofia Cosner (257507)
# Cultural Analytics - Final Paper
# A.Y. 2024/2025
# ===================================================

# --- 1) Packages --> main libraries for text analysis, 
# data wrangling, visualization, and topic modeling. ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(wordcloud)
  library(RColorBrewer)
  library(topicmodels)
})
# --- 1.1) Reproducibility ---
set.seed(1234)  # to ensure reproducibility for all random operations

# --- 2) Data loading & normalization ---
# Importing the dataset of titles, assigning IDs, 
# normalizing the text by lowercasing, trimming, 
# replacing apostrophes and standardizing hyphens
data <- read_excel("C:/Users/cosne/Documents/dataset_CA.xlsx", sheet = "Foglio1") %>%
  mutate(
    id = row_number(),
    title = coalesce(title, ""),
    title_clean = title |> str_to_lower() |> str_squish(),
    title_clean = str_replace_all(title_clean, "’", "'"),
    title_clean = str_replace_all(title_clean, "[-_]+", " ")
  )

# --- 3) Dictionaries (H1 explicit/euphemistic/degrading; H2 phrases/tokens) ---
# Creating dictionary lists for each category:
# - H1: words related to explicit, euphemistic, or degrading violence
# - H2: phrases/tokens related to retro-consent narratives
# These will later be used to annotate the dataset.
violence_explicit <- c(
  "force","forced","forcing","forceful","forcefully",
  "rape","raped","raping","rapist",
  "molest","molested","molesting",
  "abuse","abused","abusing",
  "assault","attacked","attacking",
  "beat","beating","beaten","hit","smack","slap","slapped",
  "choke","choked","choking","strangle","strangled",
  "torture","tortured","torturing",
  "kidnap","kidnapped","kidnapping","abduct","abducted","abducting",
  "restrain","restrained","binding","bound","tied","tied down","gagged",
  "drugged","unconscious",
  "brutal","brutality","savage","violent","violently",
  "destroy","destroyed","wreck","wrecked","ruined","broken",
  "victim","crying","screaming","subdue","subdued","subduing",
  "beatdown","smash","smashed"
)

violence_euphemistic <- c(
  "rough","hardcore","pounded","pounding",
  "ravish","ravished","ravishing",
  "taken","taking","captured",
  "deflower","deflowered","deflowering",
  "virgin","virginity","innocence","innocent","pure",
  "tiny","little","barely","barely legal","first time","first date",
  "naive","fragile","shy","timid",
  "helpless","powerless","obedient","submissive","slave",
  "disciplined","discipline","punish","punished","punishing",
  "dominate","dominated","owned","subjugated",
  "asleep","sleeping","against her will"
)

violence_degrading <- c(
  "slut","whore","hoe","skank","bitch",
  "cuck","cuckold",
  "cheat","cheating","cheated",
  "humiliate","humiliated","humiliating","humiliation",
  "degrade","degraded","degrading",
  "objectified","objectify",
  "public use","gangbang","gang bang","bukkake"
)

retro_consent_phrases <- c(
  "she said no","she said no but","said no but",
  "she didn't want","she didnt want",
  "at first","at first resisted","at first she refused",
  "but then","and then she","until she gave in",
  "made her enjoy","made her like","made her want","made her want it",
  "couldn't resist","could not resist","couldnt resist","cant resist",
  "had no choice","no choice",
  "pretended not to","pretended she didn",
  "acting shy","secretly wanted","started to like",
  "ended up","ended up liking",
  "eventually enjoyed","finally enjoyed","reluctantly enjoyed","reluctantly agreed",
  "against her will but liked",
  "tricked into","fooled into","coerced into",
  "hesitated at first","resisted but","said stop but",
  "couldn't say no","couldnt say no","after saying no","though she resisted",
  "she gave up","she gave in","she gave herself",
  "she started enjoying","she loved it eventually","she finally wanted","eventually wanted",
  "after a while","after","later",
  "she lost control","couldn't help herself"
)

retro_consent_tokens <- c(
  "resist","resisted","resisting",
  "refuse","refused","refusing",
  "deny","denied","denying",
  "reject","rejected","rejecting",
  "hesitant","hesitation","reluctant","reluctantly",
  "unwilling","unwillingly","hesitate","shy","timid",
  "gave in","submit","submitted","surrender","relented","yielded",
  "turned on","enjoyed","enjoying","pleasure","liked","liking",
  "finally","eventually","ended up","after a while","after","later",
  "secretly","hidden desire","couldn't help","couldnt help","cant resist","lose control"
)

# Helper function to collapse dictionaries into regex patterns.
# This makes it easier to apply pattern matching in the next step.
re_or <- function(x){
  x <- unique(x)
  paste0("\\b(", paste(x, collapse="|"), ")\\b")
}

# --- 4) H1/H2 flags ---
# Apply the dictionaries to the cleaned titles. 
# Each title is checked for matches and we create binary flags (0/1) 
# for explicit, euphemistic, degrading, phrase-based H2, and token-based H2. 
# A combined label is also added for easier grouping.
data <- data %>%
  mutate(
    h1_explicit = as.integer(str_detect(title_clean, regex(re_or(violence_explicit), ignore_case=TRUE))),
    h1_euph     = as.integer(str_detect(title_clean, regex(re_or(violence_euphemistic), ignore_case=TRUE))),
    h1_degrad   = as.integer(str_detect(title_clean, regex(re_or(violence_degrading), ignore_case=TRUE))),
    h1_flag     = pmax(h1_explicit, h1_euph, h1_degrad),
    
    h2_phrase   = as.integer(str_detect(title_clean, regex(re_or(retro_consent_phrases), ignore_case=TRUE))),
    h2_token    = as.integer(str_detect(title_clean, regex(re_or(retro_consent_tokens),  ignore_case=TRUE))),
    h2_flag     = pmax(h2_phrase, h2_token),
    
    combined = case_when(
      h1_flag==1 & h2_flag==1 ~ "H1+H2",
      h1_flag==1 & h2_flag==0 ~ "H1 only",
      h1_flag==0 & h2_flag==1 ~ "H2 only",
      TRUE ~ "None"
    )
  )

# Reorder combined categories for clearer plots later
data <- data %>%
  mutate(combined = factor(combined, levels = c("None","H1 only","H2 only","H1+H2")))

# --- 5) Descriptives + barplots ---
# First, check how many titles fall into H1/H2 categories (counts and %). 
# Then visualize these distributions with barplots.
cat("\n== Descriptive statistics ==\n")
print(table(H1 = data$h1_flag))
print(round(prop.table(table(data$h1_flag))*100, 2))
print(table(H2 = data$h2_flag))
print(round(prop.table(table(data$h2_flag))*100, 2))
print(table(Combined = data$combined))

# Barplots with Yes/No labels
ggplot(data, aes(x = factor(h1_flag, levels=c(0,1), labels=c("No","Yes")))) +
  geom_bar(fill="dodgerblue4") +
  labs(title="Distribution of H1 (Violence)", x="H1", y="Count") +
  theme_minimal()

ggplot(data, aes(x = factor(h2_flag, levels=c(0,1), labels=c("No","Yes")))) +
  geom_bar(fill="steelblue") +
  labs(title="Distribution of H2 (Retro-consent)", x="H2", y="Count") +
  theme_minimal()

ggplot(data, aes(x = combined, fill = combined)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "None"   = "grey70",
    "H1 only" = "steelblue",
    "H2 only" = "dodgerblue4",
    "H1+H2"  = "navy"
  )) +
  labs(title = "Distribution of combined categories (H1/H2)", x = "Category", y = "Count") +
  theme_minimal()

cat("\nBreakdown H1 (explicit / euphemistic / degrading):\n")
data %>%
  summarise(
    explicit = sum(h1_explicit),
    euphemistic = sum(h1_euph),
    degrading = sum(h1_degrad)
  ) %>%
  pivot_longer(everything(), names_to="type", values_to="count") %>%
  mutate(percent = round(100*count/nrow(data),2)) %>%
  print()

# Distribution of sub-categories of H1
h1_breakdown <- data %>%
  summarise(
    explicit    = sum(h1_explicit),
    euphemistic = sum(h1_euph),
    degrading   = sum(h1_degrad)
  ) %>%
  pivot_longer(everything(), names_to = "type", values_to = "count") %>%
  mutate(percent = 100 * count / nrow(data))

ggplot(h1_breakdown, aes(x = type, y = percent, fill = type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c(
    "explicit"    = "#4F6D7F",
    "euphemistic" = "#8CA0AD",
    "degrading"   = "grey"
  )) +
  labs(
    title = "Distribution of H1 subdimensions (Violence)",
    x = "Subdimension", y = "Percentage"
  ) +
  theme_minimal()

# --- 6) Distribution by platform / retrieval ---
# since the dataset includes columns 'platform' and 'retrieval_method', 
# I check how H1/H2 are distributed across them (in %)
# using stacked barplots with proportions.
if ("platform" %in% names(data)) {
  platform_summary <- data %>%
    group_by(platform) %>%
    summarise(total = n(),
              h1_count = sum(h1_flag),
              h2_count = sum(h2_flag),
              .groups = "drop")
  print(platform_summary)
}
  ggplot(data, aes(x = platform, fill = combined)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c(
      "None"    = "#BCC5D1",  
      "H1 only" = "#8CA0B3",  
      "H2 only" = "#708090",  
      "H1+H2"   = "#4F6D7A"   
    )) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "H1/H2 distribution by platform",
         x = "Platform", y = "Percent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

if ("retrieval_method" %in% names(data)) {
  retrieval_summary <- data %>%
    group_by(retrieval_method) %>%
    summarise(total = n(),
              h1_count = sum(h1_flag),
              h2_count = sum(h2_flag),
              .groups = "drop")
  print(retrieval_summary)
}
  
  ggplot(data, aes(x = retrieval_method, fill = combined)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c(
      "None"    = "#CAD2D8",  
      "H1 only" = "#9BA8B0",  
      "H2 only" = "#7A8A99",  
      "H1+H2"   = "#556677"   
    )) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "H1/H2 distribution by retrieval method",
         x = "Retrieval method", y = "Percent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))


# --- 7) Tokenization + stopwords ---
# Tokenize the titles into individual words and remove stopwords 
# (common words and custom pornography-related vocabulary).
data("stop_words")
custom_block <- unique(c(
  "anal","pussy","dick","cock","cum","blowjob","porno","porn","sex","hot",
  "amateur","teen","milf","ass","naked","nude","big","boobs","video","full","free",
  "official","clip","hd","fuck","fucks","fucked","creampie","hole","step","hard",
  "fucking","wife","sexy","home","girl","stepsister","stepdaughter","stepmom",
  "stepdad","stepbrother","couple","orgasm","inside","romantic","friend","girlfriend",
  "boyfriend","husband","massage","panties","sister","babe","time",
  "didn","dont","doesn","cant","couldnt","wouldnt","shouldnt","wasnt","werent","im","ive",
  "asian","brunette","night"
))

tidy_titles <- data %>%
  select(id, title_clean) %>%
  unnest_tokens(word, title_clean)

tidy_titles_filtered <- tidy_titles %>%
  filter(
    !word %in% stop_words$word,
    !word %in% custom_block,
    !str_detect(word, "^[0-9]+$")
  )

cat("\nTop 20 tokens (filtered):\n")
print(tidy_titles_filtered %>% count(word, sort=TRUE) %>% head(20))

# --- 8) Word cloud ---
# Building a word cloud of the most frequent (filtered) terms. 
# This helps visualize the most salient vocabulary in the dataset.
wc_colors <- c("#556677", "#6C7A89", "#7E8C9A", "#9BA8B0",
               "#B0BEC5", "#CFD8DC", "#90A4AE", "#607D8B")
word_freq <- tidy_titles_filtered %>% count(word, sort = TRUE)
wordcloud(
  words = word_freq$word,
  freq  = word_freq$n,
  max.words = 50,     
  min.freq  = 2,      
  random.order = FALSE,
  colors = wc_colors
)
title("Word Cloud (Top 50 filtered terms)")

# --- 9) TF–IDF distinctive terms ---
# Compare which terms are most distinctive for H1, H2 or H1+H2, 
# using TF–IDF weighting. Exclude titles with no category.
tidy_labeled <- tidy_titles_filtered %>%
  inner_join(data %>% select(id, h1_flag, h2_flag), by="id") %>%
  mutate(group = case_when(
    h1_flag == 1 & h2_flag == 0 ~ "H1 only",
    h1_flag == 0 & h2_flag == 1 ~ "H2 only",
    h1_flag == 1 & h2_flag == 1 ~ "H1+H2",
    TRUE ~ "None"
  ))

tidy_labeled_nz <- tidy_labeled %>% filter(group != "None")

term_counts <- tidy_labeled_nz %>% count(group, word, sort = TRUE)
term_tfidf  <- term_counts %>% bind_tf_idf(term = word, document = group, n = n)

top_tfidf <- term_tfidf %>%
  group_by(group) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

# Plot the top distinctive words per group
# Plot the top distinctive words per group (ordered facets)
top_tfidf %>%
  mutate(
    group = factor(group, levels = c("H1 only", "H2 only", "H1+H2")),
    word  = reorder_within(word, tf_idf, group)
  ) %>%
  ggplot(aes(tf_idf, word, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free") +
  scale_y_reordered() +
  scale_fill_manual(values = c(
    "H1 only" = "#4F6D7A",     
    "H2 only" = "dodgerblue4", 
    "H1+H2"   = "#8CA0AD"      
  )) +
  labs(
    title = "Most distinctive terms by group (TF–IDF)",
    x = "TF–IDF", y = "Term"
  ) +
  theme_minimal()


# --- 10) Bigrams ---
# Extract and count word pairs (bigrams) to see which sequences of words 
# are most common overall and inside H1/H2 categories.
bigrams_all <- data %>%
  select(id, title_clean, h1_flag, h2_flag) %>%
  unnest_tokens(bigram, title_clean, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("w1","w2"), sep = " ") %>%
  filter(
    !w1 %in% stop_words$word, !w2 %in% stop_words$word,
    !w1 %in% custom_block,    !w2 %in% custom_block,
    !str_detect(w1, "^[0-9]+$"), !str_detect(w2, "^[0-9]+$")
  ) %>%
  unite("bigram", w1, w2, sep = " ")

# overall top 10
bigram_counts <- bigrams_all %>% count(bigram, sort = TRUE)
cat("\nTop 10 bigrams (overall):\n"); print(head(bigram_counts, 10))
bigram_counts %>% slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "darkgrey") +
  coord_flip() +
  labs(title = "Top 10 bigrams (overall)", x = "Bigram", y = "Frequency") +
  theme_minimal()

# H1-only
bigrams_h1 <- bigrams_all %>% filter(h1_flag == 1) %>% count(bigram, sort = TRUE)
bigrams_h1 %>% slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "#556677") +
  coord_flip() +
  labs(title = "Top 10 bigrams (H1 titles)", x = "Bigram", y = "Frequency") +
  theme_minimal()

# H2-only
bigrams_h2 <- bigrams_all %>% filter(h2_flag == 1) %>% count(bigram, sort = TRUE)
bigrams_h2 %>% slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue4") +
  coord_flip() +
  labs(title = "Top 10 bigrams (H2 titles)", x = "Bigram", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold") 
  )

# --- 11) Topic modeling (LDA, k=3) ---
# Run a topic model separately for H1 and H2 subsets (k=3 topics each). 
# This is exploratory: we identify clusters of words that tend to co-occur.
# H1
tidy_h1 <- tidy_titles_filtered %>%
  inner_join(data %>% select(id, h1_flag), by="id") %>%
  filter(h1_flag==1)

dtm_h1 <- tidy_h1 %>%
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n)

if (nrow(dtm_h1) > 0) {
  lda_h1 <- LDA(dtm_h1, k=3)
  top_terms_h1 <- tidy(lda_h1, matrix="beta") %>%
    group_by(topic) %>% slice_max(beta, n=10, with_ties=FALSE) %>% ungroup()
  
  top_terms_h1 %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill=factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    scale_fill_manual(values = c(
      "1" = "#4F6D7A",   
      "2" = "#8CA0AD",   
      "3" = "#A9B7C6"    
    )) +
    labs(
      title="Top terms per topic (H1 - Violence)",
      x="Beta", y="Term"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=13, face="bold")
    )
  }

# H2
tidy_h2 <- tidy_titles_filtered %>%
  inner_join(data %>% select(id, h2_flag), by="id") %>%
  filter(h2_flag==1)

dtm_h2 <- tidy_h2 %>%
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n)

if (nrow(dtm_h2) > 0) {
  lda_h2 <- LDA(dtm_h2, k=3)
  top_terms_h2 <- tidy(lda_h2, matrix="beta") %>%
    group_by(topic) %>% slice_max(beta, n=10, with_ties=FALSE) %>% ungroup()
  
  top_terms_h2 %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill=factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    scale_fill_manual(values = c(
      "1" = "#6C7A89",   
      "2" = "#9A9EAB",   
      "3" = "#C0C5CE"    
    )) +
    labs(
      title="Top terms per topic (H2 - Retro-consent)",
      x="Beta", y="Term"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=13, face="bold")
    )
}

# --- 12) Exports ---
# Saving annotated dataset and topic modeling outputs

write.csv(data, "annotated_dataset.csv", row.names = FALSE)

if (exists("top_terms_h1")) {
  write.csv(top_terms_h1, "h1_topics_terms.csv", row.names = FALSE)
}

if (exists("top_terms_h2")) {
  write.csv(top_terms_h2, "h2_topics_terms.csv", row.names = FALSE)
}

if (exists("platform_summary")) {
  write.csv(platform_summary, "summary_by_platform.csv", row.names = FALSE)
}

if (exists("retrieval_summary")) {
  write.csv(retrieval_summary, "summary_by_retrieval.csv", row.names = FALSE)
}

# export citations
citation() # for R
citation("tidyverse")
citation("readxl")
citation("stringr")
citation("tidytext")
citation("ggplot2")
citation("wordcloud")
citation("RColorBrewer")
citation("topicmodels")