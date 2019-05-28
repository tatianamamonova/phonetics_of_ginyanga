library(tidyverse)
library(ggridges)
theme_set(theme_bw())

my_vowels_raw <- read_csv("C:/study/afr/gin/kursach_/results/durations/dur_res.csv")
View(my_vowels_raw)

my_vowels_raw %>%
  mutate(atr = factor(ifelse(str_detect(vowel, "i|o|u|e|a|ə"), "+ATR", "-ATR")),
         grand_vowel = case_when(
           str_detect(vowel, "i|ɩ") ~ "I",
           str_detect(vowel, "o|ɔ") ~ "O",
           str_detect(vowel, "e|ɛ") ~ "E",
           str_detect(vowel, "u|ʋ") ~ "U",
           TRUE ~ "t")) ->
  my_vowels_raw


my_vowels_raw %>% 
  filter(str_detect(vowel, 'ɛ|e|o|ɔ|i|ɩ|ə|a')) %>%
  ggplot(aes(vowel, duration, fill = factor(part_of_word))) +
  geom_boxplot()+
  labs(x = "difference between F2 ɛ and e", y = "")+
  facet_wrap(~speaker)


lmm <- glmer(duration ~ vowel + syl_in_word + num_syl + first + final + (1 | speaker), data = my_vowels_raw, family = "binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(lmm)


my_vowels_raw %>% 
  filter(syl_in_word == 1 )%>%
  filter(!str_detect(vowel, 'ɩ') )%>%
  group_by(vowel) %>% 
  mutate(mn = median(duration, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(vowel = reorder(vowel, mn)) %>%
  ggplot(aes(x = duration, fill = vowel)) +
  geom_density(alpha =0.4)+
  #geom_boxplot()+
  theme(text = element_text(size = 20))+
  theme_bw()+
  facet_grid(vowel~speaker, scales = 'free')

my_vowels_raw %>% 
  ggplot(aes(vowel, duration, fill = factor(nasal))) +
  geom_boxplot()+
  facet_wrap(~speaker)

my_vowels_raw %>% 
  filter(syl_in_word != 1 )%>%
  filter(!str_detect(vowel, 'aa'))%>%
  ggplot(aes(factor(first), duration, fill = vowel)) +
  geom_boxplot()+
  labs(x = "number of syllable", y = "duration, ms", fill = 'number of syllables in the word')+
  facet_wrap(~speaker)

means <- my_vowels_raw %>%
  group_by(vowel)%>%
  summarise(mean_duration = mean(duration))%>%
  print(n = 11)
View(means)
means %>% 
  ggplot(aes(x = mean_duration, y = vowel, fill = vowel))+
  geom_density_ridges()
