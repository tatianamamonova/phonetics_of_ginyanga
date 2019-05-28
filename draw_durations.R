library(tidyverse)
my_vowels_raw <- read_csv("C:/study/afr/gin/kursach_/results/durations/dur_res.csv")
View(my_vowels_raw)



#считаем где сколько гласных
my_vowels_raw %>% 
  count(vowel, sort = TRUE) %>% 
  ggplot(aes(vowel, n, label = n, fill =vowel)) +
  geom_col()+
  geom_text(aes(y = n + 1))+
  labs(x = "", y = "")

#графики с разбивкой по номеру слога (абсолютные значения)
my_vowels_raw %>%
  filter(!str_detect(word, "v[12]"),
         str_detect(word, "")) %>% 
  ggplot(aes(vowel, duration, fill = vowel))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~syl_num, scales = "free_y")+
  theme(legend.position = "bottom")+
  labs(x = "", y = "duration (ms)")
#графики с назальными
my_vowels_raw %>%
  filter(!str_detect(word, "v[12]"),
         str_detect(word, "")) %>% 
  ggplot(aes(vowel, duration, fill = vowel))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~nasal, scales = "free_y")+
  theme(legend.position = "bottom")+
  labs(x = "", y = "duration (ms)")
#пытаемся в относительныю длительность, считаем отношение одного гласного к сумме гласных в слове
my_vowels_raw %>%
  group_by(word, syl_in_word) %>% 
  filter(syl_num == 1) %>% 
  filter(syl_in_word >= 2) %>% 
  mutate(rate = duration/(sum(duration))) %>% 
  ggplot(aes(vowel, rate, fill = vowel))+
  geom_boxplot()+
  geom_point()+
  theme(legend.position = "bottom")+
  labs(x = "", y = "relative 1 syl duration")
my_vowels_raw %>%
  group_by(word) %>% 
  filter(syl_num == 2) %>% 
  filter(syl_in_word >= 2) %>% 
  mutate(rate = duration/(sum(duration))) %>% 
  ggplot(aes(vowel, rate, fill = vowel))+
  geom_boxplot()+
  geom_point()+
  theme(legend.position = "bottom")+
  labs(x = "", y = "relative 2 syl duration")


# regression --------------------------------------------------------------

library(lme4)
library(lmerTest)

my_vowels_raw %>% 
  ggplot(aes(syl_in_word, duration, fill = vowel))+
  geom_point()+
  facet_grid(num_syl~vowel)

fit_1 <- lmer(duration ~ syl_in_word + num_syl + first + final + (1|speaker/vowel), 
              data=my_vowels_raw)
summary(fit_1)
plot(fit_1)

