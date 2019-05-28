library(tidyverse)
library(ggjoy)

theme_set(theme_bw())

my_vowels_raw <- read_csv("C:/study/afr/gin/kursach_/results/formants/form_r.csv")
my_vowels_raw %>%
  mutate(difference = F2 - F1,
         atr = factor(ifelse(str_detect(phoneme, "i|o|u|e|a|ə"), "+ATR", "-ATR")),
         grand_vowel = case_when(
           str_detect(phoneme, "i|ɩ") ~ "I",
           str_detect(phoneme, "o|ɔ") ~ "O",
           str_detect(phoneme, "e|ɛ") ~ "E",
           str_detect(phoneme, "u|ʋ") ~ "U",
           TRUE ~ "transparent")) ->
  my_vowels_raw

res_aov<- aov(difference ~ phoneme, data = my_vowels_raw)
summary(res_aov)
library(lme4)
library(lmerTest)

my_vowels_raw %>% 
  ggplot(aes(grand_vowel, F1, fill = atr))+
  geom_()+
  facet_wrap(~speaker)

fit_1 <- lmer(F1 ~ atr + grand_vowel + (1|speaker/grand_vowel), 
              data=my_vowels_raw)
summary(fit_1)
plot(fit_1)

my_vowels_raw %>% 
  ggplot(aes(grand_vowel, F2, fill = atr))+
  geom_boxplot()+
  facet_wrap(~speaker)


fit_2 <- lmer(F2 ~ atr + grand_vowel + (1|speaker), 
              data=my_vowels_raw[my_vowels_raw$grand_vowel != "transparent",])
summary(fit_2)
plot(fit_2)


fit_1 <- lmer(F1 ~ atr + (1|speaker/phoneme), 
              data=my_vowels_raw)
summary(fit_1)


my_vowels_raw %>% 
  filter(str_detect(phoneme, 'ɔ|o')) %>% 
  ggplot(aes(phoneme, F1, fill = factor(phoneme))) +
  geom_boxplot()+
  labs(x = "difference between F2 o and ɔ", y = "")+
  facet_wrap(~speaker)

my_vowels_raw %>% 
  filter(str_detect(phoneme, 'ɩ|i')) %>% 
  ggplot(aes(phoneme, F1, fill = factor(phoneme))) +
  geom_boxplot()+
  labs(x = "difference between F2 i and ɩ", y = "")+
  facet_wrap(~speaker)

my_vowels_raw %>% 
  filter(str_detect(phoneme, 'ɛ|e')) %>% 
  ggplot(aes(phoneme, F1, fill = factor(phoneme))) +
  geom_boxplot()+
  labs(x = "difference between F2 ɛ and e", y = "")+
  facet_wrap(~speaker)

my_vowels_raw %>% 
  filter(str_detect(phoneme, 'u|ʋ')) %>% 
  ggplot(aes(phoneme, F1, fill = factor(phoneme))) +
  geom_boxplot()+
  labs(x = "difference between F2 u and ʋ", y = "")+
  facet_wrap(~speaker)


ggplot(my_vowels_raw, aes(x = F2, y = F1, color = phoneme)) + 
  geom_point() + 
  stat_ellipse(level = 0.67) + 
  scale_x_reverse() + scale_y_reverse() 

means <- my_vowels_raw %>%
  group_by(phoneme, speaker)%>%
  summarise(mean_F1 = median(F1),
            mean_F2 = median(F2))%>%
  print(40)
ggplot(means, aes(x = mean_F2, y = mean_F1, label = phoneme))+
  geom_label()+
  scale_x_reverse()+ scale_y_reverse()

ggplot(my_vowels_raw, aes(x = F2, y = F1, color = phoneme, label = phoneme)) + 
  geom_text(size = 3) + 
  stat_ellipse(level = 0.67) +
  geom_text(data = means, aes(x = mean_F2, y = mean_F1), color = "black", size = 4)+
  scale_x_reverse() + scale_y_reverse() +
  scale_color_discrete(breaks = c('i', 'ɩ', 'ɔ', 'o', 'ɛ', 'e', 'ə', 'a', 'u', 'ʋ'))+
  guides(color = FALSE)+
  facet_wrap(~speaker)

my_vowels_raw %>% 
  group_by(phoneme) %>% 
  mutate(mn = mean(F2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(phoneme = reorder(phoneme, mn)) %>%
  ggplot(aes(x = phoneme, y = F2, fill = phoneme)) +
  geom_boxplot(alpha =0.4, show.legend = FALSE)+
  #geom_density_ridges() 
  theme(text = element_text(size = 20))+
  theme_bw()+
  facet_grid(~speaker, scales = "free_y")


my_vowels_raw %>% 
  group_by(phoneme) %>% 
  mutate(mn = mean(F2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(phoneme = reorder(phoneme, mn)) %>%
  ggplot(aes(x = phoneme, y = F2, fill = phoneme)) +
  geom_density(alpha =0.4, show.legend = FALSE)+
  #geom_density_ridges() 
  theme(text = element_text(size = 20))+
  theme_bw()+
  facet_grid(~speaker, scales = "free_y")
