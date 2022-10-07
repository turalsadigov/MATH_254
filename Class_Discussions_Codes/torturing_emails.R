library(tidyverse)
library(infer)
library(stats2data)

?emails
emails
View(emails)

# 1

# determine if the difference in the sample means of
# (a) the number of line breaks in the emails with no $ sign
# (b) the number of line breaks in the emails with exactly one $ sign
# is statistically significant.

emails %>% 
  filter(dollar == 0 | dollar == 1) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar)) %>% 
  t_test(line_breaks~dollar, alternative = 'two-sided')


# 2

# determine if the difference in the sample means of
# (a) the number of line breaks in the emails with no $ sign 
# and one attachment
# (b) the number of line breaks in the emails with exactly one $ sign
# and no attachment 
# is statistically significant.

emails %>% 
  filter((dollar == 0 & attach == 1 ) | 
          (dollar == 1 & attach == 0)) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar)) %>% 
  t_test(line_breaks~dollar, 
         alternative = 'two-sided', order = c('0', '1'))


# 3

# determine if the difference in the sample means of
# (a) the number of line breaks in the emails with no $ sign 
# and one attachment and where someone is cc-ed
# (b) the number of line breaks in the emails with exactly one $ sign
# and no attachment where no one is cc-ed
# is statistically significant.


emails %>% 
  filter((dollar == 0 & attach == 1 & cc == 1) | 
           (dollar == 1 & attach == 0 & cc == 0)) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar)) %>% 
  t_test(line_breaks~dollar, 
         alternative = 'two-sided', order = c('0', '1'))



#4 

# determine if the difference in the sample means of
# (a) the number of line breaks in the emails with no $ sign 
# and one attachment and where someone is cc-ed
# (b) the number of line breaks in the emails with exactly one $ sign
# and no IMAGE attached and  where no one is cc-ed
# is statistically significant.


emails %>% 
  filter((dollar == 0 & attach == 1 & cc == 1) | 
           (dollar == 1 & image == 0 & cc == 0)) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar)) %>% 
  t_test(line_breaks~dollar, 
         alternative = 'two-sided', order = c('0', '1'))


#5 

# determine if the difference in the sample means of
# (a) the number of line breaks in the emails with no $ sign 
# and one attachment and where someone is cc-ed
# (b) the number of line breaks in the emails with exactly one $ sign
# and no IMAGE attached and  where no one is cc-ed 
# AND where sender had been sent an email in the last 30 days
# is statistically significant.


emails %>% 
  filter((dollar == 0 & attach == 1 & cc == 1) | 
           (dollar == 1 & image == 0 & cc == 0 & sent_email == 1)) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar)) %>% 
  t_test(line_breaks~dollar, 
         alternative = 'two-sided', order = c('0', '1'))


#6 

# determine if the difference in the sample means of
# (a) the number of line breaks in the emails with no $ sign 
# and one attachment and where someone is cc-ed
# (b) the number of line breaks in the emails with exactly one $ sign
# and no IMAGE attached and 
# AND where sender had been sent an email in the last 30 days
# is statistically significant.

emails %>% 
  filter((dollar == 0 & attach == 1 & cc == 1) | 
         (dollar == 1 & image == 0 & sent_email == 1)) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar)) %>% 
  t_test(line_breaks~dollar, 
         alternative = 'two-sided', order = c('0', '1'))




# ==============

df <- emails %>% 
  filter((dollar == 0 & attach == 1 & cc == 1) | 
           (dollar == 1 & image == 0 & sent_email == 1)) %>% 
  select(line_breaks, dollar) %>% 
  mutate(dollar = as_factor(dollar))

df  %>% arrange(dollar) %>% print(n = 22)


# check normalities for each group

df %>% 
  filter(dollar == '0') %>% 
  ggplot(aes(sample = line_breaks)) +
  stat_qq() +
  stat_qq_line()


df %>% 
  filter(dollar == '1') %>% 
  ggplot(aes(sample = line_breaks)) +
  stat_qq() +
  stat_qq_line()


# test for normality

a <- df %>% 
  filter(dollar == '0') %>% 
  pull(line_breaks)
a

shapiro.test(a)

b <- df %>% 
  filter(dollar == '1') %>% 
  pull(line_breaks)
b

shapiro.test(b)

# ==== back to original p-hacking

# apply permutation test

obs_diff <- 
  df %>% 
  specify(line_breaks~dollar) %>% 
  calculate(stat = 'diff in means')
obs_diff


set.seed(2022)
null_dist <- 
  df %>% 
  specify(line_breaks~dollar) %>% 
  hypothesise(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'diff in means', order = c('0', '1'))

null_dist %>% 
  visualise() +
  shade_p_value(obs_stat = obs_diff, direction = 'both')

null_dist %>% 
  get_p_value(obs_stat = obs_diff, direction = 'both')








