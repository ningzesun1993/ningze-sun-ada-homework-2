install.packages("ggpubr")
getwd()

library(tidyverse)
library(ggplot2)
library(dplyr)

library(mosaic)
library(reshape2)
library(ggpubr)
theme_set(theme_pubr())


list_to_df = function(missing, columns_name){
  df = data.frame(matrix(unlist(missing), nrow = length(missing), byrow = TRUE))
  names(df) = columns_name
  return(df)
}


cal_ci = function(means, std, n){
  error = qnorm(0.975) * std / sqrt(n)
  return(c(means - error, means + error))
}


cal_de = function(df_total, df_s, i){
  means = df_total[df_total$decade == i,]$means[1]
  test = df_s[df_s$decade == i,]$runtimeMinutes
  n = length(test)
  de = sqrt(sum((test - means)^2) / n) / sqrt(n)
  return(de)
}



# hw1
df = read.csv('./IMDB-movies.csv', sep = ',', stringsAsFactors = FALSE)
df = as_tibble(df)
df_filter = df %>% filter(runtimeMinutes <= 180 & runtimeMinutes >= 60 & startYear <= 1979 & startYear >= 1920) %>% 
  mutate(decade = paste0(as.character(floor(startYear%%100/10)*10), 's'))
df_filter
ggplot(data = df_filter, aes(x = runtimeMinutes)) + geom_histogram(binwidth = 10, color="black", fill="white") + 
  labs(title = "histograms of runtimeMinutes for each decade", y = "runtimeMinutes", x = "") + facet_wrap(~ decade)
df_total = df_filter %>% group_by(decade) %>% summarize(means = mean(runtimeMinutes), std = sd(runtimeMinutes))
df_total
df_s = df_filter %>% group_by(decade) %>% sample_n(100) 
df_g = df_s %>% group_by(decade) %>% summarize(means = mean(runtimeMinutes), std = sd(runtimeMinutes))
de_result = list()
uniq = distinct(as.data.frame(df_s$decade), .keep_all=TRUE)[[1]]
for (i in 1:length(uniq)){
  de_result[[i]] = c(uniq[[i]],cal_de(df_total, df_s, uniq[[i]]))
}
df_de = list_to_df(de_result, c("decade", 'de'))
df_de[['de']] = as.numeric(df_de[['de']])
df_de
df_a = df_total
df_a[['means']] = df_total[['means']] - df_g[['means']]
df_a[['std']] = df_total[['std']] - df_de[['de']]
df_a
samples = df_filter %>% group_by(decade) %>% sample_n(100)
samples = samples[c('runtimeMinutes', 'decade')]
# samples_group = samples  %>% group_by(decade) %>% summarize(means = mean(runtimeMinutes), std = sd(runtimeMinutes))
samples[['num']] = 1
for (i in 2:1000){
  df_t = df_filter %>% group_by(decade) %>% sample_n(100)
  df_t = df_t[c('runtimeMinutes', 'decade')]
  df_t[['num']] = i
  samples = bind_rows(samples, df_t)
}
names(samples)
df_sample = samples %>% group_by(decade, num) %>% summarize(mean_s = mean(runtimeMinutes), std = sd(runtimeMinutes))
df_sample
ggplot(data = df_sample, aes(x = mean_s)) + geom_histogram(binwidth = 2, color="black", fill="white") + 
  labs(title = "histograms of means of sample for each decade", y = "runtimeMinutes", x = "") + facet_wrap(~ decade)

de_result = list()
for (i in 1:length(uniq)){
  de_result[[i]] = c(uniq[[i]], cal_de(df_total, samples, uniq[[i]]))
}

test = sample[df_s$decade == '20s',]$runtimeMinutes


df_d = list_to_df(de_result, c("decade", 'de'))
df_d[['de']] = as.numeric(df_d[['de']])
df_d
df_a = df_total
df_g = df_sample %>% group_by(decade) %>% summarize(means = mean(mean_s), std = sd(mean_s))
df_a[['means']] = df_g[['means']] - df_total[['means']]
df_a[['std']] = df_d[['de']] / df_total[['std']]
names(df_a) = c('decade', 'diff of mean', 'ratio of se')
df_a[['ratio of 100 samples']] = df_d[['de']] / df_de[['de']]
df_a


#hw2
p_1 = ppois(9, 12)
p_1
p_2 = dpois(0, 12)
p_2
p_3 = dpois(5, 12)
p_3
p_4 = 1 - ppois(18, 12)
p_4
data.frame(list(x = 0:24, prob = dpois(0:24, 12)))
ggplot(data = data.frame(list(x = 0:24, prob = dpois(0:24, 12))), aes(x, prob)) + 
  geom_bar(stat="identity", fill="steelblue") + labs(x = "x", y = "prob")
p_5 = data.frame(num_of_foragers = rpois(1460, 12))
histogram(~num_of_foragers, data = p_5, xlim = c(0, 24))

#hw3
df_3 = as_tibble(read.csv("zombies.csv", sep = ',', stringsAsFactors = FALSE))
var = c("height", "weight", "zombies_killed", "years_of_education", "age")
df_q = data.frame(list(mean = sapply(df_3[var], mean), std = sapply(df_3[var], sd)))
var = c("height", "weight", "zombies_killed", "years_of_education", "age", "gender")
df_m = melt(df_3[var], id.var = "gender")
ggplot(df_m, aes(x = variable, y = value)) + geom_boxplot(aes(fill = gender)) + facet_wrap(~variable, scales = "free")
h = ggplot(df_3, aes(x = age, y = height)) + geom_point(size = 2, color = 'blue')+ 
  facet_wrap(~gender, scales = "free")
w = ggplot(df_3, aes(x = age, y = weight)) + geom_point(size = 2, color = 'red')+ 
  facet_wrap(~gender, scales = "free")
ggarrange(h, w, labels = c("A", "B"), ncol = 1, nrow = 2)
df_f = df_3 %>% filter(gender == "Female")
df_male = df_3 %>% filter(gender == "Male") 
cor.test(df_f$age, df_f$height)
cor.test(df_male$age, df_male$height)
ggplot(df_m, aes(value)) + 
  geom_histogram(binwidth = 2, color="black", fill="blue") +  
  facet_wrap(~variable, scales = "free")

ggplot(df_m, aes(sample = value)) + stat_qq() + stat_qq_line() + 
  facet_wrap(~variable, scales = "free")
var = c("height", "weight", "zombies_killed", "years_of_education", "age")
for (i in var){
  print(i)
  print(shapiro.test(df_3[[i]]))
}

df_sample = df_3 %>% sample_n(50)
t_result = list()
for (i in 1:length(var)){
  print(var[[i]])
  t = df_sample %>%summarize(means = mean(!!as.symbol(var[[i]])), std = sd(!!as.symbol(var[[i]])))
  t_result[[i]] = c(var[[i]], t$means, t$std/sqrt(50))
  print(cal_ci(t$means, t$std, 50))
}
df_t_res = list_to_df(t_result, c("variable", "mean", "std"))
df_t_res[['mean']] = as.numeric(df_t_res[['mean']])
df_t_res[['std']] = as.numeric(df_t_res[['std']])
df_t_res
df_sample[['sample']] = 1
for (i in 2:10){
  df_t = df_3 %>% sample_n(50)
  df_t[['sample']] = i
  df_sample = bind_rows(df_sample, df_t)
}
df_gr = df_sample %>% group_by(sample) %>% summarize(height = mean(height), 
         weight = mean(weight), zombies_killed = mean(zombies_killed),  
          years_of_education = mean(years_of_education), age = mean(age))
df_gm = df_gr %>% summarize(height = mean(height), weight = mean(weight),
                            zombies_killed = mean(zombies_killed), 
                           years_of_education = mean(years_of_education),
                           age = mean(age))
df_gs = df_gr %>% summarize(height = sd(height),
                            weight = sd(weight),
                            zombies_killed = sd(zombies_killed),
                            years_of_education = sd(years_of_education),
                            age = sd(age))
df_tr = bind_rows(df_gm, df_gs)
df_tr = as_tibble(t(df_tr), rownames = "name")
names(df_tr) = c("col_name", "mean", "std")
df_tr
for (i in 1:5){
  print(df_tr[['col_name']][[i]])
  print(cal_ci(df_tr[['mean']][[i]], df_tr[['std']][[i]], 1))
}
df_tt = df_tr
df_tt$mean = df_tr$mean - df_t_res$mean
df_tt$vars = df_tr$std - df_t_res$std
df_tt




df_m = melt(df_gr, id.var = "sample")
ggplot(df_m, aes(value)) + geom_histogram(binwidth = 2, color="black", fill="white") + 
  facet_wrap(~variable, scales = "free")
