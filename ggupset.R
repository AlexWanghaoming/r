library(tidyverse)
library(ggupset)

# 1.
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(n_intersections = 20)
# 2.
tidy_movies$Genres
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(order_by = "degree") +
  theme_combmatrix(combmatrix.panel.point.color.fill = "green",
                   combmatrix.panel.line.size = 0,
                   combmatrix.label.make_space = FALSE)
# 3. 
df_complex_conditions %>%
  mutate(Label = pmap(list(KO, DrugA, Timepoint), function(KO, DrugA, Timepoint){
    c(if(KO) "KO" else "WT", if(DrugA == "Yes") "Drug", paste0(Timepoint, "h"))
  })) %>%
  ggplot(aes(x=Label, y=response)) +
  geom_boxplot() +
  geom_jitter(aes(color=KO), width=0.1) +
  geom_smooth(method = "lm", aes(group = paste0(KO, "-", DrugA))) +
  scale_x_upset(order_by = "degree",
                sets = c("KO", "WT", "Drug", "8h", "24h", "48h"),
                position="top", name = "") +
  theme_combmatrix(combmatrix.label.text = element_text(size=12),
                   combmatrix.label.extra_spacing = 5)

# 4.  
avg_rating <- tidy_movies %>%
  mutate(Genres_collapsed = sapply(Genres, function(x) paste0(sort(x), collapse="-"))) %>%
  mutate(Genres_collapsed = fct_lump(fct_infreq(as.factor(Genres_collapsed)), n=12)) %>%
  group_by(stars, Genres_collapsed) %>%
  summarize(percent_rating = sum(votes * percent_rating)) %>%
  group_by(Genres_collapsed) %>%
  mutate(percent_rating = percent_rating / sum(percent_rating)) %>%
  arrange(Genres_collapsed) %>%
  ggplot(avg_rating, aes(x=Genres_collapsed, y=stars, fill=percent_rating)) +
  geom_tile() +
  stat_summary_bin(aes(y=percent_rating * stars), fun.y = sum,  geom="point", 
                   shape="—", color="red", size=6) +
  axis_combmatrix(sep = "-", levels = c("Drama", "Comedy", "Short", 
                                        "Documentary", "Action", "Romance", "Animation", "Other")) +
  scale_fill_viridis_c()













