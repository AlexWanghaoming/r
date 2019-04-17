library(pacman)  # 词袋分词
p_load(tidyverse, tidytext)

corpus = c('The sky is blue and beautiful.',
           'Love this blue and beautiful sky!',
           'The quick brown fox jumps over the lazy dog.',
           'The brown fox is quick and the blue dog is lazy!',
           'The sky is very blue and the sky is very beautiful today',
           'The dog is lazy but the brown fox is quick!' )

labels = c('weather', 'weather', 'animals', 'animals', 'weather', 'animals')

tibble(Document = corpus,Category = labels) -> corpus_df
corpus_df %>%
  mutate(id = 1:n()) -> corpus_df

# 根据文本分组
corpus_df %>%
  group_by(id) %>%
  unnest_tokens(word,Document) %>%
  count(word,sort = T) %>%
  ungroup() -> bag_of_words_raw

# 利用内置停止词包去除停止词
bag_of_words_raw %>%
  anti_join(stop_words) %>%
  arrange(id) -> bag_of_words_tidy

# 生成 文档-词 矩阵
bag_of_words_tidy %>%
  spread(word,n,fill = 0) -> bag_of_words_dtm
bag_of_words_dtm$category <- corpus_df$Category  # add 2-classification

df <- as.data.frame(t(data.frame(c(1,rep(0,9)))))
colnames(df) <- colnames(bag_of_words_dtm)[2:11]
predict(model, df)

