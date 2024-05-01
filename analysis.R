library(tidyverse)

df_21_22 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/6ef97f41-26ab-4dc6-b638-c4fae2882f51/download/bassessment2021-22.csv", # nolint
  skip = 4
)

df_21_22 %>% head()


df_21_22 %>% nrow()

df_20_21 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/8a02388a-bf0f-48aa-9943-37517c3170c1/download/bassessment2020-21.csv", # nolint
  skip = 4
)

df_20_21 %>% head()

df_20_21 %>% nrow()

df_21_22 %>% head()

df_21_22 %>% colnames()
