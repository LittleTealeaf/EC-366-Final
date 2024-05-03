library(tidyverse)

# PULLING DATA FROM INTERNET

df_07_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/5a60f756-414a-4abe-beaa-3f536f84a102/download/bassessment2007-08.csv",
  skip = 4
)

df_08_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/e8021bfd-f9e5-4e93-a648-9312dd57f086/download/bassessment2008-09.csv",
  skip = 3
)

df_09_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/c45db2fb-f75e-4837-8ab2-9732b107ef9e/download/bassessment2009-10.csv",
  skip = 3
)

df_10_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/425ca377-dfa5-421c-979d-3e2d9985eae5/download/bassessment2010-11.csv",
  skip = 4
)

df_11_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/45ba9bed-468d-4791-a61e-ab385debe0e6/download/bassessment2011-12.csv",
  skip = 4
)

df_12_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/9796d0cb-4ff2-42ee-9040-b1342875fafc/download/bassessment2012-13.csv",
  skip = 4
)

df_13_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/29043602-3a7b-4905-81db-d66848823d63/download/bassessment2013-14.csv",
  skip = 4
)

df_14_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/0106be3f-ee5b-440b-be2f-0e741d47b03a/download/bassessment2014-15.csv",
  skip = 4
)

df_15_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/099a90a2-306e-436f-9ca8-c9bf4d6047e8/download/bassessment2015-16.csv",
  skip = 4
)

df_16_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/b40cff5f-f5a1-40c9-8441-51cee3aca1ff/download/bassessment2016-17.csv",
  skip = 4
)

df_17_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/89b57bb6-2017-410b-8c92-379fb1686e02/download/bassessment2017-18.csv",
  skip = 4
)

df_18_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/a04939b6-5fea-47c4-b6bd-aaa94ee79067/download/bassessment2018-19.csv",
  skip = 4
)

df_20_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/8a02388a-bf0f-48aa-9943-37517c3170c1/download/bassessment2020-21.csv", # nolint
  skip = 4
)

df_21_raw <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/6ef97f41-26ab-4dc6-b638-c4fae2882f51/download/bassessment2021-22.csv", # nolint
  skip = 4
)

# MUTATING TO GRAB REQUIRED DATA

HS_ID <- 9

sel <- function(data) {
  library(tidyverse)
  return(
    dplyr::select(data, Year, Grade, Math_IEP_Enroll, Math_Enroll, Eng_Enroll, Eng_IEP_Enroll, State)
  )
}

df_07 <- df_07_raw  %>% mutate(
  Grade = ifelse(GRADE == "HS", HS_ID, GRADE),
  Math_IEP_Enroll = MATH_ENR_IEP,
  Math_Enroll = MATH_ENR_ALLSTU,
  Eng_IEP_Enroll = READ_ENR_IEP,
  Eng_Enroll = READ_ENR_ALLSTU
) %>% sel()

df_08 <- df_08_raw %>% mutate(
  Grade = ifelse(GRADE == "HS", HS_ID, GRADE),
  Math_IEP_Enroll = `math enrollment, all students`,
  Math_Enroll = `math enrollment, all students`,
  Eng_IEP_Enroll = `reading enrollment, iep`,
  Eng_Enroll = `reading enrollment, all students`,
) %>% sel()


df_09 <- df_09_raw %>%
  mutate(
    Year = as.numeric(Year),
    Grade = ifelse(`Grade \r\nLevel` == "H", HS_ID, `Grade \r\nLevel`),
    Math_IEP_Enroll = as.numeric(gsub(",","",`Math \r\nEnrollment\r\nIEP`)),
    Math_Enroll = as.numeric(gsub(",","",`Math \r\nEnrollment\r\nAll \r\nStudents`)),
    Eng_IEP_Enroll = as.numeric(gsub(",","",`Reading Enrollment\r\nIEP`)),
    Eng_Enroll = as.numeric(gsub(",","",`Reading \r\nEnrollment\r\nAll \r\nStudents`)),
  ) %>% sel()

df_10 <- df_10_raw %>%
  mutate(
    Year = as.numeric(Year),
    Grade = ifelse(`Grade \r\nLevel` == "H", HS_ID, `Grade \r\nLevel`),
    Math_IEP_Enroll = as.numeric(gsub(",","",`Math \r\nEnrollment IEP`)),
    Math_Enroll = as.numeric(gsub(",","",`Math \r\nEnrollment\r\nAll \r\nStudents`)),
    Eng_IEP_Enroll = as.numeric(gsub(",","",`Reading Enrollment\r\nIEP`)),
    Eng_Enroll = as.numeric(gsub(",","",`Reading \r\nEnrollment\r\nAll \r\nStudents`)),
  ) %>% sel()

df_07_10 <- bind_rows(df_07,df_08,df_09, df_10) %>%
  mutate(State = toupper(State))

# Looks like everything 2011 and up DONT use these fields?!