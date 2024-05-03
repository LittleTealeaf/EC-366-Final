library(tidyverse)

# PULLING DATA FROM INTERNET


cleanup_variable_names <- function(df) {
  names(df) <- gsub("\\r\\n|\\s+", "_", names(df))

  names(df) <- gsub("_{2,}", "_", names(df))
  names(df) <- gsub("%","percent",names(df))
  names(df) <- gsub(",","",names(df))


  return(df)
}

raw_07 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/5a60f756-414a-4abe-beaa-3f536f84a102/download/bassessment2007-08.csv",
  skip = 4
) %>% cleanup_variable_names()


raw_08 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/e8021bfd-f9e5-4e93-a648-9312dd57f086/download/bassessment2008-09.csv",
  skip = 3
) %>% cleanup_variable_names()


raw_09 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/c45db2fb-f75e-4837-8ab2-9732b107ef9e/download/bassessment2009-10.csv",
  skip = 3
) %>% cleanup_variable_names()


raw_10 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/425ca377-dfa5-421c-979d-3e2d9985eae5/download/bassessment2010-11.csv",
  skip = 4
) %>% cleanup_variable_names()


raw_11 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/45ba9bed-468d-4791-a61e-ab385debe0e6/download/bassessment2011-12.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_12 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/9796d0cb-4ff2-42ee-9040-b1342875fafc/download/bassessment2012-13.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_13 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/29043602-3a7b-4905-81db-d66848823d63/download/bassessment2013-14.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_14 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/0106be3f-ee5b-440b-be2f-0e741d47b03a/download/bassessment2014-15.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_15 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/099a90a2-306e-436f-9ca8-c9bf4d6047e8/download/bassessment2015-16.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_16 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/b40cff5f-f5a1-40c9-8441-51cee3aca1ff/download/bassessment2016-17.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_17 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/89b57bb6-2017-410b-8c92-379fb1686e02/download/bassessment2017-18.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_18 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/a04939b6-5fea-47c4-b6bd-aaa94ee79067/download/bassessment2018-19.csv",
  skip = 4
) %>% cleanup_variable_names()

raw_20 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/8a02388a-bf0f-48aa-9943-37517c3170c1/download/bassessment2020-21.csv", # nolint
  skip = 4
) %>% cleanup_variable_names()

raw_21 <- read_csv(
  "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/6ef97f41-26ab-4dc6-b638-c4fae2882f51/download/bassessment2021-22.csv", # nolint
  skip = 4
) %>% cleanup_variable_names()

############################################################
# # MUTATING TO GRAB REQUIRED DATA
#
# HS_ID <- "H"
#
# df_07 <- df_07_raw %>%
#   mutate(
#     Grade = ifelse(GRADE == "HS", HS_ID, GRADE),
#     Math_IEP_Enroll = MATH_ENR_IEP,
#     Math_Enroll = MATH_ENR_ALLSTU,
#     Eng_IEP_Enroll = READ_ENR_IEP,
#     Eng_Enroll = READ_ENR_ALLSTU
#   ) %>%
#   select(Year, Grade, Math_IEP_Enroll, Math_Enroll, Eng_Enroll, Eng_IEP_Enroll, State)
#
# df_08 <- df_08_raw %>%
#   mutate(
#     State = State,
#     Grade = ifelse(GRADE == "HS", HS_ID, GRADE),
#     Math_IEP_Enroll = `math enrollment, all students`,
#     Math_Enroll = `math enrollment, all students`,
#     Eng_IEP_Enroll = `reading enrollment, iep`,
#     Eng_Enroll = `reading enrollment, all students`,
#   ) %>%
#   select(Year, Grade, Math_IEP_Enroll, Math_Enroll, Eng_Enroll, Eng_IEP_Enroll, State)
#
#
# df_09 <- df_09_raw %>%
#   mutate(
#     State = State,
#     Year = as.numeric(Year),
#     Grade = ifelse(`Grade \r\nLevel` == "H", HS_ID, `Grade \r\nLevel`),
#     Math_IEP_Enroll = as.numeric(gsub(",", "", `Math \r\nEnrollment\r\nIEP`)),
#     Math_Enroll = as.numeric(gsub(",", "", `Math \r\nEnrollment\r\nAll \r\nStudents`)),
#     Eng_IEP_Enroll = as.numeric(gsub(",", "", `Reading Enrollment\r\nIEP`)),
#     Eng_Enroll = as.numeric(gsub(",", "", `Reading \r\nEnrollment\r\nAll \r\nStudents`)),
#   ) %>%
#   select(Year, Grade, Math_IEP_Enroll, Math_Enroll, Eng_Enroll, Eng_IEP_Enroll, State)
#
# df_10 <- df_10_raw %>%
#   mutate(
#     State = State,
#     Year = as.numeric(Year),
#     Grade = ifelse(`Grade \r\nLevel` == "H", HS_ID, `Grade \r\nLevel`),
#     Math_IEP_Enroll = as.numeric(gsub(",", "", `Math \r\nEnrollment IEP`)),
#     Math_Enroll = as.numeric(gsub(",", "", `Math \r\nEnrollment\r\nAll \r\nStudents`)),
#     Eng_IEP_Enroll = as.numeric(gsub(",", "", `Reading Enrollment\r\nIEP`)),
#     Eng_Enroll = as.numeric(gsub(",", "", `Reading \r\nEnrollment\r\nAll \r\nStudents`)),
#   ) %>%
#   select(Year, Grade, Math_IEP_Enroll, Math_Enroll, Eng_Enroll, Eng_IEP_Enroll, State)
#
# df_07_10 <- bind_rows(df_07, df_08, df_09, df_10) %>%
#   mutate(State = toupper(State))
#
# # Looks like everything 2011 and up DONT use these fields?!
# # Let's try and combine everything from 2011 up with new fields
#
# df_11 <- df_11_raw %>%
#   mutate(
#     Year = 2011,
#     State = toupper(State),
#     Grade = ifelse(Grade == "HS", HS_ID, Grade),
#     Math_IEP_Regular_Assessment_Accomodations = as.numeric(`Math IEP \r\nRegular Assessment with Accomodations`),
#     Math_IEP_Regular_Assessment_No_Accomodations = as.numeric()
#   )
#
#
#
# df_11_raw %>% clean_col_names()
############################################################


# Focusing on 2011-2018 (range we want to see)

df <- bind_rows(
  raw_11 %>% mutate(Year = 2011),
  raw_12 %>% mutate(Year = 2012),
  raw_13 %>% mutate(Year = 2013),
  raw_14 %>% mutate(Year = 2014),
  raw_15 %>% mutate(Year = 2015),
  raw_16 %>% mutate(Year = 2016),
  raw_17 %>% mutate(Year = 2017),
  raw_18 %>% mutate(Year = 2018)
) %>%
  mutate(
    # Convert to numeric
    Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total = as.numeric(Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total),
    Math_IEP_Alternate_Assessment_Grade_Level_Std_Achievement_Total = as.numeric(Math_IEP_Alternate_Assessment_Grade_Level_Std_Achievement_Total),
    Math_IEP_Alternate_Assessment_Modified_Std_Achievement_Total = as.numeric(Math_IEP_Alternate_Assessment_Modified_Std_Achievement_Total),
    Math_IEP_No_Assessment_Absent = as.numeric(Math_IEP_No_Assessment_Absent),
    Math_IEP_No_Assessment_Other_Reasons = as.numeric(Math_IEP_No_Assessment_Other_Reasons),
    Math_IEP_Medical_Emergencies = as.numeric(Math_IEP_Medical_Emergencies),
    Math_IEP_Assessment_Score_Invalid = as.numeric(Math_IEP_Assessment_Score_Invalid),
    Math_IEP_No_Assessment_Parental_Exemptions = as.numeric(Math_IEP_No_Assessment_Parental_Exemptions),
    Math_IEP_out_of_Level_Assessment_Total = as.numeric(Math_IEP_out_of_Level_Assessment_Total),
    ProfandAbove_Math_Alternate_Assessment_Alternate_Standards = as.numeric(ProfandAbove_Math_Alternate_Assessment_Alternate_Standards),
    ProfandAbove_Math_Alternate_Assessment_Grade_Level_Standards = as.numeric(ProfandAbove_Math_Alternate_Assessment_Grade_Level_Standards),
    ProfandAbove_Math_Alternate_Assessment_Modified_Standards = as.numeric(ProfandAbove_Math_Alternate_Assessment_Modified_Standards),
    Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_ESEA_1percent_CAP = as.numeric(Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_ESEA_1percent_CAP),
    Math_IEP_Alternate_Assessment_Modified_Std_Achievement_ESEA_2percent_CAP = as.numeric(Math_IEP_Alternate_Assessment_Modified_Std_Achievement_ESEA_2percent_CAP),
    Reading_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total = as.numeric(Reading_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total),
    Reading_IEP_Alternate_Assessment_Grade_Level_Std_Achievement_Total = as.numeric(Reading_IEP_Alternate_Assessment_Grade_Level_Std_Achievement_Total),
    Reading_IEP_Alternate_Assessment_Modified_Std_Achievement_Total = as.numeric(Reading_IEP_Alternate_Assessment_Modified_Std_Achievement_Total),
    Reading_IEP_LEP_students_Using_LEP_Replacing_Reading_Assessment = as.numeric(Reading_IEP_LEP_students_Using_LEP_Replacing_Reading_Assessment),
    Reading_IEP_No_Assessment_Absent = as.numeric(Reading_IEP_No_Assessment_Absent),
    Reading_IEP_No_Assessment_Other_Reasons = as.numeric(Reading_IEP_No_Assessment_Other_Reasons),
    Reading_IEP_Medical_Emergencies = as.numeric(Reading_IEP_Medical_Emergencies),
    Reading_IEP_Assessment_Score_Invalid = as.numeric(Reading_IEP_Assessment_Score_Invalid),
    Reading_IEP_No_Assessment_Parental_Exemptions = as.numeric(Reading_IEP_No_Assessment_Parental_Exemptions),
    Reading_IEP_out_of_Level_Assessment_Total = as.numeric(Reading_IEP_out_of_Level_Assessment_Total),
    ProfandAbove_Reading_Alternate_Assessment_Alternate_Standards = as.numeric(ProfandAbove_Reading_Alternate_Assessment_Alternate_Standards),
    ProfandAbove_Reading_Alternate_Assessment_Grade_Level_Standards = as.numeric(ProfandAbove_Reading_Alternate_Assessment_Grade_Level_Standards),
    ProfandAbove_Reading_Alternate_Assessment_Modified_Standards = as.numeric(ProfandAbove_Reading_Alternate_Assessment_Modified_Standards),
    Reading_IEP_Alternate_Assessment_Alternate_Std_Achievement_ESEA_1percent_CAP = as.numeric(Reading_IEP_Alternate_Assessment_Alternate_Std_Achievement_ESEA_1percent_CAP),
    Reading_IEP_Alternate_Assessment_Modified_Std_Achievement_ESEA_2percent_CAP = as.numeric(Reading_IEP_Alternate_Assessment_Modified_Std_Achievement_ESEA_2percent_CAP),



    # Fix Renames
    Math_IEP_Regular_Assessment_with_Accommodations = as.numeric(coalesce(Math_IEP_Regular_Assessment_with_Accommodations, Math_IEP_Regular_Assessment_with_Accomodations)),
    Math_IEP_Regular_Assessment_without_Accommodations = as.numeric(coalesce(Math_IEP_Regular_Assessment_without_Accommodations, Math_IEP_Regular_Assessment_without_Accomodations)),
    ProfandAbove_Math_Regular_Assessment_with_Accommodations_Grade_Level_Standards = as.numeric(coalesce(ProfandAbove_Math_Regular_Assessment_with_Accommodations_Grade_Level_Standards,ProfandAbove_Math_Regular_Assessment_with_Accomodations_Grade_Level_Standards )),
    ProfandAbove_Math_Regular_Assessment_without_Accommodations_Grade_Level_Standards = as.numeric(coalesce(ProfandAbove_Math_Regular_Assessment_without_Accommodations_Grade_Level_Standards, ProfandAbove_Math_Regular_Assessment_without_Accomodations_Grade_Level_Standards)),
    ProfandAbove_Reading_Regular_Assessment_with_Accommodations_Grade_Level_Standards = as.numeric(coalesce(ProfandAbove_Reading_Regular_Assessment_with_Accommodations_Grade_Level_Standards, ProfandAbove_Reading_Regular_Assessment_with_Accomodations_Grade_Level_Standards)),
    ProfandAbove_Reading_Regular_Assessment_without_Accommodations_Grade_Level_Standards = as.numeric(coalesce(ProfandAbove_Reading_Regular_Assessment_without_Accommodations_Grade_Level_Standards, ProfandAbove_Reading_Regular_Assessment_without_Accomodations_Grade_Level_Standards)),
    Reading_IEP_Regular_Assessment_with_Accommodations = as.numeric(coalesce(Reading_IEP_Regular_Assessment_with_Accommodations, Reading_IEP_Regular_Assessment_with_Accomodations)),
    Reading_IEP_Regular_Assessment_without_Accommodations = as.numeric(coalesce(Reading_IEP_Regular_Assessment_without_Accommodations, Reading_IEP_Regular_Assessment_without_Accomodations))
  ) %>%
  select(
    -Math_IEP_Regular_Assessment_with_Accomodations,
    -Math_IEP_Regular_Assessment_without_Accomodations,
    -ProfandAbove_Math_Regular_Assessment_with_Accomodations_Grade_Level_Standards,
    -ProfandAbove_Math_Regular_Assessment_without_Accomodations_Grade_Level_Standards,
    -ProfandAbove_Reading_Regular_Assessment_with_Accomodations_Grade_Level_Standards,
    -ProfandAbove_Reading_Regular_Assessment_without_Accomodations_Grade_Level_Standards,
    -Reading_IEP_Regular_Assessment_with_Accomodations,
    -Reading_IEP_Regular_Assessment_without_Accomodations
  )

ggplot(
  data = df %>% select(Year, count = Math_IEP_Regular_Assessment_with_Accommodations) %>% drop_na() %>% group_by(Year) %>% summarize(Math_IEP_Regular_Assessment_with_Accommodations = sum(count)),
  mapping = aes(x = Year, y = Math_IEP_Regular_Assessment_with_Accommodations)
) + geom_point() + geom_smooth()