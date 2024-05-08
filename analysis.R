library(tidyverse)

# PULLING DATA FROM INTERNET

build_df <- function(start_year, links) {
  df_ <- NULL
  for (i in seq_along(links)) {
    df <- read_csv(links[i], skip = 4)
    names(df) <- gsub("\\r\\n|\\s+", "_", names(df))

    names(df) <- gsub("_{2,}", "_", names(df))
    names(df) <- gsub("%", "percent", names(df))
    names(df) <- gsub(",", "", names(df))

    df <- df %>% mutate(Year = start_year + i - 1)

    if (is.null(df_)) {
      df_ <- df
    } else {
      df_ <- dplyr::bind_rows(df_, df)
    }
  }
  return(df_)
}

df_IEP_raw <- build_df(
  2011,
  c(
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/45ba9bed-468d-4791-a61e-ab385debe0e6/download/bassessment2011-12.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/9796d0cb-4ff2-42ee-9040-b1342875fafc/download/bassessment2012-13.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/29043602-3a7b-4905-81db-d66848823d63/download/bassessment2013-14.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/0106be3f-ee5b-440b-be2f-0e741d47b03a/download/bassessment2014-15.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/099a90a2-306e-436f-9ca8-c9bf4d6047e8/download/bassessment2015-16.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/b40cff5f-f5a1-40c9-8441-51cee3aca1ff/download/bassessment2016-17.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/89b57bb6-2017-410b-8c92-379fb1686e02/download/bassessment2017-18.csv",
    "https://data.ed.gov/dataset/1deabc43-21f9-46a8-b93d-8fbbb6d89d48/resource/a04939b6-5fea-47c4-b6bd-aaa94ee79067/download/bassessment2018-19.csv"
  )
)

df_IEP <- df_IEP_raw %>%
  mutate(
    State = toupper(State),
    GradeLevel = as.numeric(Grade),
    Class = 12 - as.numeric(Grade) + Year,
    # Convert to numeric
    Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total = as.numeric(Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total),
    Math_IEP_Alternate_Assessment_Grade_Level_Std_Achievement_Total = as.numeric(Math_IEP_Alternate_Assessment_Grade_Level_Std_Achievement_Total),
    Math_IEP_Alternate_Assessment_Modified_Std_Achievement_Total = as.numeric(Math_IEP_Alternate_Assessment_Modified_Std_Achievement_Total),
    Math_IEP_No_Assessment_Absent = as.numeric(Math_IEP_No_Assessment_Absent),
    Math_IEP_No_Assessment_Other_Reasons = as.numeric(Math_IEP_No_Assessment_Other_Reasons),
    Math_IEP_Medical_Emergencies = as.numeric(Math_IEP_Medical_Emergencies),
    Math_IEP_Medical_Exemptions = as.numeric(Math_IEP_Medical_Exemptions),
    Math_IEP_Assessment_Score_Invalid = as.numeric(Math_IEP_Assessment_Score_Invalid),
    Math_IEP_No_Assessment_Parental_Exemptions = as.numeric(Math_IEP_No_Assessment_Parental_Exemptions),
    Math_IEP_Non_Participants = as.numeric(Math_IEP_Non_Participants),
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
    Reading_IEP_Medical_Exemptions = as.numeric(Reading_IEP_Medical_Exemptions),
    Reading_IEP_Non_Participants = as.numeric(Reading_IEP_Non_Participants),
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
    ProfandAbove_Math_Regular_Assessment_with_Accommodations_Grade_Level_Standards = as.numeric(coalesce(ProfandAbove_Math_Regular_Assessment_with_Accommodations_Grade_Level_Standards, ProfandAbove_Math_Regular_Assessment_with_Accomodations_Grade_Level_Standards)),
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
  ) %>%
  mutate(
    Math_IEP_Total = coalesce(Math_IEP_Regular_Assessment_with_Accommodations, 0) +
      coalesce(Math_IEP_Regular_Assessment_without_Accommodations, 0) +
      coalesce(Math_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total, 0) +
      coalesce(Math_IEP_Medical_Exemptions, 0) +
      coalesce(Math_IEP_Non_Participants, 0),
    Reading_IEP_Total = coalesce(Reading_IEP_Regular_Assessment_with_Accommodations, 0) +
      coalesce(Reading_IEP_Regular_Assessment_without_Accommodations, 0) +
      coalesce(Reading_IEP_Alternate_Assessment_Alternate_Std_Achievement_Total, 0) +
      coalesce(Reading_IEP_Medical_Exemptions, 0) +
      coalesce(Reading_IEP_Non_Participants, 0)
  ) %>%
  select(order(names(.)))

STATES <- c(
  "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA",
  "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS",
  "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO",
  "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA",
  "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"
)

df_IEP %>%
  # filter(State == "COLORADO") %>%
  filter(!is.na(State)) %>%
  filter(State %in% STATES) %>%
  mutate(Class = as.character(Class)) %>%
  group_by(Year, State) %>%
  summarize(IEP_Total = (sum(Math_IEP_Total) + sum(Reading_IEP_Total)) / 2) %>%
  ggplot(
    mapping = aes(
      x = Year,
      y = IEP_Total
    )
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(State ~ .) +
  ylab("Average Number of Students in Math and Reading IEPs")


# https://www2.ed.gov/about/inits/ed/edfacts/data-files/index.html
raw_ED_11 <- read_csv(
  "https://www2.ed.gov/about/inits/ed/edfacts/data-files/math-achievement-lea-sy2011-12.csv"
)

df_ED_Participation_raw <- read_csv(
  "https://www2.ed.gov/about/inits/ed/edfacts/data-files/math-participation-lea-sy2017-18.csv"
)


clean_participation_data <- function(df) {
  col_names <- colnames(df) %>% sort()
  cols_pct <- col_names[grep("ALL_MTH\\d{2}PCTPART_\\d{4}", col_names)]
  cols_cnt <- col_names[grep("ALL_MTH\\d{2}NUMPART_\\d{4}", col_names)]
  cols_end <- gsub("PART_\\d{4}$", "TOTALPART", cols_cnt)

  df[cols_pct] <- lapply(df[cols_pct], function(x) {
    x %>%
      str_replace_all("GE", "") %>%
      str_replace_all("LE", "") %>%
      str_replace_all("GT", "") %>%
      str_replace_all("LT", "") %>%
      str_replace_all("PS", "100") %>%
      str_replace_na("100")
  })

  df[cols_pct] <- lapply(df[cols_pct], function(x) {
    ifelse(
      grepl("-", x),  # Check for presence of a dash '-'
      as.numeric(sapply(strsplit(x, "-"), function(...) {
        return(mean(as.numeric(...)))
      })),  # Split, calculate average
      as.numeric(x)  # Otherwise convert to numeric
    )
  })

  for (i in seq_along(cols_pct)) {
    col_pct <- cols_pct[i]
    col_cnt <- cols_cnt[i]
    col_end <- cols_end[i]
    df[[col_end]] <- as.numeric(df[[col_cnt]]) / as.numeric(df[[col_pct]]) * 100
  }

  return(
    df %>%
      pivot_longer(
        cols = ends_with("TOTALPART"),
        names_to = 'GRADE',
        names_pattern = "ALL_MTH(\\d+)NUMTOTALPART"
      ) %>%
      mutate(value = ifelse(is.na(value), 0, value)) %>%
      group_by(STNAM, GRADE) %>%
      summarize(Participants = sum(value)) %>%
      mutate(State = STNAM, Grade = GRADE) %>%
      select(-STNAM, -GRADE)
  )
}

df <- df_ED_Participation_raw %>%
  clean_participation_data() %>%
  mutate(Year = 2011)