library(tidyverse)

# Pulling Data from the Internet. Hosted version of files on Google Drive (to avoid spamming gov website with requests...)

build_df <- function(start_year, links) {
  df_ <- NULL
  for (i in seq_along(links)) {
    id <- links[i]
    # df <- read_csv(links[i], skip = 4)
    df <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), skip = 4)
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
    "168lSKfOhT9n1VDDgroIi9rfw-Twhi4r_",
    "166zoxw1cCPtWffh5-JL7kRbHgb_oG0xe",
    "16CMMBCyLuybc5l_Rcb-0wkY6J_8DIGwX",
    "16H5Db_1D3LDnmmBL0Z6hohk-WLwnL7M8",
    "16LFBnMAr7QhWPGjKEUFYAq8_SPTmBl60",
    "16NBQHLMb2_FeiTpZxnqEE8NNyva2TrZK",
    "16Te_uVusbWHvhxDdZWRXvQq9n2yWui89",
    "16WmaaGrOxUrouZnla4lCZiaenxrIcMHX"
  )
)

df <- df_IEP_raw %>%
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
  group_by(State) %>%
  mutate(
    Math_IEP_Total_Scaled = Math_IEP_Total / max(Math_IEP_Total),
    Reading_IEP_Total_Scaled = Reading_IEP_Total / max(Reading_IEP_Total),
  ) %>%
  ungroup() %>%
  select(order(names(.)))

STATES <- c(
  "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA",
  "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS",
  "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO",
  "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA",
  "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"
)



df %>%
  group_by(State) %>%
  mutate(Math_IEP_Total_Scaled = scale(Math_IEP_Total)) %>%
  ungroup() %>%
  # filter(State == "COLORADO") %>%
  filter(!is.na(State)) %>%
  filter(State %in% STATES) %>%
  mutate(Class = as.character(Class)) %>%
  group_by(Year, State) %>%
  summarize(IEP_Total = sum(Math_IEP_Total_Scaled)) %>%
  ggplot(
    mapping = aes(
      x = Year,
      y = IEP_Total
    )
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(State ~ .) +
  ylab("Scaled Average Number of Students in Math IEPs") +
  geom_vline(xintercept = 2013)

# STATES that had little to no effect (Not made known to parents enough)
# Nebraska, Missouri, Kentucky, Colorado

# States that had significant effect
# Deleware, California, North Dakota, Louisiana, Minnesta

STATES_TREATED <- c("DELEWARE", "CALIFORNIA", "NORTH DAKOTA", "LOUISIANA", "MINNESOTA")
STATES_NOT_TREATED <- c("NEBRASKA", "MISSOURI", "KENTUCKY", "COLORADO")


