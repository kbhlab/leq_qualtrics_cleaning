#-------------------------------------------------------------------------------------------Libraries
library(tidyverse)
library(janitor)
library(here)
library(tidylog)

#function for swapping elements in a string for later efficient renaming
str_swap <- function(string, str1, str2) {
  if (str_detect(string, str1, negate = TRUE) | str_detect(string, str2, negate = TRUE)) {
    stop("str1 and str2 (the two pieces to be swapped) must both appear in the full string")
  }
  str1 <- as.character(str1)
  str2 <- as.character(str2)
  str1pos <- str_locate(string, str1)
  str2pos <- str_locate(string, str2)
  
  if (str1pos[1] > str2pos[1]) {
    str1temp <- str1
    str1 <- str2
    str2 <- str1temp
    str1pos <- str_locate(string, str1)
    str2pos <- str_locate(string, str2)
  }
  string <- paste0(str_sub(string, 1L, str1pos[1]-1), str2, str_sub(string, str1pos[2]+1, str2pos[1]-1), str1, str_sub(string, str2pos[2]+1, str_length(string)))
  return(string)
}

#vectorize the function so it works across multiple inputs
str_swap <- Vectorize(str_swap)

#-------------------------------------------------------------------------------------------Data

#get column names because the Qualtrics output has some extra lines that interfere with reading in the data properly (first row is colnames, second is the question text from Qualtrics, third is some weird Qualtrics import metadata thing)
col_names <- names(read_csv(here(paste0(input_var, ".csv")), n_max = 0))

#create tibble with column types in case some are read in with all NAs, causing them to incorrectly be assigned logical
types <- tibble(col = col_names) %>%
  mutate(type = case_when(str_detect(col, "[D|d]ate") ~ "D",
                          str_detect(col, "seconds|baby_id|_age|_start$|hours|hrs|_end$|_count|_length|^num_|_est|_exp$") ~ "n",
                          TRUE ~ "c")) %>%
  pull(type) %>%
  paste0(., collapse = "") #read_csv can take a single string of shortcode column types

admin_cols <- c("response_id", "user_language", "participation_date", "researcher", "baby_id", "study_id", "study_name", "date_of_birth", "child_gender")

leq_data <- read_csv(here(paste0(input_var, ".csv")), col_names = col_names, col_types = types, skip = 3) %>%
  clean_names() %>%
  #get rid of all the extra columns not needed because Qualtrics
  select(-start_date, -end_date, -recorded_date, -status, -ip_address, -progress, -duration_in_seconds, -finished,-starts_with("recipient"), -external_reference, -location_latitude, -location_longitude, -distribution_channel, -baby_gender, -baby_gender2, -baby_gender3, -mono_exception_text, -today_date, -sleepwake_diff, -age_sit_start, -contains("default"), -cgvr1_lang3, -cgvr2_lang3, -cgvr1_lang4, -cgvr2_lang4, -highest_age, -confirm_age_4, -situation_count, -matches("sit\\d+_type.*_text"), -matches("l\\d_global_exp")) %>%
  #rename columns so they are more understandable and consistent (like using l1, l2 instead of sometimes lang1, lang2 and using weekday text instead of numbers):
  rename_with(~str_replace_all(., 
                               c("_hrs_" = "_hrs_l", 
                                 "(?<=_hrs_l\\d_)1" = "mon", 
                                 "(?<=_hrs_l\\d_)2" = "tue",
                                 "(?<=_hrs_l\\d_)3" = "wed",
                                 "(?<=_hrs_l\\d_)4" = "thu",
                                 "(?<=_hrs_l\\d_)5" = "fri",
                                 "(?<=_hrs_l\\d_)6" = "sat",
                                 "(?<=_hrs_l\\d_)7" = "sun",
                                 "(?<=_sit\\d{1,2}_)exp" = "hrsperweek",
                                 "lang(?=\\d)" = "l",
                                 "langs_(?=\\d)" = "l",
                                 "9_text" = "other",
                                 "sit1_ages_1" = "sit1_start",
                                 "ages_2" = "end",
                                 "ages_1" = "end",
                                 "est_1_" = "est_l",
                                 "_ac$" = "_acquired"))) %>%
  rename(cgvr1_other = cgvr1_3_text,
         cgvr2_other = cgvr2_3_text,
         daycare_yn = daycare,
         daycare_l3 = daycare_l3_text,
         daycare_l4 = daycare_l4_text,
         researcher_notes = final_notes,
         cgvr2_yn = second_cgiver_yn)
names(leq_data)


#-------------------------------------------------------------------------------------------Cleaning

#combine the caregiver "other" specifying columns into the regular caregiver columns
leq_data <- leq_data %>%
  mutate(cgvr1 = case_when(str_detect(cgvr1, "Other") & !is.na(cgvr1_other) ~ 
                             cgvr1_other,
                           str_detect(cgvr1, "Other") & is.na(cgvr1_other) ~ 
                             "Other (did not specify)",
                           TRUE ~ cgvr1),
         cgvr2 = case_when(str_detect(cgvr2, "Other") & !is.na(cgvr2_other) ~ 
                             cgvr2_other,
                           str_detect(cgvr2, "Other") & is.na(cgvr2_other) ~ 
                             "Other (did not specify)",
                           TRUE ~ cgvr2)) %>%
  select(-matches("cgvr\\d_other")) %>%
  #combine the "other" language column into the list of daycare languages
  mutate(daycare_langs = case_when(str_detect(daycare_langs, "Other 1") & !is.na(daycare_l3) ~ str_replace(daycare_langs, "Other 1 \\(please specify\\)", daycare_l3),
                                   str_detect(daycare_langs, "Other 1") & is.na(daycare_l3) ~ str_replace(daycare_langs, "Other 1 \\(please specify\\)", "Other 1 (did not specify)"),
                                   TRUE ~ daycare_langs),
         daycare_langs = case_when(str_detect(daycare_langs, "Other 2") & !is.na(daycare_l4) ~ str_replace(daycare_langs, "Other 2 \\(please specify\\)", daycare_l4),
                                   str_detect(daycare_langs, "Other 2") & is.na(daycare_l4) ~ str_replace(daycare_langs, "Other 2 \\(please specify\\)", "Other 2 (did not specify)"),
                                   TRUE ~ daycare_langs),
         daycare_langs = str_replace_all(daycare_langs, ",", ", ")) %>%
  select(-matches("daycare_l\\d"))

#clean the situations so that any "Other" ones use the description
clean_situations <- leq_data %>% 
  select(baby_id, study_name, study_id, response_id, matches("sit\\d+_type$"), matches("sit\\d+_type_specify")) %>%
  rename_with(stringr::str_replace, 
              pattern = "type_", replacement = "") %>%
  pivot_longer(sit1_type:sit10_specify, 
               names_to = c("variable", ".value"), 
               names_sep="_" ) %>%
  mutate(type = case_when(str_detect(type, "Other") ~ str_replace(type, "write explanation", specify),
                          TRUE ~ type)) %>%
  select(-specify) %>%
  pivot_wider(names_from = "variable", values_from = "type")

leq_data_wide <- leq_data %>%
  select(-matches("sit\\d+_type$"), -matches("sit\\d+_type_specify")) %>%
  left_join(clean_situations) %>%
  select(all_of(admin_cols), starts_with("cgvr1"), starts_with("cgvr2"), contains("cgvrs"), contains("daycare"), contains("trips"), starts_with("child_l"), wake_time, sleep_time, hours_nap, hrs_awake,
         sit1, matches("sit1(?!0)", perl = TRUE), 
         sit2, contains("sit2"), 
         sit3, contains("sit3"), 
         sit4, contains("sit4"),
         sit5, contains("sit5"),
         sit6, contains("sit6"),
         sit7, contains("sit7"),
         sit8, contains("sit8"),
         sit9, contains("sit9"),
         sit10, contains("sit10"),
         sit_count, starts_with("glob"), researcher_notes, mono_exception, contains("acquired"), baby_age, baby_fullage, child_langs, num_langs, contains("global"), contains("total"), contains("cumu"), contains("curr"), contains("overall"))

#----------------------------------------Check double entries for discrepancies
#first make everything character so can pivot longer, then identify multiple entries for same participant, and check what's different
if(check_discrepancies == TRUE) {

discrepancies_prep <- leq_data_wide %>%
  mutate(across(everything(), as.character)) %>%
  #remove auto-calculated columns that aren't something an RA enters
  select(-user_language, -researcher, -ends_with("hrsperweek"), -sit_count, -ends_with("age_acquired"), -mono_exception, -baby_age, 
         -baby_fullage, -num_langs, -ends_with("total_hrs"), -ends_with("cumu_exp"), -ends_with("overall_exp")) %>%
  #group by same study name and ID to find entries relating to same participation, filter to only those with 2 entries, then add a data entry #
  group_by(study_name, study_id) %>%
  filter(n() > 1) %>%
  arrange(response_id) %>%
  mutate(entry_num = row_number(),
         entry_num = case_when(entry_num == 1 ~ "first_entry",
                               entry_num == 2 ~ "second_entry")) 

id_list <- discrepancies_prep %>%
  distinct(response_id, study_id, entry_num) %>%
  mutate(entry_num = ifelse(str_detect(entry_num, "first"), "id_1", "id_2")) %>%
  pivot_wider(names_from = "entry_num", values_from = "response_id")
  
  
discrepancies <- discrepancies_prep %>%
  select(-response_id) %>%
  #pivot to get everything in one column for easier comparing
  pivot_longer(-c(study_id, study_name, entry_num), names_to = "variable", values_to = "response") %>% 
  pivot_wider(names_from = "entry_num", values_from = c("response")) %>% 
  mutate(discrepancy = case_when(first_entry != second_entry ~ "CHECK",
                            is.na(first_entry) & !is.na(second_entry) ~ "CHECK",
                            !is.na(first_entry) & is.na(second_entry) ~ "CHECK",
                            TRUE ~ ""))
  
discrepancies %>% 
  filter(discrepancy == "CHECK") %>% 
  left_join(id_list) %>%
  mutate(info_to_keep = "", comment = "") %>%
  write_csv(here(paste0(output_var, lubridate::today(), "_discrepancies_to_check.csv")))

}

#----------------------------------------Make succinct data without all the daily hours but totals only

leq_data_clean_long_detailed <- leq_data_wide %>%
  rename(num_sits = sit_count,
         l1.lang_name = child_l1,
         l2.lang_name = child_l2,
         l3.lang_name = child_l3,
         l4.lang_name = child_l4,
         l1.glob_est = glob_est_l1,
         l2.glob_est = glob_est_l2,
         l3.glob_est = glob_est_l3,
         l4.glob_est = glob_est_l4) %>%
  select(-ends_with("mon"), -ends_with("tue"), -ends_with("wed"), -ends_with("thu"), -ends_with("fri"), -ends_with("sat"), -ends_with("sun"), -contains("cgvr"), -contains("daycare"), -contains("trips")) %>%
  rename_with(~ str_swap(string = .x, 
                         str1 = str_extract(.x, "^l\\d_"), 
                         str2 = str_extract(.x, "sit\\d+_")), 
              matches("l\\d_sit\\d+")) %>%
  rename_with(~ str_replace(.x, "(?<=l\\d)_", ".")) %>% 
  pivot_longer(starts_with("sit"), 
               names_to = c('situation', '.value'),
               names_pattern = '(.*)_(.+)') %>% 
  filter(!is.na(situation)) %>% 
  group_by(response_id) %>%
  mutate(start = case_when(start == 0 ~ start,
                           TRUE ~ lag(end))) %>%
  ungroup() %>%
  filter(parse_number(situation) <= num_sits) %>% 
  pivot_longer(matches("^l\\d."),
               names_to = c('language', '.value'),
               names_pattern = '(.*)\\.(.+)')  %>% 
  filter(!is.na(lang_name)) %>% 
  #fix exposure variables, which in Qualtrics is only based on most recent situation (not all of them, but when data is long, this makes no sense)
  group_by(response_id, situation) %>%
  mutate(curr_exp = round(hrsperweek/sum(hrsperweek), 3),
         sit_hrs = sum(hrsperweek)) %>%
  group_by(response_id, language) %>%
  mutate(cumu_hrs = cumsum(hrsperweek)) %>% 
  group_by(response_id) %>%
  mutate(cumu_exp = round(cumu_hrs/(sit_hrs*parse_number(situation)), 3)) %>% 
  select(-sit_hrs) %>%
  group_by(response_id) %>%
  mutate(num_langs = n_distinct(lang_name),
         situation = parse_number(situation),
         glob_est = glob_est/100,
         overall_exp = overall_exp/100,
         lang_temp = case_when(language == "l3" | language == "l4" ~ "other",
                               TRUE ~ language)) %>%
  #flag kids who have multilingual situations where l3 + l4 > 10%
  group_by(response_id, situation, lang_temp) %>%
  mutate(multilingual = case_when(sum(cumu_exp) > .1 & lang_temp == "other" ~ 1,
                                  TRUE ~ 0)) %>%
  group_by(response_id, situation) %>%
  mutate(multilingual = sum(multilingual)) %>%
  select(-lang_temp) %>%
  group_by(response_id, language) %>%
  #flag kids who have situations where exposure changes more than 20% from previous
  mutate(large_change = case_when(lag(cumu_exp) > cumu_exp + .2 | lag(cumu_exp) < cumu_exp - .2 ~ 1,
                                  TRUE ~ 0)) %>%
  ungroup()

#----------------------------------------Make succinct summary data

leq_data_clean_wide_summary <- leq_data_clean_long_detailed %>%
  group_by(response_id) %>%
  filter(situation == max(situation)) %>%
  select(admin_cols, child_langs, num_sits, num_langs, researcher_notes, mono_exception, baby_age, baby_fullage, glob_est, overall_exp, curr_exp, cumu_exp, total_hrs, language, lang_name) %>%
  distinct() %>%
  mutate(language = case_when(str_detect(lang_name, "fran.ais|French") ~ "fre",
                              str_detect(lang_name, "anglais|English") ~ "eng",
                              TRUE ~ "other")) %>%
  group_by(response_id, language) %>%
  mutate(language = case_when(language == "other" ~ paste0(language, row_number()),
                              TRUE ~ language)) %>%
  select(-lang_name) %>%
  pivot_wider(names_from = "language", values_from = c("glob_est", "overall_exp", "curr_exp", "cumu_exp", "total_hrs")) %>%
  mutate(across(contains("exp"), round, 3))



#----------------------------------------Parent Language Strategies (OPOL, etc.)


#-------------------------------------------------------------------------------------------Save Data


#-----------------------------------------Full Data

if("wide_detailed" %in% data_type) {
  leq_data_wide %>% write_csv(here(paste0(output_var, lubridate::today(), "_wide_detailed_leq.csv")))
}

#-----------------------------------------Summary Data

if("wide_summary" %in% data_type) {
  leq_data_clean_wide_summary %>% write_csv(here(paste0(output_var, lubridate::today(), "_wide_summary_leq.csv")))
}

#-----------------------------------------Long Data

if("long" %in% data_type) {
  leq_data_clean_long_detailed %>% write_csv(here(paste0(output_var, lubridate::today(), "_long_leq.csv")))
}





