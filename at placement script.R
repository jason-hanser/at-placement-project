
## Loading libraries

library(dplyr)
library(tidyr)
library(stringr)
library(clue)



##############################
######## LOADING DATA ########
##############################

## Loading Data Files

AT_COURSES    <- read.csv("Data//at_courses.csv", stringsAsFactors = FALSE)
AT_SELECTIONS <- read.csv("Data//at_selections.csv", stringsAsFactors = FALSE)


## Loading supporting data files (i.e. related disciplines)

RELATED_DISP_WTS <- read.csv("Data/Supporting Data/related_discipline_weights.csv", stringsAsFactors = FALSE)  




#######################################
######## INITIAL CLEANING DATA ########
#######################################

## Creating SEATS data frame

AT_COURSES %>%
  slice(rep(x    = 1:n(), 
            each = ceiling(length(unique(AT_SELECTIONS$STUDENT_ID))/length(unique(AT_COURSES$SECTION_ID))))) %>%
  group_by(SECTION_ID) %>%
  mutate(SEAT_NUMBER = row_number()) %>%
  ungroup() %>%
  select(SECTION_ID,
         SEAT_NUMBER) -> AT_SEATS


## Reserving 30% of seats for male students to preserve gender balance across sections

AT_SEATS %>%
  group_by(SECTION_ID) %>%
  mutate(SEAT_SEX = case_when(row_number() <= max(row_number())*0.40 ~ "FEMALE_ONLY",
                              row_number() <= max(row_number())*0.65 ~ "MALE_ONLY",
                              TRUE ~ "ANY")) %>%
  ungroup() -> AT_SEATS

## Reserving ~40% of seats for non-majors

AT_SEATS %>%
  group_by(SECTION_ID,
           SEAT_SEX) %>%
  mutate(SEAT_MAJOR = case_when(row_number() <= ceiling(max(row_number())*0.4) ~ "NON_MAJOR_ONLY", 
                                TRUE ~ "ANY")) %>%
  ungroup() -> AT_SEATS


## Assigning weights to discipline and joining SEATS onto AT_COURSES

AT_COURSES %>%
  mutate(DISCIPLINE_WEIGHT = case_when(DISCIPLINE_SIZE == "BIG"    ~ 0.01,
                                       DISCIPLINE_SIZE == "MEDIUM" ~ 0.5,
                                       DISCIPLINE_SIZE == "SMALL"  ~ 1.0,
                                       TRUE ~ 0.1)) %>%
  left_join(y  = AT_SEATS,
            by = c("SECTION_ID")) -> AT_SEATS



#################################
######## WEIGHTING SEATS ########
#################################

## Joining AT_SEATS onto AT_SELECTIONS and setting an initial weight of 10

AT_SELECTIONS %>%
  left_join(y = AT_SEATS,
            by = c("SECTION_ID")) %>%
  mutate(WEIGHT = 10) -> AT_WEIGHTS


## Blocking off males seats

AT_WEIGHTS %>%
  mutate(WEIGHT = case_when(SEAT_SEX == "MALE_ONLY"   & SEX != "M" ~ 0,
                            SEAT_SEX == "FEMALE_ONLY" & SEX != "F" ~ 0,
                            TRUE ~ WEIGHT)) %>%
  select(-SEAT_SEX) -> AT_WEIGHTS


## Blocking off seats for non-majors

AT_WEIGHTS %>%
  mutate(WEIGHT = case_when(SEAT_MAJOR == "NON_MAJOR_ONLY" & MAJOR_1 == PROF_DISCIPLINE ~ 0,
                            SEAT_MAJOR == "NON_MAJOR_ONLY" & MAJOR_2 == PROF_DISCIPLINE ~ 0,
                            TRUE ~ WEIGHT)) %>%
  select(-SEAT_MAJOR) -> AT_WEIGHTS


## Joining RELATED_DISP_WTS onto AT_WEIGHTS

AT_WEIGHTS %>%
  left_join(y  = RELATED_DISP_WTS,
            by = c("MAJOR_1"    = "INTENDED_MAJOR",
                   "PROF_DISCIPLINE" = "RELATED_DISP")) %>%
  left_join(y  = RELATED_DISP_WTS,
            by = c("MAJOR_2"    = "INTENDED_MAJOR",
                   "PROF_DISCIPLINE" = "RELATED_DISP")) %>%
  mutate(RELATED_DISP_WEIGHT.x = ifelse(is.na(RELATED_DISP_WEIGHT.x) == TRUE, 0, RELATED_DISP_WEIGHT.x),
         RELATED_DISP_WEIGHT.y = ifelse(is.na(RELATED_DISP_WEIGHT.y) == TRUE, 0, RELATED_DISP_WEIGHT.y),
         RELATED_DEPT_WEIGHT   = ifelse(RELATED_DISP_WEIGHT.x > RELATED_DISP_WEIGHT.y, RELATED_DISP_WEIGHT.x, RELATED_DISP_WEIGHT.y)) %>%
  select(-RELATED_DISP_WEIGHT.x,
         -RELATED_DISP_WEIGHT.y) -> AT_WEIGHTS

rm(RELATED_DISP_WTS)


## Adjust weights based on academic intersts

AT_WEIGHTS %>%
  mutate(WEIGHT = case_when(MAJOR_1 != "Undecided" & RELATED_DEPT_WEIGHT > 0 ~ WEIGHT*RELATED_DEPT_WEIGHT,
                            MAJOR_1 != "Undecided" & INTENDED_LANGUAGE == PROF_LANGUAGE ~ WEIGHT*0.25,
                            MAJOR_1 == "Undecided" ~ WEIGHT*0.5,
                            TRUE ~ WEIGHT*0.1)) %>%
  select(-RELATED_DEPT_WEIGHT,
         -PROF_DISCIPLINE,
         -INTENDED_LANGUAGE,
         -PROF_LANGUAGE) -> AT_WEIGHTS


## Multiplying by discipline weights

AT_WEIGHTS %>%
  mutate(WEIGHT = WEIGHT*DISCIPLINE_WEIGHT) %>%
  select(-DISCIPLINE_WEIGHT,
         -DISCIPLINE_SIZE) -> AT_WEIGHTS



######################################################
######## MATRIX CONTRUCTION & SOLVING PROBLEM ########
######################################################

## Re-shaping the data to create the final matrix

AT_WEIGHTS %>%
  transmute(STUDENT_ID = STUDENT_ID,
            SEAT      = paste("SEAT", SECTION_ID, SEAT_NUMBER, sep = "_"),
            WEIGHT    = WEIGHT) %>%
  spread(SEAT,
         WEIGHT) %>%
  mutate_at(.vars = vars(matches("SEAT_")),
            .funs = funs(ifelse(is.na(.), 0, .))) -> matrix_SEATS


## Adding in Dummy students tokeep the sections balanced

matrix_SEATS[nrow(AT_SEATS), ] <- NA

matrix_SEATS %>%
  mutate(STUDENT_ID = ifelse(is.na(STUDENT_ID) == TRUE, paste("DUMMY_", row_number()), STUDENT_ID)) %>%
  mutate_at(.vars = vars(matches("SEAT_\\d*_1$")),
            .funs = funs(ifelse(is.na(.) == TRUE, 1, .))) %>%
  mutate_at(.vars = vars(matches("SEAT_")),
            .funs = funs(ifelse(is.na(.) == TRUE, 0, .))) -> matrix_SEATS


## Solving the matrix

matrix_SEATS %>%
  select(-STUDENT_ID) %>%
  as.matrix() %>%
  solve_LSAP(maximum = TRUE) %>%
  as.numeric() -> AT_PLACEMENTS

## Getting the assigned section for each student

matrix_SEATS %>%
  mutate(ASSIGNMENT = AT_PLACEMENTS) %>%
  filter(str_detect(STUDENT_ID, "DUMMY_") == FALSE) %>%
  transmute(STUDENT_ID = STUDENT_ID,
            SECTION_ID = str_extract(names(.)[ASSIGNMENT+1], "(?<=SEAT_)C_\\d*(?=_)")) %>%
  select(STUDENT_ID,
         SECTION_ID) -> AT_PLACEMENTS

rm(matrix_SEATS, AT_SEATS, AT_WEIGHTS)



######################################
######## WRITING DATA TO FILE ########
######################################

AT_SELECTIONS %>%
  inner_join(y = AT_PLACEMENTS,
             by = c("STUDENT_ID", "SECTION_ID")) %>%
  left_join(y  = AT_COURSES,
            by = c("SECTION_ID")) %>%
  write.csv("Output//AT_PLACEMENTS.csv", row.names = FALSE)





