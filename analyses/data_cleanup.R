source("startup.R")

df = read.csv("../data/study1a_data.csv") %>%
  select(-gender.1, -workerid.1, -assignmentid) %>%
  rename(assess = asses)
write.csv(df, "../data/study1a_data.csv", row.names = F)

df = read.csv("../data/study1b_data.csv") %>%
  select(-Assignment, -X)
write.csv(df, "../data/study1b_data.csv", row.names = F)

df = read.csv("../data/study2_data.csv") %>%
  select(-workerid.1, -assignmentid) %>%
  rename(assess = asses)
write.csv(df, "../data/study2_data.csv", row.names = F)

df = read.csv("../data/study3_data.csv") %>%
  select(-workerid.1, -assignmentid, -gender.1) %>%
  rename(assess = asses)
write.csv(df, "../data/study3_data.csv", row.names = F)

df = read.csv("../data/study4_data.csv") %>%
  select(-workerid.1, -assignmentid) %>%
  rename(assess = asses)
write.csv(df, "../data/study4_data.csv", row.names = F)

