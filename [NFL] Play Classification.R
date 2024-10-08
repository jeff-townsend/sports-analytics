library(tidyverse)
library(nflreadr)
library(ggthemes)

pbp.import <-
  load_pbp() %>%
  filter(!is.na(play_type))

pbp <-
  pbp.import %>%
  rename(play_type_pbp = play_type) %>%
  arrange(start_time, game_id, play_id) %>%
  mutate(play_id = c(1:(nrow(pbp.import))))

plays <-
  pbp %>%
  select(play_id, game_id, week, posteam, defteam, desc, yards_gained, play_type_pbp, play_type_nfl,
         qb_dropback, pass_attempt, sack, qb_scramble, rush_attempt, qb_spike, qb_kneel,
         penalty, penalty_team, penalty_yards, epa, success) %>%
  ## overwrite NA values
  mutate(yards_gained = ifelse(!is.na(yards_gained), yards_gained, 0),
         pass_attempt = ifelse(!is.na(pass_attempt), pass_attempt, 0),
         sack = ifelse(!is.na(sack), sack, 0),
         rush_attempt = ifelse(!is.na(rush_attempt), rush_attempt, 0),
         penalty = ifelse(!is.na(penalty), penalty, 1), # offsetting penalties
         penalty_yards = ifelse(!is.na(penalty_yards), penalty_yards, 0)) %>%
  mutate(play = ifelse(play_type_pbp == "no_play", 0, 1),
         unit = ifelse(play_type_nfl %in% c("KICK_OFF", "PUNT", "FIELD_GOAL", "XP_KICK"), "Special Teams", "Offense"),
         sub_unit = ifelse(unit == "Special Teams",
                           ifelse(play_type_nfl == "KICK_OFF",
                                  "Kickoff",
                                  ifelse(play_type_nfl == "PUNT",
                                         "Punt",
                                         ifelse(play_type_nfl == "FIELD_GOAL",
                                                "Field Goal",
                                                "Extra Point"))),
                           ifelse(play_type_nfl == "PAT2",
                                  "Conversion",
                                  "Offense")),
         penalty_type = ifelse(penalty == 1,
                               ifelse(is.na(penalty_team),
                                      "Offsetting Penalties",
                                      ifelse(penalty_team == posteam,
                                             "Offensive Penalty",
                                             "Defensive Penalty")),
                               "No Penalty")) %>%
  mutate(play_type = ifelse(qb_dropback == 1 & pass_attempt == 1 & sack == 0, "Pass Attempt", "Other"),
         play_type = ifelse(qb_dropback == 1 & sack == 1, "Sack", play_type),
         play_type = ifelse(qb_dropback == 1 & qb_scramble == 1, "Scramble", play_type),
         play_type = ifelse(qb_dropback == 0 & rush_attempt == 1 & qb_kneel == 0, "Designed Run", play_type),
         play_type = ifelse(qb_spike == 1, "Spike", play_type),
         play_type = ifelse(qb_kneel == 1, "Kneel", play_type),
         play_type = ifelse(penalty == 1 & play == 0, "Penalty Play", play_type),
         play_type = ifelse(play_type_nfl == "KICK_OFF" & play == 1, "Kickoff", play_type),
         play_type = ifelse(play_type_nfl == "PUNT" & play == 1, "Punt", play_type),
         play_type = ifelse(play_type_nfl == "FIELD_GOAL" & play == 1, "Field Goal", play_type),
         play_type = ifelse(play_type_nfl == "XP_KICK" & play == 1, "Extra Point", play_type),
         play_type = ifelse(play_type_nfl == "TIMEOUT", "Timeout", play_type)) %>%
  mutate(offensive_play = ifelse(play_type %in% c("Pass Attempt", "Designed Run", "Scramble", "Sack") & sub_unit == "Offense", 1, 0)) %>%
  mutate(pass_attempt = ifelse(qb_dropback == 1 & pass_attempt == 1 & sack == 0, 1, 0),
         sack = ifelse(qb_dropback == 1 & sack == 1, 1, 0),
         scramble = ifelse(qb_dropback == 1 & qb_scramble == 1, 1, 0),
         designed_run = ifelse(qb_dropback == 0 & rush_attempt == 1 & qb_kneel == 0, 1, 0),
         spike = ifelse(qb_spike == 1 & play == 1, 1, 0),
         kneel = ifelse(qb_kneel == 1 & play == 1, 1, 0),
         penalty_play = ifelse(penalty == 1 & play == 0, 1, 0),
         kickoff = ifelse(play_type_nfl == "KICK_OFF" & play == 1, 1, 0),
         punt = ifelse(play_type_nfl == "PUNT" & play == 1, 1, 0),
         field_goal = ifelse(play_type_nfl == "FIELD_GOAL" & play == 1, 1, 0),
         extra_point = ifelse(play_type_nfl == "XP_KICK" & play == 1, 1, 0),
         timeout = ifelse(play_type_nfl == "TIMEOUT", 1, 0)) %>%
  mutate(play_types = pass_attempt + sack + scramble + designed_run + spike + kneel + penalty_play +
           kickoff + punt + field_goal + extra_point + timeout) %>%
  select(play_id, game_id, week, posteam, defteam, desc, yards_gained, epa, success,
         play_type, unit, sub_unit, penalty_type, offensive_play,
         pass_attempt, sack, scramble, designed_run, spike, kneel, penalty_play,
         kickoff, punt, field_goal, extra_point, timeout, play_types)

offense.games <-
  plays %>%
  filter(offensive_play == 1) %>%
  group_by(posteam, week) %>%
  summarize(off_plays = n(),
            off_epa = sum(epa),
            off_successes = sum(success)) %>%
  ungroup() %>%
  mutate(off_epa_per_play = off_epa / off_plays,
         off_success_rate = off_successes / off_plays) %>%
  rename(team = posteam)
  
offense <-
  plays %>%
  filter(offensive_play == 1) %>%
  group_by(posteam) %>%
  summarize(off_plays = n(),
            off_epa = sum(epa),
            off_successes = sum(success)) %>%
  ungroup() %>%
  mutate(off_epa_per_play = off_epa / off_plays,
         off_success_rate = off_successes / off_plays) %>%
  rename(team = posteam)

defense.games <-
  plays %>%
  filter(offensive_play == 1) %>%
  group_by(defteam, week) %>%
  summarize(def_plays = n(),
            def_epa = sum(epa),
            def_successes = sum(success)) %>%
  ungroup() %>%
  mutate(def_epa_per_play = def_epa / def_plays,
         def_success_rate = def_successes / def_plays) %>%
  rename(team = defteam)

defense <-
  plays %>%
  filter(offensive_play == 1) %>%
  group_by(defteam) %>%
  summarize(def_plays = n(),
            def_epa = sum(epa),
            def_successes = sum(success)) %>%
  ungroup() %>%
  mutate(def_epa_per_play = def_epa / def_plays,
         def_success_rate = def_successes / def_plays) %>%
  rename(team = defteam)

weekly.metrics <-
  offense.games %>%
  inner_join(defense.games, by = c("team", "week")) %>%
  mutate(week = paste("Week", week, sep = " ")) %>%
  select(team, week, off_epa_per_play, def_epa_per_play, off_success_rate, def_success_rate) %>%
  mutate(epa_per_play_delta = off_epa_per_play - def_epa_per_play,
         success_rate_delta = off_success_rate - def_success_rate)

season.metrics <-
  offense %>%
  inner_join(defense, by = "team") %>%
  select(team, off_epa_per_play, def_epa_per_play, off_success_rate, def_success_rate) %>%
  mutate(epa_per_play_delta = off_epa_per_play - def_epa_per_play,
         success_rate_delta = off_success_rate - def_success_rate)

success.rate <- with(plays %>% filter(offensive_play == 1), mean(success))
season.low.sr <- min(c(with(season.metrics, min(off_success_rate)), with(season.metrics, min(def_success_rate))))
season.high.sr <- max(c(with(season.metrics, max(off_success_rate)), with(season.metrics, max(def_success_rate))))

epa.per.play <- with(plays %>% filter(offensive_play == 1), mean(epa))
season.low.epa <- min(c(with(season.metrics, min(off_epa_per_play)), with(season.metrics, min(def_epa_per_play))))
season.high.epa <- max(c(with(season.metrics, max(off_epa_per_play)), with(season.metrics, max(def_epa_per_play))))

# success rate chart
ggplot(season.metrics, aes(x = off_success_rate, y = def_success_rate, label = team)) +
  geom_text() +
  scale_x_continuous(limits = c(0.3, 0.55)) +
  scale_y_continuous(limits = c(0.3, 0.55)) +
  geom_hline(yintercept = success.rate, linetype = "dotted") +
  geom_vline(xintercept = success.rate, linetype = "dotted") +
  geom_abline(linetype = "dotted") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("NFL Team Success Rate") +
  xlab("Offensive Success Rate") +
  ylab("Defensive Success Rate") +
  annotate("polygon", x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf),
           fill = "green",
           alpha = 0.05) +
  annotate("polygon", x = c(Inf, -Inf, -Inf), y = c(Inf, -Inf, Inf),
           fill = "red",
           alpha = 0.05) +
  annotate("rect", xmin = -Inf, xmax = success.rate, ymin = success.rate, ymax = Inf,
           fill = "red",
           alpha = 0.25) +
  annotate("rect", xmin = success.rate, xmax = Inf, ymin = -Inf, ymax = success.rate,
           fill = "green",
           alpha = 0.25) +
  annotate("text", x = .305, y = .36, label = "Good Defense", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .305, y = .49, label = "Bad Both Sides", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .505, y = .36, label = "Good Both Sides", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .505, y = .49, label = "Good Offense", hjust = 0, fontface = "bold.italic")

# epa per play chart
ggplot(season.metrics, aes(x = off_epa_per_play, y = def_epa_per_play, label = team)) +
  geom_text() +
  scale_x_continuous(limits = c(-0.3, 0.3)) +
  scale_y_continuous(limits = c(-0.3, 0.3)) +
  geom_hline(yintercept = epa.per.play, linetype = "dotted") +
  geom_vline(xintercept = epa.per.play, linetype = "dotted") +
  geom_abline(linetype = "dotted") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("NFL Team EPA per Play") +
  xlab("Offensive EPA per Play") +
  ylab("Defensive EPA per Play") +
  annotate("polygon", x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf),
           fill = "green",
           alpha = 0.05) +
  annotate("polygon", x = c(Inf, -Inf, -Inf), y = c(Inf, -Inf, Inf),
           fill = "red",
           alpha = 0.05) +
  annotate("rect", xmin = -Inf, xmax = epa.per.play, ymin = epa.per.play, ymax = Inf,
           fill = "red",
           alpha = 0.25) +
  annotate("rect", xmin = epa.per.play, xmax = Inf, ymin = -Inf, ymax = epa.per.play,
           fill = "green",
           alpha = 0.25) +
  annotate("text", x = -.295, y = .36, label = "Good Defense", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .305, y = .49, label = "Bad Both Sides", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .505, y = .36, label = "Good Both Sides", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .505, y = .49, label = "Good Offense", hjust = 0, fontface = "bold.italic")


weekly.low <- with(weekly.metrics, min(off_success_rate))
weekly.high <- with(weekly.metrics, max(off_success_rate))
ggplot(weekly.metrics %>% filter(team == "GB"), aes(x = off_success_rate, y = def_success_rate, label = week)) +
  geom_text() +
  scale_x_continuous(limits = c(0.2, 0.65)) +
  scale_y_continuous(limits = c(0.2, 0.65)) +
  geom_hline(yintercept = success.rate, linetype = "dotted") +
  geom_vline(xintercept = success.rate, linetype = "dotted") +
  geom_abline(linetype = "dotted") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggtitle("Green Bay Packers' Weekly Success Rate") +
  xlab("Offensive Success Rate") +
  ylab("Defensive Success Rate") +
  annotate("polygon", x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf),
           fill = "green",
           alpha = 0.05) +
  annotate("polygon", x = c(Inf, -Inf, -Inf), y = c(Inf, -Inf, Inf),
           fill = "red",
           alpha = 0.05) +
  annotate("rect", xmin = -Inf, xmax = success.rate, ymin = success.rate, ymax = Inf,
           fill = "red",
           alpha = 0.25) +
  annotate("rect", xmin = success.rate, xmax = Inf, ymin = -Inf, ymax = success.rate,
           fill = "green",
           alpha = 0.25) +
  annotate("text", x = .205, y = .385, label = "Good Defense", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .205, y = .485, label = "Bad Both Sides", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .505, y = .385, label = "Good Both Sides", hjust = 0, fontface = "bold.italic") +
  annotate("text", x = .505, y = .485, label = "Good Offense", hjust = 0, fontface = "bold.italic")
