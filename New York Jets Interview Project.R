
library(tidyr)
library(dplyr)
library(ggplot2)

offense <- read.csv("/Users/jonahlubin/Downloads/Intern Project/data.off.csv")
defense <- read.csv("/Users/jonahlubin/Downloads/Intern Project/data.def.csv")
plays <- read.csv("/Users/jonahlubin/Downloads/Intern Project/data.plays.csv")

#PLAYER AND TEAM STATISTIC SUMMARIES


#Cleaning Data on Offense
offense[offense$pff_DROPPEDPASS == "Y", "pff_DROPPEDPASS"] <- 1
offense[offense$pff_HURRYALLOWED == "Y", "pff_HURRYALLOWED"] <- 1
offense[offense$pff_PRESSUREALLOWED == "Y", "pff_PRESSUREALLOWED"] <- 1
offense[offense$pff_SACKALLOWED == "Y", "pff_SACKALLOWED"] <- 1
offense[offense$pff_INTERCEPTION == "Y", "pff_INTERCEPTION"] <- 1
offense[offense$pff_FUMBLE == "Y", "pff_FUMBLE"] <- 1
offense[offense$pff_TOUCHDOWN == "Y", "pff_TOUCHDOWN"] <- 1
offense[offense$pff_PASSER == "Y", "pff_PASSER"] <- 1
offense$completion <- ifelse(!is.na(offense$pff_PASSINGYARDS), 1, 0)
offense$incompletion <- ifelse(offense$pff_INCOMPLETIONTYPE != "", 1, 0)
offense <- offense %>%
  mutate(pff_DROPPEDPASS = as.numeric(pff_DROPPEDPASS),
         pff_HURRYALLOWED = as.numeric(pff_HURRYALLOWED),
         pff_PRESSUREALLOWED = as.numeric(pff_PRESSUREALLOWED),
         pff_SACKALLOWED = as.numeric(pff_SACKALLOWED),
         pff_INTERCEPTION = as.numeric(pff_INTERCEPTION),
         pff_TOUCHDOWN = as.numeric(pff_TOUCHDOWN),
         pff_PASSER = as.numeric(pff_PASSER),
         pff_FUMBLE = as.numeric(pff_FUMBLE))


#Cleaning Data on Defense
defense[defense$pff_ASSIST == "Y", "pff_ASSIST"] <- .5
defense[defense$pff_TACKLE == "Y", "pff_TACKLE"] <- 1
defense[defense$pff_BATTEDPASS == "Y", "pff_BATTEDPASS"] <- 1
defense[defense$pff_FORCEDFUMBLE == "Y", "pff_FORCEDFUMBLE"] <- 1
defense[defense$pff_FUMBLERECOVERED == "Y", "pff_FUMBLERECOVERED"] <- 1
defense[defense$pff_HURRY == "Y", "pff_HURRY"] <- 1
defense[defense$pff_PRESSURE == "Y", "pff_PRESSURE"] <- 1
defense[defense$pff_INTERCEPTION == "Y", "pff_INTERCEPTION"] <- 1
defense[defense$pff_MISSEDTACKLE == "Y", "pff_MISSEDTACKLE"] <- 1
defense[defense$pff_PASSBREAKUP == "Y", "pff_PASSBREAKUP"] <- 1
defense[defense$pff_PENALTY != "", "pff_PENALTY"] <- 1
defense[defense$pff_SACK == "Y", "pff_SACK"] <- 1
defense[defense$pff_STOP == "Y", "pff_STOP"] <- 1
defense[defense$pff_TOUCHDOWN == "Y", "pff_TOUCHDOWN"] <- 1
defense[defense$pff_BOXPLAYER == "Y", "pff_BOXPLAYER"] <- 1

defense <- defense %>%
  mutate(pff_ASSIST = as.numeric(pff_ASSIST),
         pff_TACKLE = as.numeric(pff_TACKLE),
         pff_BATTEDPASS = as.numeric(pff_BATTEDPASS),
         pff_FORCEDFUMBLE = as.numeric(pff_FORCEDFUMBLE),
         pff_FUMBLERECOVERED = as.numeric(pff_FUMBLERECOVERED),
         pff_HURRY = as.numeric(pff_HURRY),
         pff_PRESSURE = as.numeric(pff_PRESSURE),
         pff_INTERCEPTION = as.numeric(pff_INTERCEPTION),
         pff_MISSEDTACKLE = as.numeric(pff_MISSEDTACKLE),
         pff_PASSBREAKUP = as.numeric(pff_PASSBREAKUP),
         pff_BATTEDPASS = as.numeric(pff_BATTEDPASS),
         pff_PENALTY = as.numeric(pff_PENALTY),
         pff_SACK = as.numeric(pff_SACK),
         pff_STOP = as.numeric(pff_STOP),
         pff_TOUCHDOWN = as.numeric(pff_TOUCHDOWN),
         pff_BOXPLAYER = as.numeric(pff_BOXPLAYER),
         Open = as.numeric(Open))


#Merging Offensive Data With Plays, And Getting Rid Of Plays That Did Not Count
o_merged_data <- merge(offense, plays, by = "pff_PLAYID")
off_plays_no_nonplays <- subset(o_merged_data, pff_NOPLAY != 1)

d_merged_data <- merge(defense, plays, by = "pff_PLAYID")
def_plays_no_nonplays <- subset(d_merged_data, pff_NOPLAY != 1)


#PLAYER STATS:

#PLAYER PASSING STATS:

qbs <- subset(off_plays_no_nonplays, off_plays_no_nonplays$JetPosition == "QB")

passes <- subset(qbs, qbs$pff_ROLE == "Pass" & qbs$pff_BALLCARRIER.x!="Y")
num_passes <- passes %>%
  count(PlayerName) %>%
  rename(total_passes = n)

player_passing_stats <- passes %>%
  group_by(PlayerName) %>%
  summarize(team = first(pff_TEAM),
            completions = sum(completion, na.rm = TRUE),
            total_passes = n(),
            passing_yards = sum(pff_PASSINGYARDS, na.rm = TRUE),
            touchdowns = sum(pff_TOUCHDOWN.x, na.rm = TRUE),
            interceptions = sum(pff_INTERCEPTION, na.rm = TRUE),
            comp_perc = completions/total_passes,
            yards_per_attempt = passing_yards / total_passes,
            yards_after_catch = sum(pff_YARDSAFTERCATCH.x, na.rm = TRUE),
            intended_air_yards_per_attempt = sum(pff_PASSDEPTH, na.rm = TRUE) / total_passes, 
            passer_rating = (((comp_perc - .3) * 5 + (yards_per_attempt - 3)*.25 + (touchdowns/total_passes) * 20 + 2.375 - (interceptions/total_passes * 25))/6)*100) %>%
  arrange(desc(team), desc(completions))

qb_stats_ratings <- qbs %>%
  group_by(PlayerName) %>%
  summarize(pff_grade = sum(pff_PFFGRADE, na.rm = TRUE),
            pffvar = sum(PFFVAR, na.rm = TRUE),
            EPAR = sum(EPAR, na.rm = TRUE))

player_passing_stats <- player_passing_stats %>%
  left_join(qb_stats_ratings, by = "PlayerName")


#PLAYER RUSHING STATS:

runners <- subset(off_plays_no_nonplays, !is.na(off_plays_no_nonplays$pff_RUSHINGYARDS))
num_rushes <- runners %>%
  count(PlayerName) %>%
  rename(total_rushes = n)

player_rushing_stats <- runners %>%
  group_by(PlayerName) %>%
  summarize(team = first(pff_TEAM),
            total_rushes = n(),
            rushing_yards = sum(pff_RUSHINGYARDS, na.rm = TRUE),
            yards_per_carry = rushing_yards/total_rushes,
            touchdowns = sum(pff_TOUCHDOWN.x, na.rm = TRUE),
            fumbles_lost = sum(pff_FUMBLE, na.rm = TRUE),
            long = max(pff_RUSHINGYARDS, na.rm = TRUE),
            tackles_avoided = sum(pff_TACKLESAVOIDED, na.rm = TRUE),
            first_downs = sum(pff_FIRSTDOWNGAINED, na.rm = TRUE),
            pff_grade = sum(pff_PFFGRADE, na.rm = TRUE),
            pffvar = sum(PFFVAR, na.rm = TRUE),
            EPAR = sum(EPAR, na.rm = TRUE)) %>%
  arrange(desc(team), desc(total_rushes), desc(rushing_yards))


#PLAYER RECEIVING STATS:

receivers <- subset(off_plays_no_nonplays, !is.na(off_plays_no_nonplays$pff_RECEIVINGYARDS))
num_catches <- receivers %>%
  count(PlayerName) %>%
  rename(total_catches = n)

player_receiving_stats <- receivers %>%
  group_by(PlayerName) %>%
  summarize(team = first(pff_TEAM),
            total_catches = n(),
            receiving_yards = sum(pff_RECEIVINGYARDS , na.rm = TRUE),
            yards_after_catch = sum(pff_YARDSAFTERCATCH.x, na.rm = TRUE),
            yards_per_catch = receiving_yards/total_catches,
            touchdowns = sum(pff_TOUCHDOWN.x, na.rm = TRUE),
            fumbles_lost = sum(pff_FUMBLE, na.rm = TRUE),
            long = max(pff_RECEIVINGYARDS, na.rm = TRUE),
            first_downs = sum(pff_FIRSTDOWNGAINED, na.rm = TRUE),
            tackles_avoided = sum(pff_TACKLESAVOIDED, na.rm = TRUE),
            pff_grade = sum(pff_PFFGRADE, na.rm = TRUE),
            pffvar = sum(PFFVAR, na.rm = TRUE),
            EPAR = sum(EPAR, na.rm = TRUE)) %>%
  arrange(desc(team), desc(total_catches), desc(receiving_yards))

#OFFENSIVE LINEMEN STATS:

linemen <- subset(off_plays_no_nonplays, off_plays_no_nonplays$PosGroup == "OL")
num_snaps <- linemen %>%
  count(PlayerName) %>%
  rename(total_snaps = n)

player_offensive_line_stats <- linemen %>%
  group_by(PlayerName) %>%
  summarize(team = first(pff_TEAM),
            total_snaps = n(),
            pressures_allowed = sum(pff_PRESSUREALLOWED, na.rm = TRUE),
            hurries_allowed = sum(pff_HURRYALLOWED, na.rm = TRUE),
            sacks_allowed = sum(pff_SACKALLOWED, na.rm = TRUE),
            pff_grade = sum(pff_PFFGRADE, na.rm = TRUE),
            pffvar = sum(PFFVAR, na.rm = TRUE),
            EPAR = sum(EPAR, na.rm = TRUE)) %>%
  arrange(desc(team), desc(total_snaps), pressures_allowed)

#DEFENSIVE PLAYER STATS:

num_snaps <- def_plays_no_nonplays %>%
  count(PlayerName) %>%
  rename(total_snaps = n)

player_defensive_stats <- def_plays_no_nonplays %>%
  group_by(PlayerName) %>%
  summarize(team = first(pff_TEAM),
            total_snaps = n(),
            solo_tackles = sum(pff_TACKLE, na.rm = TRUE),
            assists = sum(pff_ASSIST, na.rm = TRUE) * 2,
            total_tackles = assists * .5 + solo_tackles,
            sacks = sum(pff_SACK, na.rm = TRUE),
            interceptions = sum(pff_INTERCEPTION, na.rm = TRUE),
            forced_fumbles = sum(pff_FORCEDFUMBLE, na.rm = TRUE),
            fumble_recoveries = sum(pff_FUMBLERECOVERED, na.rm = TRUE),
            touchdowns = sum(pff_TOUCHDOWN.x, na.rm = TRUE),
            hurries = sum(pff_HURRY, na.rm = TRUE),
            pressures = sum(pff_PRESSURE, na.rm = TRUE),
            batted_passes = sum(pff_BATTEDPASS, na.rm = TRUE),
            missed_tackles = sum(pff_MISSEDTACKLE, na.rm = TRUE),
            stops = sum(pff_STOP, na.rm = TRUE),
            pass_breakups = sum(pff_PASSBREAKUP, na.rm = TRUE),
            allowed_open_receiver = sum(Open, na.rm = TRUE),
            pff_PFFGRADE = sum(pff_PFFGRADE, na.rm = TRUE),
            pffvar = sum(PFFVAR, na.rm = TRUE),
            EPAR = sum(EPAR, na.rm = TRUE),) %>%
  arrange(desc(team), desc(total_snaps), desc(solo_tackles), desc(assists))


#TEAM STATISTIC SUMMARIES

#Getting rid of no-plays and non_offensive plays for each and binding
dalplays <- subset(plays, plays$pff_OFFTEAM=="DAL" & !is.na(plays$pff_DRIVE) & plays$pff_NOPLAY!=1)
philplays <- subset(plays, plays$pff_OFFTEAM=="PHI" & !is.na(plays$pff_DRIVE) & plays$pff_NOPLAY!=1)
all_actual_plays <- bind_rows(dalplays, philplays) %>%
  select(pff_OFFTEAM)

#Add sack counts
sacks <- subset(defense, defense$pff_SACK==1)
sacks_merged <- merge(sacks, plays, by = "pff_PLAYID")
sacks_merged <- sacks_merged %>%
  select(pff_PLAYID, pff_DEFTEAM, pff_GAINLOSSNET) %>%
  group_by(pff_PLAYID, pff_DEFTEAM) %>%
  summarize(sack_yards = mean(pff_GAINLOSSNET)) %>%
  rename(team="pff_DEFTEAM")
sacks_merged <- sacks_merged[,-1]

sacks_by_team <- sacks_merged %>% 
  count(team) %>% 
  rename(sacks = n)

#Merging all player statistics
all_player_stats <- bind_rows(player_passing_stats, player_rushing_stats, player_receiving_stats, player_offensive_line_stats, sacks_merged)
all_player_stats <- all_player_stats[,-c(1)]
team_stats_summary <- all_player_stats %>% 
  group_by(team) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))
team_stats_summary$total_yards <- team_stats_summary$passing_yards + team_stats_summary$rushing_yards + team_stats_summary$sack_yards


#Merge sacks with team_stats_summary
team_stats_summary <- left_join(team_stats_summary, sacks_by_team, by = "team")
team_stats_summary <- replace(team_stats_summary, is.na(team_stats_summary), 0)

#Get the total play counts and altering old columns
dal_plays_count <- nrow(filter(all_actual_plays, pff_OFFTEAM == "DAL"))
phi_plays_count <- nrow(filter(all_actual_plays, pff_OFFTEAM == "PHI"))
team_stats_summary <- team_stats_summary %>%
  mutate(number_of_plays = ifelse(team == "DAL", dal_plays_count, phi_plays_count))
team_stats_summary$yards_per_play <- team_stats_summary$total_yards / team_stats_summary$number_of_plays
team_stats_summary$passing_yards <- team_stats_summary$passing_yards + team_stats_summary$sack_yards
team_stats_summary$yards_per_carry <- team_stats_summary$rushing_yards / team_stats_summary$total_rushes 
team_stats_summary$yards_per_pass <- team_stats_summary$receiving_yards / team_stats_summary$total_passes 

#Getting third and fourth down statistics
dalthirddownsconv <- subset(plays, plays$pff_OFFTEAM=="DAL" & plays$Down==3 & plays$pff_NOPLAY!=1 & plays$pff_FIRSTDOWNGAINED)
philthirddownsconv <- subset(plays, plays$pff_OFFTEAM=="PHI" & plays$Down==3 & plays$pff_NOPLAY!=1 & plays$pff_FIRSTDOWNGAINED)
thirddownconv <- bind_rows(dalthirddownsconv, philthirddownsconv) %>%
  select(pff_OFFTEAM)
dal_thirddownconv_count <- nrow(filter(thirddownconv, pff_OFFTEAM == "DAL"))
phi_thirddownconv_count <- nrow(filter(thirddownconv, pff_OFFTEAM == "PHI"))
team_stats_summary <- team_stats_summary %>%
  mutate(third_downs_conversions = ifelse(team == "DAL", dal_thirddownconv_count, phi_thirddownconv_count))

dalthirddowns <- subset(plays, plays$pff_OFFTEAM=="DAL" & plays$Down==3 & plays$pff_NOPLAY!=1)
philthirddowns <- subset(plays, plays$pff_OFFTEAM=="PHI" & plays$Down==3 & plays$pff_NOPLAY!=1)
thirddowns <- bind_rows(dalthirddowns, philthirddowns) %>%
  select(pff_OFFTEAM)
dal_thirddown_count <- nrow(filter(thirddowns, pff_OFFTEAM == "DAL"))
phi_thirddown_count <- nrow(filter(thirddowns, pff_OFFTEAM == "PHI"))
team_stats_summary <- team_stats_summary %>%
  mutate(third_downs = ifelse(team == "DAL", dal_thirddown_count, phi_thirddown_count))

team_stats_summary$third_down_conversion_rate <- team_stats_summary$third_downs_conversions / team_stats_summary$third_downs

dalfourthdownsconv <- subset(plays, plays$pff_OFFTEAM=="DAL" & plays$Down==4 & plays$pff_NOPLAY!=1 & plays$pff_FIRSTDOWNGAINED)
philfourthdownsconv <- subset(plays, plays$pff_OFFTEAM=="PHI" & plays$Down==4 & plays$pff_NOPLAY!=1 & plays$pff_FIRSTDOWNGAINED)
fourthdownconv <- bind_rows(dalfourthdownsconv, philfourthdownsconv) %>%
  select(pff_OFFTEAM)
dal_fourthdownconv_count <- nrow(filter(fourthdownconv, pff_OFFTEAM == "DAL"))
phi_fourthdownconv_count <- nrow(filter(fourthdownconv, pff_OFFTEAM == "PHI"))
team_stats_summary <- team_stats_summary %>%
  mutate(fourth_downs_conversions = ifelse(team == "DAL", dal_fourthdownconv_count, phi_fourthdownconv_count))

dalfourthdowns <- subset(plays, plays$pff_OFFTEAM=="DAL" & plays$Down==4 & plays$pff_NOPLAY!=1 & plays$pff_KICKRESULT=="")
philfourthdowns <- subset(plays, plays$pff_OFFTEAM=="PHI" & plays$Down==4 & plays$pff_NOPLAY!=1 & plays$pff_KICKRESULT=="")
fourthdowns <- bind_rows(dalfourthdowns, philfourthdowns) %>%
  select(pff_OFFTEAM)
dal_fourthdown_count <- nrow(filter(fourthdowns, pff_OFFTEAM == "DAL"))
phi_fourthdown_count <- nrow(filter(fourthdowns, pff_OFFTEAM == "PHI"))
team_stats_summary <- team_stats_summary %>%
  mutate(fourth_downs = ifelse(team == "DAL", dal_fourthdown_count, phi_fourthdown_count))

team_stats_summary$fourth_down_conversion_rate <- team_stats_summary$fourth_downs_conversions / team_stats_summary$fourth_downs

#Cleaning the Team Statistics
team_stats_summary <- team_stats_summary %>%
  select(team, number_of_plays, total_yards, yards_per_play, first_downs, third_downs, third_downs_conversions, third_down_conversion_rate, fourth_downs, fourth_downs_conversions, fourth_down_conversion_rate,
         completions, total_passes, comp_perc, passing_yards, interceptions, passer_rating, yards_per_attempt, intended_air_yards_per_attempt,
         total_rushes, rushing_yards, yards_per_carry, yards_after_catch, fumbles_lost, tackles_avoided,
         sacks, sack_yards, pressures_allowed, hurries_allowed)

colnames(team_stats_summary) <- c("Team", "Number of Plays", "Total Yards", "Yards Per Play", "First Downs", "Third Downs", "Third Down Conversions", "Third Down Conversion Rate", "Fourth Downs", "Fourth Down Conversions", "Fourth Down Conversion Rate",
                              "Total Completions", "Total Passes", "Completion Percentage", "Total Passing Yards", "Interceptions Thrown", "Passer Rating", "Yards Per Attempt", "Intended Air Yards Per Attempt",
                              "Total Rushes", "Rushing Yards", "Yards Per Carry", "Yards After Catch", "Fumbles Lost", "Tackles Avoided",
                              "Total Sacks", "Total Sack Yards", "Pressures Alllowed", "Hurries Allowed")

#Cleaning Player Statistics
player_passing_stats <- player_passing_stats %>%
  select(PlayerName, team, completions, total_passes, comp_perc, passing_yards, touchdowns, interceptions, passer_rating, yards_per_attempt, yards_after_catch, intended_air_yards_per_attempt, pff_grade, pffvar, EPAR)
colnames(player_passing_stats) <- c("Player Name", "Team", "Total Completions", "Total Passes", "Completion Percentage", "Passing Yards", "Touchdowns", "Interceptions", "Passer Rating", "Yards Per Attempt", "Yards After Catch", "Intended Air Yards Per Attempt", "PFF Grade +/-", "PFFVAR", "EPAR")

colnames(player_rushing_stats) <- c("Player Name", "Team", "Total Rushes", "Rushing Yards", "Yards Per Carry", "Touchdowns", "Fumbles Lost", "Longest Run", "Tackles Avoided", "First Downs", "PFF Grade +/-", "PFFVAR", "EPAR")

player_receiving_stats <- player_receiving_stats %>%
  select(PlayerName, team, total_catches, receiving_yards, yards_per_catch, yards_after_catch, touchdowns, fumbles_lost, long, tackles_avoided, first_downs, pff_grade, pffvar, EPAR)
colnames(player_receiving_stats) <- c("Player Name", "Team", "Total Catches", "Receiving Yards", "Yards Per Catch", "Yards After Catch", "Touchdowns", "Fumbles Lost", "Longest Catch", "Tackles Avoided", "First Downs", "PFF Grade +/-", "PFFVAR", "EPAR")

colnames(player_offensive_line_stats) <- c("Player Name", "Team", "Total Snaps", "Pressures Allowed", "Hurries Allowed", "Sacks Allowed", "PFF Grade +/-", "PFFVAR", "EPAR")

colnames(player_defensive_stats) <- c("Player Name", "Team", "Total Snaps", "Solo Tackles", "Assists", "Total Tackles", "Sacks", "Interceptions", "Forced Fumbles", "Fumble Recoveries", "Touchdowns", "Hurries", "Pressures", "Batted Passes", "Missed Tackles", "Stops", "Pass Breakups", "Allowed Open Receiver", "PFF Grade +/-", "PFFVAR", "EPAR")



#ANALYZING DATA FOR INSIGHTS

#Subsetting the team stats for each team
dalteamstats <- subset(team_stats_summary, team_stats_summary$Team=="DAL")
philteamstats <- subset(team_stats_summary, team_stats_summary$Team=="PHI")

#See how effective both teams were with no huddle offense
dalnohuddle <- subset(plays, plays$pff_OFFTEAM=="DAL" & !is.na(plays$pff_DRIVE) & plays$pff_NOHUDDLE==1 & plays$pff_NOPLAY!=1)
philnohuddle <- subset(plays, plays$pff_OFFTEAM=="PHI" & !is.na(plays$pff_DRIVE) & plays$pff_NOHUDDLE==1 & plays$pff_NOPLAY!=1)

dal_difference_no_hudd <- mean(dalnohuddle$pff_GAINLOSSNET) - dalteamstats$`Yards Per Play`

phil_difference_no_hudd <- mean(philnohuddle$pff_GAINLOSSNET) - philteamstats$`Yards Per Play` 


#Check to see if the amount of the players in box affects rushing yards or passing yards
dal_offense_and_plays <- subset(off_plays_no_nonplays, off_plays_no_nonplays$pff_TEAM=="DAL")
phil_offense_and_plays <- subset(off_plays_no_nonplays, off_plays_no_nonplays$pff_TEAM=="PHI")
dal_defense_and_plays <- subset(def_plays_no_nonplays, def_plays_no_nonplays$pff_TEAM=="DAL")
phil_defense_and_plays <- subset(def_plays_no_nonplays, def_plays_no_nonplays$pff_TEAM=="PHI")
phil_pass <- subset(phil_offense_and_plays, phil_offense_and_plays$pff_ROLE=="Pass")
dal_pass <- subset(dal_offense_and_plays, dal_offense_and_plays$pff_ROLE=="Pass")

dalbox <- aggregate(pff_BOXPLAYER ~ pff_PLAYID, data = dal_defense_and_plays, FUN = sum)
philrushyards <- aggregate(pff_RUSHINGYARDS ~ pff_PLAYID, data = phil_offense_and_plays, FUN = sum)
philpassingyards <- aggregate(pff_GAINLOSSNET ~ pff_PLAYID, data = phil_pass, FUN = sum)
dalbox_vs_philrushyards <- merge(philrushyards, dalbox, by = "pff_PLAYID")
dalbox_vs_philrushyardsmean <- aggregate(pff_RUSHINGYARDS ~ pff_BOXPLAYER, data = dalbox_vs_philrushyards, FUN = mean)
dalbox_vs_philrushyardssum <- aggregate(pff_RUSHINGYARDS ~ pff_BOXPLAYER, data = dalbox_vs_philrushyards, FUN = sum)
dalbox_vs_philrushyards <- merge(dalbox_vs_philrushyardsmean, dalbox_vs_philrushyardssum, by = "pff_BOXPLAYER")
dalbox_vs_philrushyards$Freq <- dalbox_vs_philrushyards$pff_RUSHINGYARDS.y / dalbox_vs_philrushyards$pff_RUSHINGYARDS.x
colnames(dalbox_vs_philrushyards) <- c("Number of Players in Box", "Mean Net Gain on Running Plays", "Total Net Gain on Running Plays", "Number of Running Plays")

dalbox_vs_philpassingyards <- merge(philpassingyards, dalbox, by = "pff_PLAYID")
dalbox_vs_philpassingyardsmean <- aggregate(pff_GAINLOSSNET ~ pff_BOXPLAYER, data = dalbox_vs_philpassingyards, FUN = mean)
dalbox_vs_philpassingyardssum <- aggregate(pff_GAINLOSSNET ~ pff_BOXPLAYER, data = dalbox_vs_philpassingyards, FUN = sum)
dalbox_vs_philpassingyards <- merge(dalbox_vs_philpassingyardsmean, dalbox_vs_philpassingyardssum, by = "pff_BOXPLAYER")
dalbox_vs_philpassingyards$Freq <- dalbox_vs_philpassingyards$pff_GAINLOSSNET.y / dalbox_vs_philpassingyards$pff_GAINLOSSNET.x
colnames(dalbox_vs_philpassingyards) <- c("Number of Players in Box", "Mean Net Gain on Passing Plays", "Total Net Gain on Passing Plays", "Number of Passing Plays")

philbox <- aggregate(pff_BOXPLAYER ~ pff_PLAYID, data = phil_defense_and_plays, FUN = sum)
dalrushyards <- aggregate(pff_RUSHINGYARDS ~ pff_PLAYID, data = dal_offense_and_plays, FUN = sum)
dalpassingyards <- aggregate(pff_GAINLOSSNET ~ pff_PLAYID, data = dal_pass, FUN = sum)
philbox_vs_dalrushyards <- merge(dalrushyards, philbox, by = "pff_PLAYID")
philbox_vs_dalrushyardsmean <- aggregate(pff_RUSHINGYARDS ~ pff_BOXPLAYER, data = philbox_vs_dalrushyards, FUN = mean)
philbox_vs_dalrushyardssum <- aggregate(pff_RUSHINGYARDS ~ pff_BOXPLAYER, data = philbox_vs_dalrushyards, FUN = sum)
philbox_vs_dalrushyards <- merge(philbox_vs_dalrushyardsmean, philbox_vs_dalrushyardssum, by = "pff_BOXPLAYER")
philbox_vs_dalrushyards$Freq <- philbox_vs_dalrushyards$pff_RUSHINGYARDS.y / philbox_vs_dalrushyards$pff_RUSHINGYARDS.x
colnames(philbox_vs_dalrushyards) <- c("Number of Players in Box", "Mean Net Gain on Running Plays", "Total Net Gain on Running Plays", "Number of Running Plays")

philbox_vs_dalpassingyards <- merge(dalpassingyards, philbox, by = "pff_PLAYID")
philbox_vs_dalpassingyardsmean <- aggregate(pff_GAINLOSSNET ~ pff_BOXPLAYER, data = philbox_vs_dalpassingyards, FUN = mean)
philbox_vs_dalpassingyardssum <- aggregate(pff_GAINLOSSNET ~ pff_BOXPLAYER, data = philbox_vs_dalpassingyards, FUN = sum)
philbox_vs_dalpassingyards <- merge(philbox_vs_dalpassingyardsmean, philbox_vs_dalpassingyardssum, by = "pff_BOXPLAYER")
philbox_vs_dalpassingyards$Freq <- philbox_vs_dalpassingyards$pff_GAINLOSSNET.y / philbox_vs_dalpassingyards$pff_GAINLOSSNET.x
colnames(philbox_vs_dalpassingyards) <- c("Number of Players in Box", "Mean Net Gain on Passing Plays", "Total Net Gain on Passing Plays", "Number of Passing Plays")
philbox_vs_dalpassingyards <- philbox_vs_dalpassingyards[-1,]

phil_ypcarry <- team_stats_summary$`Yards Per Carry`[2]
dal_ypcarry <- team_stats_summary$`Yards Per Carry`[1]

phil_yppass <- mean(phil_pass$pff_GAINLOSSNET)
dal_yppass <- mean(dal_pass$pff_GAINLOSSNET)


dal_box_analysis <- dalbox_vs_philrushyards
dal_box_analysis$`Percent More Than Average Yards Per Carry ` <- dal_box_analysis$`Mean Net Gain on Running Plays` / phil_ypcarry
dal_box_analysis <- merge(dal_box_analysis, dalbox_vs_philpassingyards, by = "Number of Players in Box", all = TRUE)
dal_box_analysis$`Percent More Than Average Yards Per Passing Play` <- dal_box_analysis$`Mean Net Gain on Passing Plays` / phil_yppass
dal_box_analysis$`Average Percent More Total Yards Over Average Allowed` <- (dal_box_analysis$`Percent More Than Average Yards Per Carry ` + dal_box_analysis$`Percent More Than Average Yards Per Passing Play`) /2
dal_box_analysis$`Total Plays` <- dal_box_analysis$`Number of Running Plays` + dal_box_analysis$`Number of Passing Plays`

phil_box_analysis <- philbox_vs_dalrushyards
phil_box_analysis$`Percent More Than Average Yards Per Carry ` <- phil_box_analysis$`Mean Net Gain on Running Plays` / dal_ypcarry
phil_box_analysis <- merge(phil_box_analysis, philbox_vs_dalpassingyards, by = "Number of Players in Box", all = TRUE)
phil_box_analysis$`Percent More Than Average Yards Per Passing Play` <- phil_box_analysis$`Mean Net Gain on Passing Plays` / dal_yppass
phil_box_analysis$`Average Percent More Total Yards Over Average Allowed` <- (phil_box_analysis$`Percent More Than Average Yards Per Carry ` + phil_box_analysis$`Percent More Than Average Yards Per Passing Play`) /2
phil_box_analysis$`Total Plays` <- phil_box_analysis$`Number of Running Plays` + phil_box_analysis$`Number of Passing Plays`


#GRAPH RESULTS - Don't Love Them
# par(mfrow=c(2,2))
# 
# plot(NA, xlim = c(5, 10), ylim = c(0, 20), xlab = "Number of People in the Box", ylab = "Average Yards Gained")
# 
# lines(dalbox_vs_philpassingyards$`Number of Players in Box`, dalbox_vs_philpassingyards$`Mean Net Gain on Passing Plays`, col = "lightgreen")
# lines(dalbox_vs_philrushyards$`Number of Players in Box`, dalbox_vs_philrushyards$`Mean Net Gain on Running Plays`, col = "darkgreen")
# legend("topright", legend = c("Passing Plays", "Running Plays"), col = c("lightgreen", "darkgreen"), lty = 1, cex = .2)
# 
# plot(NA, xlim = c(5, 10), ylim = c(0, 20), xlab = "Number of People in the Box", ylab = "Number of Plays")
# 
# lines(dalbox_vs_philpassingyards$`Number of Players in Box`, dalbox_vs_philpassingyards$`Number of Passing Plays`, col = "lightgreen")
# lines(dalbox_vs_philrushyards$`Number of Players in Box`, dalbox_vs_philrushyards$`Number of Running Plays`, col = "darkgreen")
# legend("topright", legend = c("Passing Plays", "Running Plays"), col = c("lightgreen", "darkgreen"), lty = 1, cex = .2)
# 
# plot(NA, xlim = c(5, 10), ylim = c(0, 20), xlab = "Number of People in the Box", ylab = "Average Yards Gained")
# 
# lines(philbox_vs_dalpassingyards$`Number of Players in Box`, philbox_vs_dalpassingyards$`Mean Net Gain on Passing Plays`, col = "lightblue")
# lines(philbox_vs_dalrushyards$`Number of Players in Box`, philbox_vs_dalrushyards$`Mean Net Gain on Running Plays`, col = "darkblue")
# legend("topright", legend = c("Passing Plays", "Running Plays"), col = c("lightblue", "darkblue"), lty = 1, cex = .2)
# 
# plot(NA, xlim = c(5, 10), ylim = c(0, 20), xlab = "Number of People in the Box", ylab = "Number of Plays")
# 
# lines(philbox_vs_dalpassingyards$`Number of Players in Box`, philbox_vs_dalpassingyards$`Number of Passing Plays`, col = "lightblue")
# lines(philbox_vs_dalrushyards$`Number of Players in Box`, philbox_vs_dalrushyards$`Number of Running Plays`, col = "darkblue")
# legend("topright", legend = c("Passing Plays", "Running Plays"), col = c("lightblue", "darkblue"), lty = 1, cex = .2)
# 
# #
# par(mfrow=c(1,1))


#Time to throw analysis
philttt <- subset(phil_offense_and_plays, phil_offense_and_plays$pff_TIMETOTHROW>0) %>%
  select(pff_PLAYID, pff_TIMETOTHROW, Rushers, pff_GAINLOSSNET)%>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID = first(pff_PLAYID),
            pff_TIMETOTHROW = first(pff_TIMETOTHROW),
            Rushers = first(Rushers),
            pff_GAINLOSSNET = first(pff_GAINLOSSNET))

philtttrushers <- philttt %>%
  select(pff_PLAYID, pff_TIMETOTHROW, Rushers, pff_GAINLOSSNET) %>%
  group_by(Rushers) %>%
  summarize(mean_pff_TIMETOTHROW = mean(pff_TIMETOTHROW),
            mean_pff_GAINLOSSNET = mean(pff_GAINLOSSNET))



dalttt <- subset(dal_offense_and_plays, dal_offense_and_plays$pff_TIMETOTHROW>0) %>%
  select(pff_PLAYID, pff_TIMETOTHROW, Rushers, pff_GAINLOSSNET)%>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID = first(pff_PLAYID),
            pff_TIMETOTHROW = first(pff_TIMETOTHROW),
            Rushers = first(Rushers),
            pff_GAINLOSSNET = first(pff_GAINLOSSNET))

daltttrushers <- dalttt %>%
  select(pff_PLAYID, pff_TIMETOTHROW, Rushers, pff_GAINLOSSNET) %>%
  group_by(Rushers) %>%
  summarize(mean_pff_TIMETOTHROW = mean(pff_TIMETOTHROW),
            mean_pff_GAINLOSSNET = mean(pff_GAINLOSSNET))
daltttrushers <- daltttrushers[-2,]


cor(philttt$pff_TIMETOTHROW, philttt$pff_GAINLOSSNET)

cor(dalttt$pff_TIMETOTHROW, dalttt$pff_GAINLOSSNET)



#Check to see if running to a certain direction was beneficial

dalrundirection <- subset(dal_offense_and_plays, dal_offense_and_plays$pff_ROLE=="Run") %>%
  select(pff_PLAYID, pff_RBDIRECTION.x, pff_GAINLOSSNET)%>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID = first(pff_PLAYID),
            pff_RBDIRECTION.x = first(pff_RBDIRECTION.x),
            pff_GAINLOSSNET = first(pff_GAINLOSSNET))%>%
  select(pff_PLAYID, pff_RBDIRECTION.x, pff_GAINLOSSNET) %>%
  group_by(pff_RBDIRECTION.x) %>%
  summarize(mean_pff_GAINLOSSNET = mean(pff_GAINLOSSNET))

philrundirection <- subset(phil_offense_and_plays, phil_offense_and_plays$pff_ROLE=="Run") %>%
  select(pff_PLAYID, pff_RBDIRECTION.x, pff_GAINLOSSNET)%>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID = first(pff_PLAYID),
            pff_RBDIRECTION.x = first(pff_RBDIRECTION.x),
            pff_GAINLOSSNET = first(pff_GAINLOSSNET))%>%
  select(pff_PLAYID, pff_RBDIRECTION.x, pff_GAINLOSSNET) %>%
  group_by(pff_RBDIRECTION.x) %>%
  summarize(mean_pff_GAINLOSSNET = mean(pff_GAINLOSSNET))



dalrunpoaactual <- subset(dal_offense_and_plays, dal_offense_and_plays$pff_ROLE=="Run") %>%
  select(pff_PLAYID, pff_POAACTUAL, pff_GAINLOSSNET)%>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID = first(pff_PLAYID),
            pff_POAACTUAL = first(pff_POAACTUAL),
            pff_GAINLOSSNET = first(pff_GAINLOSSNET))%>%
  select(pff_PLAYID, pff_POAACTUAL, pff_GAINLOSSNET) %>%
  group_by(pff_POAACTUAL) %>%
  summarize(mean_pff_GAINLOSSNET = mean(pff_GAINLOSSNET))

philrunpoaactual <- subset(phil_offense_and_plays, phil_offense_and_plays$pff_ROLE=="Run") %>%
  select(pff_PLAYID, pff_POAACTUAL, pff_GAINLOSSNET)%>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID = first(pff_PLAYID),
            pff_POAACTUAL = first(pff_POAACTUAL),
            pff_GAINLOSSNET = first(pff_GAINLOSSNET))%>%
  select(pff_PLAYID, pff_POAACTUAL, pff_GAINLOSSNET) %>%
  group_by(pff_POAACTUAL) %>%
  summarize(mean_pff_GAINLOSSNET = mean(pff_GAINLOSSNET))


dal_change_gap <- subset(dal_offense_and_plays, dal_offense_and_plays$pff_POAACTUAL!=dal_offense_and_plays$pff_POAINTENDED)
dal_change_gap <- subset(dal_change_gap, dal_change_gap$pff_ROLE=="Run")

phil_change_gap <- subset(phil_offense_and_plays, phil_offense_and_plays$pff_POAACTUAL!=phil_offense_and_plays$pff_POAINTENDED)
phil_change_gap <- subset(phil_change_gap, phil_change_gap$pff_ROLE=="Run")


#Check to see if disguising the MOF affects the gained yards per play

dalmofchange <- subset(dal_defense_and_plays, dal_defense_and_plays$pff_MOFOCPLAYED!=dal_defense_and_plays$pff_MOFOCSHOWN)
philmofchange <- subset(phil_defense_and_plays, phil_defense_and_plays$pff_MOFOCPLAYED!=phil_defense_and_plays$pff_MOFOCSHOWN)
dalnomofchange <- subset(dal_defense_and_plays, dal_defense_and_plays$pff_MOFOCPLAYED==dal_defense_and_plays$pff_MOFOCSHOWN)
philnomofchange <- subset(phil_defense_and_plays, phil_defense_and_plays$pff_MOFOCPLAYED==phil_defense_and_plays$pff_MOFOCSHOWN)

dalmofchange <- dalmofchange%>%
  select(pff_PLAYID, pff_MOFOCPLAYED, pff_MOFOCSHOWN) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            pff_MOFOCSHOWN = first(pff_MOFOCSHOWN),
            pff_MOFOCPLAYED = first(pff_MOFOCPLAYED))

philmofchange <- philmofchange%>%
  select(pff_PLAYID, pff_MOFOCPLAYED, pff_MOFOCSHOWN) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            pff_MOFOCSHOWN = first(pff_MOFOCSHOWN),
            pff_MOFOCPLAYED = first(pff_MOFOCPLAYED))

dalnomofchange <- dalnomofchange%>%
  select(pff_PLAYID, pff_MOFOCPLAYED, pff_MOFOCSHOWN) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            pff_MOFOCSHOWN = first(pff_MOFOCSHOWN),
            pff_MOFOCPLAYED = first(pff_MOFOCPLAYED))

philnomofchange <- philnomofchange%>%
  select(pff_PLAYID, pff_MOFOCPLAYED, pff_MOFOCSHOWN) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            pff_MOFOCSHOWN = first(pff_MOFOCSHOWN),
            pff_MOFOCPLAYED = first(pff_MOFOCPLAYED))


dalmofchange_vs_philpassingyards <- merge(philpassingyards, dalmofchange, by = "pff_PLAYID")
dalnomofchange_vs_philpassingyards <- merge(philpassingyards, dalnomofchange, by = "pff_PLAYID")
dalnomofchange_vs_philpassingyards <- dalnomofchange_vs_philpassingyards[-20,]
phil_passing_gain_loss_dal_mof_change <- mean(dalmofchange_vs_philpassingyards$pff_GAINLOSSNET)
phil_passing_gain_loss_dal_no_mof_change <- mean(dalnomofchange_vs_philpassingyards$pff_GAINLOSSNET)
phil_passing_difference_no_mof_vs_mof_change <- phil_passing_gain_loss_dal_mof_change - phil_passing_gain_loss_dal_no_mof_change

philmofchange_vs_dalpassingyards <- merge(dalpassingyards, philmofchange, by = "pff_PLAYID")
philnomofchange_vs_dalpassingyards <- merge(dalpassingyards, philnomofchange, by = "pff_PLAYID")
dal_passing_gain_loss_phil_mof_change <- mean(philmofchange_vs_dalpassingyards$pff_GAINLOSSNET)
dal_passing_gain_loss_phil_no_mof_change <- mean(philnomofchange_vs_dalpassingyards$pff_GAINLOSSNET)
dal_passing_difference_no_mof_vs_mof_change <- dal_passing_gain_loss_phil_mof_change - dal_passing_gain_loss_phil_no_mof_change

passing_gain_loss_mof_change <- c(phil_passing_gain_loss_dal_mof_change, dal_passing_gain_loss_phil_mof_change)
passing_gain_loss_no_mof_change <- c(phil_passing_gain_loss_dal_no_mof_change, dal_passing_gain_loss_phil_no_mof_change)
passing_difference_no_mof_vs_mof_change <- c(phil_passing_difference_no_mof_vs_mof_change, dal_passing_difference_no_mof_vs_mof_change)

passing_against_mof_change_analysis <- data.frame(
  Team = c("PHI", "DAL"),
  passing_gain_loss_mof_change,
  passing_gain_loss_no_mof_change,
  passing_difference_no_mof_vs_mof_change
)

passing_against_mof_change_analysis_long <- pivot_longer(passing_against_mof_change_analysis,
                                                         cols = c(passing_gain_loss_no_mof_change, passing_gain_loss_mof_change, passing_difference_no_mof_vs_mof_change),
                                                         names_to = "variable",
                                                         values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("passing_gain_loss_mof_change", "passing_gain_loss_no_mof_change", "passing_difference_no_mof_vs_mof_change")))

ggplot(passing_against_mof_change_analysis_long, aes(x = Team, y = value, fill = variable, label = round(value, 3))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(position=position_dodge(width=.7), vjust=-.5) +
  labs(x = "Team", y = "Average Yardage per Passing Play", title = "How Each Team Did Against Disguised Middle of the Fields", fill = NULL, caption = "Week 16: Dallas Cowboys vs Philadelphia Eagles") +
  scale_fill_manual(values = c("firebrick2", "grey9", "blue"),
                    breaks = c("passing_difference_no_mof_vs_mof_change", "passing_gain_loss_no_mof_change", "passing_gain_loss_mof_change"),
                    labels = c("Difference (MOF Disguised - MOF Not Disguised)", "Against Non-Disguised Middle of Field Pre-Snap", "Against Disguised Middle of Field Pre-Snap"),
                    guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "lightgray"))

colnames(passing_against_mof_change_analysis) <- c("Team", "Net Gain on Passing Plays When the MOF Look Is Disguised Pre Snap", "Net Gain on Passing Plays When the MOF Look Is Not Disguised Pre Snap", "Difference")

#Check out RPO data
dalrpos <- subset(off_plays_no_nonplays, off_plays_no_nonplays$pff_RUNPASSOPTION == 1 & off_plays_no_nonplays$pff_TEAM=="DAL") %>%
  select(pff_PLAYID, pff_GAINLOSSNET) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            pff_GAINLOSSNET=first(pff_GAINLOSSNET))
mean(dalrpos$pff_GAINLOSSNET)

philrpos <- subset(off_plays_no_nonplays, off_plays_no_nonplays$pff_RUNPASSOPTION == 1 & off_plays_no_nonplays$pff_TEAM=="PHI") %>%
  select(pff_PLAYID, pff_GAINLOSSNET) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            pff_GAINLOSSNET=first(pff_GAINLOSSNET))
mean(philrpos$pff_GAINLOSSNET)

#Teams against zone
dalzone <- subset(def_plays_no_nonplays, def_plays_no_nonplays$pff_TEAM=="DAL" & def_plays_no_nonplays$ManZone == "Zone") %>%
select(pff_PLAYID) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID))

philzone <- subset(def_plays_no_nonplays, def_plays_no_nonplays$pff_TEAM=="PHI" & def_plays_no_nonplays$ManZone == "Zone") %>%
  select(pff_PLAYID) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID))

#Philadelphia against zone
philoff_vs_dalzone <- merge(phil_offense_and_plays, dalzone, by = 'pff_PLAYID')
philqb_vs_dalzone <- subset(philoff_vs_dalzone, philoff_vs_dalzone$JetPosition=="QB") %>%
select(pff_PLAYID, completion, incompletion, pff_PASSINGYARDS) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            completion=first(completion),
            incompletion=first(incompletion),
            pff_PASSINGYARDS=first(pff_PASSINGYARDS))
philqb_vs_dalzone$pass <- philqb_vs_dalzone$completion + philqb_vs_dalzone$incompletion
philqb_vs_dalzone <- subset(philqb_vs_dalzone, philqb_vs_dalzone$pass==1)
philqb_vs_dalzone[is.na(philqb_vs_dalzone)] <- 0
philqb_vs_dalzone <- philqb_vs_dalzone[,-1]
colSums(philqb_vs_dalzone)

#Dallas against zone
daloff_vs_philzone <- merge(dal_offense_and_plays, philzone, by = 'pff_PLAYID')
dalqb_vs_philzone <- subset(daloff_vs_philzone, daloff_vs_philzone$JetPosition=="QB") %>%
  select(pff_PLAYID, completion, incompletion, pff_PASSINGYARDS) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            completion=first(completion),
            incompletion=first(incompletion),
            pff_PASSINGYARDS=first(pff_PASSINGYARDS))
dalqb_vs_philzone$pass <- dalqb_vs_philzone$completion + dalqb_vs_philzone$incompletion
dalqb_vs_philzone <- subset(dalqb_vs_philzone, dalqb_vs_philzone$pass==1)
dalqb_vs_philzone[is.na(dalqb_vs_philzone)] <- 0
dalqb_vs_philzone <- dalqb_vs_philzone[,-1]
colSums(dalqb_vs_philzone)

#Teams against man
dalman <- subset(def_plays_no_nonplays, def_plays_no_nonplays$pff_TEAM=="DAL" & def_plays_no_nonplays$ManZone == "Man") %>%
  select(pff_PLAYID) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID))

philman <- subset(def_plays_no_nonplays, def_plays_no_nonplays$pff_TEAM=="PHI" & def_plays_no_nonplays$ManZone == "Man") %>%
  select(pff_PLAYID) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID))

#Philadelphia against man
philoff_vs_dalman <- merge(phil_offense_and_plays, dalman, by = 'pff_PLAYID')
philqb_vs_dalman <- subset(philoff_vs_dalman, philoff_vs_dalman$JetPosition=="QB") %>%
  select(pff_PLAYID, completion, incompletion, pff_PASSINGYARDS) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            completion=first(completion),
            incompletion=first(incompletion),
            pff_PASSINGYARDS=first(pff_PASSINGYARDS))
philqb_vs_dalman$pass <- philqb_vs_dalman$completion + philqb_vs_dalman$incompletion
philqb_vs_dalman <- subset(philqb_vs_dalman, philqb_vs_dalman$pass==1)
philqb_vs_dalman[is.na(philqb_vs_dalman)] <- 0
philqb_vs_dalman <- philqb_vs_dalman[,-1]
colSums(philqb_vs_dalman)

#Dallas against man
daloff_vs_philman <- merge(dal_offense_and_plays, philman, by = 'pff_PLAYID')
dalqb_vs_philman <- subset(daloff_vs_philman, daloff_vs_philman$JetPosition=="QB") %>%
  select(pff_PLAYID, completion, incompletion, pff_PASSINGYARDS) %>%
  group_by(pff_PLAYID) %>%
  summarize(pff_PLAYID=first(pff_PLAYID),
            completion=first(completion),
            incompletion=first(incompletion),
            pff_PASSINGYARDS=first(pff_PASSINGYARDS))
dalqb_vs_philman$pass <- dalqb_vs_philman$completion + dalqb_vs_philman$incompletion
dalqb_vs_philman <- subset(dalqb_vs_philman, dalqb_vs_philman$pass==1)
dalqb_vs_philman[is.na(dalqb_vs_philman)] <- 0
dalqb_vs_philman <- dalqb_vs_philman[,-1]
colSums(dalqb_vs_philman)


#Checking biggest WP swings in the game
play_details <- plays %>%
  select(pff_SORTORDER, PlayDescription, pff_QUARTER, pff_CLOCK)

wps <- plays %>%
  select(pff_SORTORDER, WP) %>%
  group_by(pff_SORTORDER) %>%
  summarize(pff_SORTORDER = first(pff_SORTORDER),
            WP = first(WP))

wp_diff <- wps %>%
  filter(!is.na(pff_SORTORDER)) %>%
  mutate(next_WP = lead(WP)) %>%
  filter(!is.na(next_WP)) %>%
  mutate(diff_WP = abs(next_WP - WP),
         team_benefitted = ifelse(next_WP > WP, "DAL", "PHI")) %>%
  arrange(desc(diff_WP)) %>%
  left_join(play_details, by = "pff_SORTORDER") %>%
  arrange(desc(diff_WP)) %>%
  select(pff_QUARTER, pff_CLOCK, team_benefitted, WP, next_WP, diff_WP, PlayDescription)

wp_diff_top_10 <- head(wp_diff, n=10)


#Determining which players had the biggest impact in their units

passing_metrics <- player_passing_stats %>%
  select(`Player Name`, Team, `PFF Grade +/-`, PFFVAR, EPAR)

rushing_metrics <- player_rushing_stats %>%
  select(`Player Name`, Team, `PFF Grade +/-`, PFFVAR, EPAR)

receiving_metrics <- player_receiving_stats %>%
  select(`Player Name`, Team, `PFF Grade +/-`, PFFVAR, EPAR)

offensive_line_metrics <- player_offensive_line_stats %>%
  select(`Player Name`, Team, `PFF Grade +/-`, PFFVAR, EPAR)

defensive_metrics <- player_defensive_stats %>%
  select(`Player Name`, Team, `PFF Grade +/-`, PFFVAR, EPAR)

offensive_metrics <- bind_rows(passing_metrics, rushing_metrics, receiving_metrics, offensive_line_metrics) %>%
  group_by(`Player Name`) %>%
  summarize(`Player Name` = first(`Player Name`),
            Team = first(Team),
            `PFF Grade +/-` = sum(`PFF Grade +/-`),
            PFFVAR = sum(PFFVAR),
            EPAR = sum(EPAR),
            `Average Value Above Replacement` = sum(PFFVAR, EPAR) / 2) %>%
  arrange(Team, desc(`Average Value Above Replacement`)) %>%
  select(`Player Name`, Team, PFFVAR, EPAR, `Average Value Above Replacement`)

defensive_metrics$`Average Value Above Replacement` <- (defensive_metrics$PFFVAR + defensive_metrics$EPAR) / 2
defensive_metrics <- defensive_metrics %>%
  select(`Player Name`, Team, PFFVAR, EPAR, `Average Value Above Replacement`) %>%
  arrange(Team, desc(`Average Value Above Replacement`))


#Exporting Tables into CSV

write.csv(player_passing_stats, file = "player_passing_stats.csv", row.names = FALSE)
write.csv(player_rushing_stats, file = "player_rushing_stats.csv", row.names = FALSE)
write.csv(player_receiving_stats, file = "player_receiving_stats.csv", row.names = FALSE)
write.csv(player_offensive_line_stats, file = "player_offensive_line_stats.csv", row.names = FALSE)
write.csv(player_defensive_stats, file = "player_defensive_stats.csv", row.names = FALSE)
write.csv(team_stats_summary, file = "team_stats_summary.csv", row.names = FALSE)
write.csv(wp_diff_top_10, file = "win_probability_shifts_top_10.csv", row.names = FALSE)

