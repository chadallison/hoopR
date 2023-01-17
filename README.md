hoopR
================
chad allison
2023-01-16

### setup

``` r
library(tidyverse)
library(hoopR)
theme_set(theme_minimal())
```

### data import

``` r
# write_csv(load_mbb_pbp(seasons = 2023), "pbp_data.csv")
df = read_csv("pbp_data.csv", col_types = cols())
glimpse(df)
```

    ## Rows: 1,111,934
    ## Columns: 55
    ## $ id                              <dbl> 4.014829e+17, 4.014829e+17, 4.014829e+…
    ## $ sequence_number                 <dbl> 101806601, 101806602, 101806603, 10180…
    ## $ type_id                         <dbl> 519, 540, 540, 558, 587, 572, 519, 598…
    ## $ type_text                       <chr> "PersonalFoul", "MadeFreeThrow", "Made…
    ## $ text                            <chr> "Foul on Caleb Love.", "Shykeim Philli…
    ## $ away_score                      <dbl> 0, 1, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4,…
    ## $ home_score                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2,…
    ## $ period_number                   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ period_display_value            <chr> "1st Half", "1st Half", "1st Half", "1…
    ## $ clock_display_value             <time> 19:33:00, 19:33:00, 19:33:00, 19:14:0…
    ## $ scoring_play                    <lgl> FALSE, TRUE, TRUE, FALSE, FALSE, TRUE,…
    ## $ score_value                     <dbl> 0, 1, 1, 2, 0, 2, 0, 0, 2, 0, 0, 1, 1,…
    ## $ team_id                         <dbl> 153, 350, 350, 153, 350, 350, 153, 153…
    ## $ participants_0_athlete_id       <dbl> 4433144, 4592975, 4592975, 4395650, 45…
    ## $ wallclock                       <dttm> 2022-11-08 02:07:19, 2022-11-08 02:07…
    ## $ shooting_play                   <lgl> FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, …
    ## $ participants_1_athlete_id       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ season                          <dbl> 2023, 2023, 2023, 2023, 2023, 2023, 20…
    ## $ season_type                     <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ away_team_id                    <dbl> 350, 350, 350, 350, 350, 350, 350, 350…
    ## $ away_team_name                  <chr> "UNC Wilmington", "UNC Wilmington", "U…
    ## $ away_team_mascot                <chr> "Seahawks", "Seahawks", "Seahawks", "S…
    ## $ away_team_abbrev                <chr> "UNCW", "UNCW", "UNCW", "UNCW", "UNCW"…
    ## $ away_team_name_alt              <chr> "UNC Wilmington", "UNC Wilmington", "U…
    ## $ home_team_id                    <dbl> 153, 153, 153, 153, 153, 153, 153, 153…
    ## $ home_team_name                  <chr> "North Carolina", "North Carolina", "N…
    ## $ home_team_mascot                <chr> "Tar Heels", "Tar Heels", "Tar Heels",…
    ## $ home_team_abbrev                <chr> "UNC", "UNC", "UNC", "UNC", "UNC", "UN…
    ## $ home_team_name_alt              <chr> "North Carolina", "North Carolina", "N…
    ## $ home_team_spread                <dbl> 23.5, 23.5, 23.5, 23.5, 23.5, 23.5, 23…
    ## $ game_spread                     <dbl> -23.5, -23.5, -23.5, -23.5, -23.5, -23…
    ## $ home_favorite                   <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
    ## $ game_spread_available           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
    ## $ game_id                         <dbl> 401482947, 401482947, 401482947, 40148…
    ## $ qtr                             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ time                            <time> 19:33:00, 19:33:00, 19:33:00, 19:14:0…
    ## $ clock_minutes                   <dbl> 19, 19, 19, 19, 19, 18, 18, 18, 18, 18…
    ## $ clock_seconds                   <chr> "33", "33", "33", "14", "11", "51", "3…
    ## $ half                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ game_half                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ lag_qtr                         <dbl> NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ lead_qtr                        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ lag_game_half                   <dbl> NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ lead_game_half                  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ start_quarter_seconds_remaining <dbl> 1173, 1173, 1173, 1154, 1151, 1131, 11…
    ## $ start_half_seconds_remaining    <dbl> 1773, 1773, 1773, 1754, 1751, 1731, 17…
    ## $ start_game_seconds_remaining    <dbl> 2973, 2973, 2973, 2954, 2951, 2931, 29…
    ## $ game_play_number                <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,…
    ## $ end_quarter_seconds_remaining   <dbl> 600, 1173, 1173, 1173, 1154, 1151, 113…
    ## $ end_half_seconds_remaining      <dbl> 1200, 1773, 1773, 1773, 1754, 1751, 17…
    ## $ end_game_seconds_remaining      <dbl> 2400, 2973, 2973, 2973, 2954, 2951, 29…
    ## $ period                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ coordinate_x                    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ coordinate_y                    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ media_id                        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

### getting end game data

``` r
end_game = df |>
  filter(type_text == "End Game") |>
  select(home_team_name, away_team_name, home_score, away_score) |>
  mutate(win_team = ifelse(home_score > away_score, home_team_name, away_team_name),
         lose_team = ifelse(win_team == home_team_name, away_team_name, home_team_name)) |>
  rename(home_team = home_team_name, away_team = away_team_name)

sample_n(end_game, 10)
```

    ## # A tibble: 10 × 6
    ##    home_team           away_team            home_score away_sc…¹ win_t…² lose_…³
    ##    <chr>               <chr>                     <dbl>     <dbl> <chr>   <chr>  
    ##  1 Iona                St. Bonaventure              72        57 Iona    St. Bo…
    ##  2 UC Davis            Arkansas State               75        60 UC Dav… Arkans…
    ##  3 Tennessee Tech      Kentucky Christian          104        72 Tennes… Kentuc…
    ##  4 Lindenwood          Lamar                        71        73 Lamar   Linden…
    ##  5 UNC Asheville       Western Carolina             73        61 UNC As… Wester…
    ##  6 Eastern Illinois    Southern Indiana             91        80 Easter… Southe…
    ##  7 Mercer              Middle Georgia State        100        62 Mercer  Middle…
    ##  8 Houston             North Florida                76        42 Houston North …
    ##  9 Ole Miss            Alcorn State                 73        58 Ole Mi… Alcorn…
    ## 10 Charleston Southern Longwood                     74        79 Longwo… Charle…
    ## # … with abbreviated variable names ¹​away_score, ²​win_team, ³​lose_team

### getting team records

*note that only teams having played ten or more games are included*

``` r
all_teams = sort(unique(c(end_game$home_team, end_game$away_team)))
team_records = data.frame(team = all_teams)

team_wins = end_game |>
  count(win_team) |>
  rename(team = win_team)

team_losses = end_game |>
  count(lose_team) |>
  rename(team = lose_team)

team_records = team_records |>
  left_join(team_wins, by = "team") |>
  rename(wins = n) |>
  left_join(team_losses, by = "team") |>
  rename(losses = n) |>
  mutate(wins = ifelse(is.na(wins), 0, wins),
         losses = ifelse(is.na(losses), 0, losses),
         games_played = wins + losses,
         win_prop = round(wins / games_played, 3)) |>
  filter(games_played >= 10)

all_teams = team_records$team
rm(team_wins, team_losses)

team_records |>
  arrange(desc(win_prop)) |>
  head(10)
```

    ##                team wins losses games_played win_prop
    ## 1        Charleston   17      1           18    0.944
    ## 2           Houston   17      1           18    0.944
    ## 3            Kansas   16      1           17    0.941
    ## 4  Florida Atlantic   15      1           16    0.938
    ## 5            Purdue   15      1           16    0.938
    ## 6        New Mexico   16      2           18    0.889
    ## 7              UCLA   16      2           18    0.889
    ## 8           Alabama   15      2           17    0.882
    ## 9             Texas   15      2           17    0.882
    ## 10     Kansas State   12      2           14    0.857

### getting team offensive points per game stats

``` r
team_off_ppg = data.frame(team = all_teams, off_ppg = NA)

for (i in 1:nrow(team_off_ppg)) {
  
  home_total = end_game |>
    filter(home_team == team_off_ppg$team[i]) |>
    pull(home_score) |>
    sum()
  
  away_total = end_game |>
    filter(away_team == team_off_ppg$team[i]) |>
    pull(away_score) |>
    sum()
  
  team_total = home_total + away_total
  team_gp = team_records$games_played[which(team_records$team == team_off_ppg$team[i])]
  team_ppg = round(team_total / team_gp, 3)
  team_off_ppg$off_ppg[i] = team_ppg
  
}

team_off_ppg |>
  arrange(desc(off_ppg)) |>
  head(10)
```

    ##                team off_ppg
    ## 1           Gonzaga  86.286
    ## 2          Missouri  85.688
    ## 3               UAB  85.500
    ## 4     James Madison  85.000
    ## 5  Youngstown State  84.895
    ## 6           Arizona  84.833
    ## 7            Toledo  84.600
    ## 8     Southern Utah  84.588
    ## 9           Alabama  84.353
    ## 10           Xavier  84.167

### getting team defensive points per game stats

``` r
team_def_ppg = data.frame(team = all_teams, def_ppg = NA)

for (i in 1:nrow(team_def_ppg)) {
  
  home_total = end_game |>
    filter(home_team == team_def_ppg$team[i]) |>
    pull(away_score) |>
    sum()
  
  away_total = end_game |>
    filter(away_team == team_def_ppg$team[i]) |>
    pull(home_score) |>
    sum()
  
  team_total = home_total + away_total
  team_gp = team_records$games_played[which(team_records$team == team_def_ppg$team[i])]
  team_ppg = round(team_total / team_gp, 3)
  team_def_ppg$def_ppg[i] = team_ppg
  
}

team_def_ppg |>
  arrange(def_ppg) |>
  head(10)
```

    ##                 team def_ppg
    ## 1            Houston  52.889
    ## 2          Tennessee  53.250
    ## 3        North Texas  54.294
    ## 4            Rutgers  56.944
    ## 5  Mississippi State  57.471
    ## 6       Saint Mary's  57.632
    ## 7         Iowa State  57.938
    ## 8        Sam Houston  58.500
    ## 9             Dayton  58.750
    ## 10           Liberty  59.000

### joining offensive and defensive points per game stats

``` r
team_ppg = team_off_ppg |>
  left_join(team_def_ppg, by = "team")

rm(team_off_ppg, team_def_ppg)

team_ppg |>
  mutate(diff = off_ppg - def_ppg) |>
  arrange(desc(diff)) |>
  left_join(team_records, by = "team") |>
  ggplot(aes(diff, win_prop)) +
  geom_point(aes(col = win_prop)) +
  scale_color_gradient(high = "springgreen3", low = "indianred1") +
  labs(x = "offensive ppg - defensive ppg",
       y = "win percentage",
       title = "team win percentage by ppg differential") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](hoopR_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

*work in progress, to be continued*
