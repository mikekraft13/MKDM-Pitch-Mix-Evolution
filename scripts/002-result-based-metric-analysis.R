# Mike's working script

# Pulling cleaned pitch-by-pitch data out of our list
clean_savant_pbp2014 <- as.data.frame(filtered_dfs[1])
clean_savant_pbp2015 <- as.data.frame(filtered_dfs[2])
clean_savant_pbp2016 <- as.data.frame(filtered_dfs[3])
clean_savant_pbp2017 <- as.data.frame(filtered_dfs[4])
clean_savant_pbp2018 <- as.data.frame(filtered_dfs[5])
clean_savant_pbp2019 <- as.data.frame(filtered_dfs[6])
clean_savant_pbp2020 <- as.data.frame(filtered_dfs[7])
clean_savant_pbp2021 <- as.data.frame(filtered_dfs[8])
clean_savant_pbp2022 <- as.data.frame(filtered_dfs[9])
clean_savant_pbp2023 <- as.data.frame(filtered_dfs[10])
clean_savant_pbp2024 <- as.data.frame(filtered_dfs[11])


# Overall (2014-2024) analysis of individual pitch usage w/ respect to velocity
# FF = 4-seam fastball; SI = sinker; FC = cutter
# CH = changeup; FS = split-finger; FO = forkball; SC = screwball
# CU = curveball; KC = knuckle-curve; CS = slow curve
# SL = slider; ST = sweeper; SV = slurve
# KN = knuckleball
# EP = eephus; FA = other; IN = intentional ball; PO = pitchout

filtered_dfs_binded <- filtered_dfs_binded |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

# Same logic as above, but with yearly dataframes
clean_savant_pbp2014 <- clean_savant_pbp2014 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2015 <- clean_savant_pbp2015 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2016 <- clean_savant_pbp2016 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2017 <- clean_savant_pbp2017 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2018 <- clean_savant_pbp2018 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2019 <- clean_savant_pbp2019 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2020 <- clean_savant_pbp2020 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2021 <- clean_savant_pbp2021 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2022 <- clean_savant_pbp2022 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2023 <- clean_savant_pbp2023 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))

clean_savant_pbp2024 <- clean_savant_pbp2024 |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str.sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS' ~ 'Breaking-Curve'),
                                 pitch_type %in% c('SL', 'ST', 'SV' ~ 'Breaking-Slider'),
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out
                                 TRUE ~ NA_character_))






