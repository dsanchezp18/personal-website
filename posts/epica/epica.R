# Epica Stream and Text Analysis
# Daniel Sanchez
# June 2023

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(lastfmR) # Use remotes::install_github("ppatrzyk/lastfmR") to install the lastfmR package
library(spotifyr)
library(tidyr)
library(lubridate)
library(fixest)
library(broom)

# Get scrobbles data from Last.fm -----------------------------------------

# Get artist information from Epica

epica_artist_info <-
  lastfmR::get_artist_info('Epica')

# Get all tracks for Epica

epica_tracks <-
  lastfmR::get_tracks('Epica')

# Check for duplicates

# Get artist data from Spotify's API --------------------------------------


# There is no albums mapping in this dataset, so I will get artists mapping from spotifyR

# Execute my credentials (unstaged in the repository for privacy reasons)

source('posts/epica/credentials.R')

# Set up my access token

access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
                                         client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

# Get stuff from Epica: at the album level.

epica_id <- '5HA5aLY3jJV7eimXWkRBBp'

epica_albums <-
  get_artist_albums(epica_id, 
                    authorization = access_token) %>%
  mutate(release_date = as.Date(release_date)) %>% 
  transmute(id, name, year = case_when(year(release_date) %>% is.na() ~ 2003, TRUE ~ year(release_date))) %>% 
  mutate(name = case_when(year == 2003 ~ 'The Phantom Agony',
                          year == 2005 ~ 'Consign to Oblivion',
                          TRUE ~ name)) %>% 
  filter(!(name %in% c('Live at Paradiso', 'Omega Alive', 'Kingdom of Heaven Part 3 - The Antediluvian Universe - Omega Alive -',
                       'Epica vs. Attack on Titan Songs', 'The Score 2.0 - An Epic Journey', 'The Holographic Principle (Track Commentary)',
                       'The Quantum Enigma (B-Sides)', 'Retrospect - 10th Anniversary (Live)','Design Your Universe (Gold Edition: Deluxe Edition)',
                       'The Classical Conspiracy (Live in Miskolc)', 'We Will Take You With Us')),
         id != '0cSFIq70TNkZvsqSfAQrXe') %>% 
  arrange(desc(year))

phantom_agony_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Phantom Agony') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'The Phantom Agony',
            album_id = epica_albums %>% filter(name == 'The Phantom Agony') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

consign_to_oblivion_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Consign to Oblivion') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'Consign to Oblivion',
            album_id = epica_albums %>% filter(name == 'Consign to Oblivion') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

divine_conspiracy_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Divine Conspiracy') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'The Divine Conspiracy',
            album_id = epica_albums %>% filter(name == 'The Divine Conspiracy') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

design_your_universe_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Design Your Universe') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'Design Your Universe',
            album_id = epica_albums %>% filter(name == 'Design Your Universe') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

requiem_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Requiem for the Indifferent') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'Requiem for the Indifferent',
            album_id = epica_albums %>% filter(name == 'Requiem for the Indifferent') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

quantum_enigma_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Quantum Enigma') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'The Quantum Enigma',
            album_id = epica_albums %>% filter(name == 'The Quantum Enigma') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

holographic_principle_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Holographic Principle') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'The Holographic Principle',
            album_id = epica_albums %>% filter(name == 'The Holographic Principle') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

omega_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Omega') %>% select(id)) %>% 
  transmute(id, name, 
            track_number, 
            album = 'Omega',
            album_id = epica_albums %>% filter(name == 'Omega') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

alchemy_project_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Alchemy Project') %>% select(id)) %>% 
  transmute(id, 
            name, 
            track_number, 
            album = 'The Alchemy Project',
            album_id = epica_albums %>% filter(name == 'The Alchemy Project') %>% select(id) %>% as.vector()) %>% 
  arrange(track_number)

# Since the solace system is an EP and not an album per se, I had to pull out the data for it differently:

solace_system_tracks <-
  get_album_tracks('280MqsLfyF7Tc9VOe2tm1X') %>% 
  transmute(id, name, track_number, 
            album = 'The Solace System', 
            year = 2017,
            album_id = '280MqsLfyF7Tc9VOe2tm1X') %>% 
  arrange(track_number)

epica_all_tracks <-
  phantom_agony_tracks %>% 
  bind_rows(consign_to_oblivion_tracks) %>% 
  bind_rows(design_your_universe_tracks) %>% 
  bind_rows(divine_conspiracy_tracks) %>% 
  bind_rows(requiem_tracks) %>% 
  bind_rows(quantum_enigma_tracks) %>% 
  bind_rows(holographic_principle_tracks) %>% 
  bind_rows(omega_tracks) %>% 
  bind_rows(alchemy_project_tracks) %>% 
  left_join(epica_albums %>% select(name, year), by = c('album' = 'name')) %>% 
  mutate(album_id = as.character(album_id)) %>% 
  bind_rows(solace_system_tracks) %>% 
  arrange(year)

# Adding album data to Last.fm data ---------------------------------------

# Inner join the songs dataset from lastfm to the song-album panel

epica_tracks_clean <-
  epica_tracks %>% 
  inner_join(epica_all_tracks, by = c('track' = 'name')) %>% 
  arrange(year, track_number) %>% 
  as.data.frame()

# Get metrics for tracks

epica_track_metrics <-
  get_artist_audio_features(epica_id) %>%
  select(track_name,
         track_number,
         album_name,
         album_id,
         energy,
         loudness,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo)

# Again, since the Solace System is an EP, I need to look it up differently

solace_system_audio_features <-
  get_artist_audio_features(epica_id, 
                            include_groups = 'single') %>% 
  filter(album_name == 'The Solace System') %>% 
  select(track_name,
         track_number,
         album_name,
         album_id,
         energy,
         loudness,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo)

# Bind solace system to the rest of my stuff

epica_track_metrics <-
  epica_track_metrics %>% 
  bind_rows(solace_system_audio_features)

# Summarise with averages at the album level

epica_album_metrics <-
  epica_track_metrics %>% 
  group_by(album_name, album_id) %>% 
  summarise(energy = mean(energy),
            loudness = mean(loudness),
            speechiness = mean(speechiness),
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness),
            liveness = mean(liveness),
            valence = mean(valence),
            tempo = mean(tempo)) %>% 
  ungroup() %>% 
  select(-album_name)

# Create scrobbles by album and join with the album metrics and with album table to get the year

scrobbles_by_album <-
  epica_tracks_clean %>% 
  group_by(album, album_id, year) %>% 
  summarise(scrobbles = sum(scrobbles)) %>% 
  ungroup() %>% 
  left_join(epica_album_metrics, by = 'album_id')

# Analysis ----------------------------------------------------------------

# Get a scrobbles ranking by album

scrobbles_by_album %>%
  ggplot(aes(reorder(album,scrobbles), scrobbles)) + 
  geom_col() + 
  coord_flip()

# Build a long form dataset to do better graphs of several audio metrics

scrobbles_by_album_long <-
  scrobbles_by_album %>% 
  gather(metric,
         value,
         energy,
         loudness,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo)

# Now graph an album ranking of some metrics

scrobbles_by_album_long %>% 
  filter(metric %in% c('energy', 'valence', 'instrumentalness')) %>% 
  ggplot(aes(reorder(album, year), value, fill = metric)) +
  geom_col(position = 'dodge') +
  coord_flip()
  
# Regression Model --------------------------------------------------------

# Run a regression model of scrobbles (all in log form) at the track level, do a coefficient plot

# First, construct the dataset by joining the track metrics data to the scrobbles data

log_x_1 <- function(x){log(x + 1)}

scrobbles_model_data <-
  epica_track_metrics %>% 
  left_join(epica_tracks_clean %>% select(track, listeners, scrobbles), by = c('track_name' = 'track')) %>%
  mutate(loudness = abs(loudness)) %>% 
  mutate_if(is.numeric, log_x_1)

scrobbles_model <- 
  feols(scrobbles ~ energy + loudness + speechiness + acousticness + instrumentalness + liveness + 
          valence + tempo | album_name,
        data = scrobbles_model_data,
        cluster = ~ album_name)

summary(scrobbles_model)

# Do a coefplot

coefs <- coef(scrobbles_model)
clustered_se <- sqrt(diag(vcov(scrobbles_model, cluster = scrobbles_model_data$album_name)))


df_for_plot <- tibble(variable = names(coefs),
                      coefficient = coefs,
                      std_error = clustered_se)

df_for_plot %>%
  arrange(coefficient) %>%
  mutate(variable = factor(variable, levels = variable)) %>%
  ggplot(aes(x = variable, y = coefficient, ymin = coefficient - 1.96*std_error, ymax = coefficient + 1.96*std_error)) +
  geom_pointrange() +
  coord_flip() +
  ylab("Coefficient estimate") +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  theme_bw() 

