# Epica Stream and Text Analysis
# Daniel Sanchez
# June 2023

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(lastfmR) # Use remotes::install_github("ppatrzyk/lastfmR") to install the lastfmR package
library(spotifyr)
library(lubridate)

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
  transmute(id, name, track_number, album = 'The Phantom Agony') %>% 
  arrange(track_number)

consign_to_oblivion_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Consign to Oblivion') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'Consign to Oblivion') %>% 
  arrange(track_number)

divine_conspiracy_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Divine Conspiracy') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'The Divine Conspiracy') %>% 
  arrange(track_number)

design_your_universe_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Design Your Universe') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'Design Your Universe') %>% 
  arrange(track_number)

requiem_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Requiem for the Indifferent') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'Requiem for the Indifferent') %>% 
  arrange(track_number)

quantum_enigma_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Quantum Enigma') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'The Quantum Enigma') %>% 
  arrange(track_number)

holographic_principle_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Holographic Principle') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'The Holographic Principle') %>% 
  arrange(track_number)

omega_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'Omega') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'Omega') %>% 
  arrange(track_number)

alchemy_project_tracks <-
  get_album_tracks(epica_albums %>% filter(name == 'The Alchemy Project') %>% select(id)) %>% 
  transmute(id, name, track_number, album = 'The Alchemy Project') %>% 
  arrange(track_number)

solace_system_tracks <-
  get_album_tracks('280MqsLfyF7Tc9VOe2tm1X') %>% 
  transmute(id, name, track_number, album = 'The Solace System', year = 2017) %>% 
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
  bind_rows(solace_system_tracks) %>% 
  arrange(year)

# Adding album data to Last.fm data ---------------------------------------

# Inner join the songs dataset from lastfm to the song-album panel

epica_tracks_clean <-
  epica_tracks %>% 
  inner_join(epica_all_tracks, by = c('track' = 'name')) %>% 
  arrange(year, track_number) %>% 
  as.data.frame()

# Analysis ----------------------------------------------------------------

# Get a scrobbles ranking by album

scrobbles_by_album <-
  epica_tracks_clean %>% 
  group_by(album) %>% 
  summarise(scrobbles = sum(scrobbles)) %>% 
  ggplot(aes(reorder(album,scrobbles), scrobbles)) + 
  geom_col() + 
  coord_flip()


  
