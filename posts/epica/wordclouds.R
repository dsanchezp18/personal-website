# Get the genius artist id for Epica using their artist search function

epica_id <-
  search_artist('epica') %>% 
  select(artist_id) %>% 
  as.vector()

# Look for a track in the album to get the Genius album id

cry_for_the_moon_id <-
  search_song('cry for the moon') %>% 
  slice(1) %>% 
  select(song_id) %>% 
  as.vector()

# Get data for cry for the moon

cry_for_the_moon_df <-
  get_song_df(cry_for_the_moon_id)

# Get the album id for The Phantom Agony in the data of Cry for the Moon

the_phantom_agony_id <-
  cry_for_the_moon_df %>% 
  select(album_id) %>% 
  as.vector()

# Get all songs in the album and their ids

the_phantom_agony_songs  <- 
  get_album_tracklist_id(the_phantom_agony_id) %>% 
  inner_join(get_artist_songs_df(epica_id) %>% 
               select(song_name, song_id), 
             by = c('song_title' = 'song_name'))

track_ids <-
  the_phantom_agony_songs$song_id

# Extract lyrics for all tracks and combine into a single dataframe

phantom_agony_lyrics <-
  lapply(track_ids, get_lyrics_id) %>% 
  bind_rows() %>%
  slice(-1:-2)

# Tokenize the lyrics and antijoin with stop words

phantom_agony_words <-
  phantom_agony_lyrics %>% 
  unnest_tokens(output = word,
                input = line) %>% 
  anti_join(stop_words, by = 'word')

# A word count graph

phantom_agony_wordcount <-
  phantom_agony_words %>% 
  count(word) %>% 
  rename(freq = 'n') %>% 
  arrange(desc(freq))

word_count_pa <-
  phantom_agony_wordcount %>% 
  top_n(15) %>% 
  ggplot(aes(reorder(word, freq), freq)) +
  geom_col(fill = '#742014') +
  coord_flip() +
  geom_text(aes(label = freq), hjust = -0.5) +
  labs(title = "Most common words in Epica's The Phantom Agony",
       x = '',
       y = '') + 
  theme_daniel
