# Rscript to load all the required packages for the ShinyApp
#====================================================================================================================

# Define the function for artist search query
find_artist <- function(artist) {
  artist_search_results <- search_spotify(artist, type="artist")
  return(artist_search_results)
}

# Define a function to read and prepare data
read_data_function <- function(artist_names) {
  #Get the data of the artists included in artist_names and store them in a list
  df <- lapply(artist_names, function(x) get_artist_audio_features(x))
  
  # Release year to join to other summaries
  df_album_release_years <- lapply(df, function(x){
    x %>% group_by(album_name) %>% 
      summarise(release_year=first(album_release_year), track_count=n_distinct(track_name), album_image=first(album_images)) %>% 
      mutate(album_name_yr=str_c(album_name, " (", release_year, ")"))
  })
  
  return(list(df=df, df_album_release_years=df_album_release_years))
}

# Define a function to get artist id
get_artist_id <- function(df) {
  artist_id_df <- mapply(x=df, function(x){
    x %>% select(artist_name, artist_id) %>% summarise(artist_name=first(artist_name), artist_id=first(artist_id))
  })
  artist_id <- unnest(as_tibble(t(artist_id_df)), cols=attr(artist_id_df, 'dimnames')[[1]])
  
  return(artist_id)
}

# Define a function to read artist data
read_artist_data_function <- function(artist_id){
  df_artist <- lapply(artist_id$artist_id, function(x) get_artists(x))
}

# Define a function to extract artist data
df_artist_summary <- function(df_artist){
  df_artist_summary <- mapply(x=df_artist, function(x){
    x %>% mutate_if(is.character, as.factor)
  })
  artist_summary <- unnest(as_tibble(t(df_artist_summary)), cols=attr(df_artist_summary, 'dimnames')[[1]])
  artist_summary$genres[!lengths(artist_summary$genres) > 0] <- ''
  
  return(artist_summary)
}

# Define a function to extract popularity of artist
df_artist_popularity <- function(artist_summary, text_color_df){
  artist_popularity <- artist_summary %>% select(artist_name=name, popularity) %>% 
    group_by(artist_name) %>% 
    left_join(text_color_df)
  
  return(artist_popularity)
}

# Define a function to extract artist followers
df_artist_followers <- function(artist_summary, text_color_df){
  artist_followers <- artist_summary %>% select(artist_name=name, followers=followers.total) %>% 
    group_by(artist_name) %>%
    left_join(text_color_df)
  
  return(artist_followers)
}

artist_popularity_text <- function(artist_popularity, artist_followers) {
  text_df <- data.frame(text=c())
  
  for (i in 1:nrow(artist_popularity)) {
    popularity <- artist_popularity[i, ]
    followers <- artist_followers[i, ]

    text <- paste0(with(followers, paste0("<span style=\"color: ", color, ";\">", artist_name, "</span>",
                                           " has a follower count of ", format(followers, big.mark=",", scientific=FALSE))),
                   with(popularity, paste0(" which relates to their popularity score of ", popularity, ".")))
    text_df[i, "text"] <- text
  }
  
  return(text_df)
}

# Define a function to extract genres of artist
df_artist_genres <- function(artist_summary, text_color_df){
  artist_genres <- artist_summary %>% select(artist_name=name, genres) %>% unnest(cols=c(genres)) %>% 
    group_by(artist_name) %>% mutate(key = paste0("genre",row_number())) %>% spread(key, genres) %>%
    left_join(text_color_df)
  
  return(artist_genres)
}

# Define a function to create text from genres of artist
artist_genre_text <- function(artist_genres){
  text_df <- data.frame(text=c())
  
  for (i in 1:nrow(artist_genres)){
    genre <- artist_genres[i, ]
    genre <- genre[, colSums(is.na(genre)) == 0]
    
    if (genre$genre1 == ""){
      text <- with(genre, paste0("<span style=\"color: ", color, ";\">", artist_name, "</span>",
                                 " does not have any listed genres."))
      text_df[i, "text"] <- text
    } else if (!has_name(genre, "genre2")) {
      text <- with(genre, paste0("<span style=\"color: ", color, ";\">", artist_name, "</span>",
                                 " is mostly characterized as ", genre1, "."))
      text_df[i, "text"] <- text
    } else if (!has_name(genre, "genre3")) {
      text <- with(genre, paste0("<span style=\"color: ", color, ";\">", artist_name, "</span>",
                                 " is mostly characterized as ", genre1, ", and ", genre2, "."))
      text_df[i, "text"] <- text
    } else {
      text <- with(genre, paste0("<span style=\"color: ", color, ";\">", artist_name, "</span>",
                                 " is mostly characterized as ", genre1, ", ", genre2, ", and ", genre3, "."))
      text_df[i, "text"] <- text
    }
  }
  
  return(text_df)
}

# Define the function to generate the data for all valence and energy plots
df_valence_energy_function <- function(df, df_album_release_years){
  df_valence_energy <- mapply(x=df, y=df_album_release_years, function(x, y){
    x %>% select(artist_name, album_name, track_name, valence, energy) %>%
      left_join(y) %>% 
      mutate_if(is.character, as.factor) %>% 
      mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year)))
  })
  #Combine the valence and energy data of all the artist into a single dataframe
  valence_energy <- unnest(as_tibble(t(df_valence_energy)), cols=attr(df_valence_energy, 'dimnames')[[1]]) %>% 
    mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year)))
  
  album_image <- valence_energy %>% select(album_image) %>% unnest(cols=c(album_image)) %>% filter(height==64) %>% select(url)
  
  valence_energy$album_image2 <- album_image$url

  return(valence_energy)
}

# Define the function to generate the data of median valence and energy values
median_valence_energy_function <- function(df_valence_energy){
  median_df <- df_valence_energy %>% select(artist_name, valence, energy) %>% 
    group_by(artist_name) %>% summarise(medianValence = median(valence), medianEnergy = median(energy))
  
  return(median_df)
}

# Define the function to generate the valence and energy plot
valence_energy_plot <- function(df_valence_energy, plot_colors){
  # Determine the valence and energy per artist
  centroids_df <- df_valence_energy %>% select(artist_name, valence, energy) %>% 
    nest(data=c(valence, energy)) %>% mutate(model = map(data, kmeans, 1),
                                     centers = map(model, broom::tidy)) %>%
    unnest(centers) %>% select(artist_name, x1, x2) %>% rename(valence=x1, energy=x2)
  
  #Make Plot of the valence and energy per track per artist
  plotline <- list(color = "#000000",
                   width = 2,
                   value = 0.5)
  
  plot_valence_energy <- highchart() %>% 
    hc_add_series(df_valence_energy, "scatter", hcaes(x=valence, y=energy, group=artist_name), showInLegend = TRUE) %>% 
    hc_add_series(centroids_df, "bubble", name="centroids", hcaes(x=valence, y=energy, group=artist_name, size=2), showInLegend = FALSE) %>%
    hc_colors(colors =  hex_to_rgba(plot_colors$color, alpha=0.7)) %>% 
    hc_yAxis(title = list(text = "Energy"), plotLines=list(plotline), min=0, max=1) %>% 
    hc_xAxis(title = list(text = "Valence"), plotLines=list(plotline), min=0, max=1) %>% 
    hc_tooltip(formatter = JS("function () { if (this.series.name=='centroids'){
                                                  return 'Centroid Valence: ' + this.point.valence + '<br/>' +
                                                         'Centroid Energy: ' + this.point.energy
                                                } else {
                                                return '<table><tr><td style=\"padding:5px\">' +
                                                       '<img src=\"' + this.point.album_image2 + '\" width=64>' +
                                                       '</td><td>' +
                                                       'Album Name: ' + this.point.album_name + '<br/>' +
                                                       'Track Name: ' + this.point.track_name + '<br/>' +
                                                       'Valence: ' + this.point.valence + '<br/>' +
                                                       'Energy: ' + this.point.energy +
                                                       '</td></tr></table>'
                                                } ;}"),
               useHTML=TRUE) %>%

    hc_annotations(list(
      labels=list(
        list(point = list(x = 0.05, y = 0.95, xAxis = 0, yAxis = 0),
             text = "Turbulent/Angry", style=list(fontWeight='bold', fontSize="14px")),
        list(point = list(x = 0.05, y = 0, xAxis = 0, yAxis = 0),
             text = "Sad/Depressing", style=list(fontWeight='bold', fontSize="14px")),
        list(point = list(x = 0.95, y = 0.95, xAxis = 0, yAxis = 0),
             text = "Happy/Joyful", style=list(fontWeight='bold', fontSize="14px")),
        list(point = list(x = 0.95, y = 0, xAxis = 0, yAxis = 0),
             text = "Chill/Peaceful", style=list(fontWeight='bold', fontSize="14px"))
      ), draggable=FALSE, labelOptions=list(backgroundColor="rgba(255,255,255, 0.5)", borderWidth=0, strokeWidth=4)
    ))
  
  return(plot_valence_energy)  
}

# Define the function to generate the valence over time plot
valence_time_plot <- function(df_valence_energy, plot_colors){
  valence_time_plot <- highchart() %>% 
    hc_add_series(df_valence_energy, "column", hcaes(x=release_year, y=valence, group=artist_name),
                  tooltip = list(pointFormat = "Album Name: {point.album_name},<br/>
                                               Valence: {point.valence}"),
                  pointRange=1, pointPadding=0.2, borderColor="") %>% 
    hc_colors(colors =  hex_to_rgba(plot_colors$color, alpha=1)) %>% 
    hc_yAxis(title = list(text = "Valence")) %>% 
    hc_xAxis(title = list(text = "Year"), minPadding=0.05, maxPadding=0.05)
  return(valence_time_plot)
}

# Define the function to generate the energy over time plot
energy_time_plot <- function(df_valence_energy, plot_colors){
  energy_time_plot <- highchart() %>% 
    hc_add_series(df_valence_energy, "column", hcaes(x=release_year, y=energy, group=artist_name),
                  tooltip = list(pointFormat = "Album Name: {point.album_name},<br/>
                                               Energy: {point.energy}"),
                  pointRange=1, pointPadding=0.2, borderColor="") %>% 
    hc_colors(colors =  hex_to_rgba(plot_colors$color, alpha=1)) %>% 
    hc_yAxis(title = list(text = "Energy")) %>% 
    hc_xAxis(title = list(text = "Year"), minPadding=0.05, maxPadding=0.05)
  return(energy_time_plot)
}

# Define the function to generate the valence and energy density plots
valence_dens_plot <- function(df_valence_energy){
  median_df <- median_valence_energy_function(df_valence_energy)
  
  max_dens_valence <- df_valence_energy %>% group_by(artist_name) %>% summarise(dens=max(density(valence)$y)) %>% top_n(1) %>% pull("dens")
  artist_levels <- nlevels(df_valence_energy$artist_name)
  
  df_valence_energy <- df_valence_energy %>% rename(`Artist Name`=artist_name)
  
  valence_dens_plot <- ggplot() +
    geom_density(data=df_valence_energy, aes(x = valence, color=`Artist Name`, fill=`Artist Name`), alpha=0.3) +
    geom_vline(data=median_df, aes(xintercept=medianValence, color=artist_name), linetype="dashed", size=1.25, show.legend = FALSE) +
    theme_minimal_hgrid(12) +
    geom_text(data=median_df, aes(x=medianValence, y=max_dens_valence+0.25, label=medianValence, color=artist_name), show.legend=FALSE)  +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    xlab("Valence") + ylab("Density") + theme(legend.title=element_blank())
  
  valence_dens_plot <- ggplotly(valence_dens_plot, tooltip=c("fill", "x", "y")) %>% 
    style(showlegend = FALSE, traces=c(seq(from=artist_levels+1, to=artist_levels+artist_levels)))
  
  return(valence_dens_plot)
}

# Define the function to generate the energy density plot
energy_dens_plot <- function(df_valence_energy){
  median_df <- median_valence_energy_function(df_valence_energy)
  
  max_dens_energy <- df_valence_energy %>% group_by(artist_name) %>% summarise(dens=max(density(energy)$y)) %>% top_n(1) %>% pull("dens")
  artist_levels <- nlevels(df_valence_energy$artist_name)
  
  df_valence_energy <- df_valence_energy %>% rename(`Artist Name`=artist_name)
  
  energy_dens_plot <- ggplot() +
    geom_density(data=df_valence_energy, aes(x = energy, color=`Artist Name`, fill=`Artist Name`), alpha=0.3) +
    geom_vline(data=median_df, aes(xintercept=medianEnergy, color=artist_name), linetype="dashed", size=1.25, show.legend = FALSE) +
    theme_minimal_hgrid(12) +
    geom_text(data=median_df, aes(x=medianEnergy, y=max_dens_energy+0.25, label=medianEnergy, color=artist_name), show.legend=FALSE)  +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    xlab("Energy") + ylab("Density") + theme(legend.title=element_blank())
  
  energy_dens_plot <- ggplotly(energy_dens_plot, tooltip=c("fill", "x", "y")) %>% 
    style(showlegend = FALSE, traces=c(seq(from=artist_levels+1, to=artist_levels+artist_levels)))
  
  return(energy_dens_plot)
}

# Define the function to generate the data for all variety plots
df_variety_function <- function(df, df_album_release_years){
  # Determine the variety (from musical key) per album per artist
  df_variety <- mapply(x=df, y=df_album_release_years, function(x, y){
    x %>% group_by(album_name) %>% count(key_mode) %>% 
      left_join(y) %>% arrange(release_year, desc(n)) %>%
      ungroup() %>%
      mutate_if(is.character, as.factor) %>% 
      mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year))) %>% 
      group_by(album_name_yr) %>% 
      summarise(variety=n_distinct(key_mode), normalised_variety=variety/first(track_count), release_year=first(release_year)) %>% 
      mutate(artist_name=x$artist_name[1])
  })
  #Combine the variety data of all the artist into a single dataframe
  variety <- unnest(as_tibble(t(df_variety)), cols=attr(df_variety, 'dimnames')[[1]]) %>% 
    mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year)))
  
  return(variety)
}

# Define the function to generate the variety plot
variety_plot <- function(df_variety){
  #Make Plot of the albums over time
  plot_variety <- ggplot(df_variety, aes(x=album_name_yr, y=normalised_variety, fill=artist_name,
                                         text=paste("</br>Artist name: ", artist_name,
                                                    "</br>Album name: ", album_name_yr,
                                                    "</br>Normalised Variety: ", normalised_variety))) + geom_bar(stat="identity") + 
    coord_flip() + theme_minimal_hgrid(12) +
    scale_fill_viridis(discrete = TRUE) +
    xlab("Album Name") + ylab("Variety") + labs(fill = "Artist Name")
  
  plot_variety <- ggplotly(plot_variety, tooltip=c("text"))
  
  return(plot_variety)
}

# Define the function to generate the variety plot
text_plot_color <- function(artist_names, df_valence_energy) {
  text_color_df <- data.frame(artist_name=factor(artist_names, levels=levels(df_valence_energy$artist_name))) %>% 
    mutate(color=viridis(n=length(artist_names)))
  
  return(text_color_df)
}

# Define the function to generate the data for all tempo and loudness plots
df_tempo_loudness_function <- function(df, df_album_release_years){
  df_tempo_loudness <- mapply(x=df, y=df_album_release_years, function(x, y){
    x %>% select(artist_name, album_name, track_name, tempo, loudness) %>%
      left_join(y) %>% 
      mutate_if(is.character, as.factor) %>% 
      mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year)))
  })
  #Combine the valence and energy data of all the artist into a single dataframe
  tempo_loudness <- unnest(as_tibble(t(df_tempo_loudness)), cols=attr(df_tempo_loudness, 'dimnames')[[1]]) %>% 
    mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year)))
  
  return(tempo_loudness)
}

# Define the function to generate the data of median tempo and loudness values
median_tempo_loudness_function <- function(df_tempo_loudness){
  median_df <- df_tempo_loudness %>% select(artist_name, tempo, loudness) %>% 
    group_by(artist_name) %>% summarise(medianTempo = median(tempo), medianLoudness = median(loudness))
  
  return(median_df)
}

# Define the function to generate the tempo over time plot
tempo_time_plot <- function(df_tempo_loudness, plot_colors){
  tempo_time_plot <- highchart() %>% 
    hc_add_series(df_tempo_loudness, "column", hcaes(x=release_year, y=tempo, group=artist_name),
                  tooltip = list(pointFormat = "Album Name: {point.album_name},<br/>
                                               Tempo: {point.tempo}"),
                  pointRange=1, pointPadding=0.2, borderColor="") %>% 
    hc_colors(colors =  hex_to_rgba(plot_colors$color, alpha=1)) %>% 
    hc_yAxis(title = list(text = "Tempo (in BPM)")) %>% 
    hc_xAxis(title = list(text = "Year"), minPadding=0.05, maxPadding=0.05) %>% 
    hc_title(text="Tempo over the years")
  
  return(tempo_time_plot)
}


# Define the function to generate the loudness over time plot
loudness_time_plot <- function(df_tempo_loudness, plot_colors){
  loudness_time_plot <- highchart() %>% 
    hc_add_series(df_tempo_loudness, "column", hcaes(x=release_year, y=loudness, group=artist_name),
                  tooltip = list(pointFormat = "Album Name: {point.album_name},<br/>
                                               Loudness: {point.loudness}"),
                  pointRange=1, pointPadding=0.2, borderColor="") %>% 
    hc_colors(colors =  hex_to_rgba(plot_colors$color, alpha=1)) %>% 
    hc_yAxis(title = list(text = "Loudness")) %>% 
    hc_xAxis(title = list(text = "Year"), minPadding=0.05, maxPadding=0.05) %>% 
    hc_title(text="Loudness over the years")
  
  return(loudness_time_plot)
}

# Define the function to generate the tempo density plot
tempo_dens_plot <- function(df_tempo_loudness){
  median_df <- median_tempo_loudness_function(df_tempo_loudness)
  
  max_dens_tempo <- df_tempo_loudness %>% group_by(artist_name) %>% summarise(dens=max(density(tempo)$y)) %>% top_n(1) %>% pull("dens")
  artist_levels <- nlevels(df_tempo_loudness$artist_name)
  
  df_tempo_loudness <- df_tempo_loudness %>% rename(`Artist Name`=artist_name)
  
  tempo_dens_plot <- ggplot() +
    geom_density(data=df_tempo_loudness, aes(x = tempo, color=`Artist Name`, fill=`Artist Name`), alpha=0.3) +
    geom_vline(data=median_df, aes(xintercept=medianTempo, color=artist_name), linetype="dashed", size=1.25, show.legend = FALSE) +
    theme_minimal_hgrid(12) +
    geom_text(data=median_df, aes(x=medianTempo, y=max_dens_tempo+0.25, label=medianTempo, color=artist_name), show.legend=FALSE)  +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    xlab("Tempo (in BPM)") + ylab("Density") + theme(legend.title=element_blank())
  
  tempo_dens_plot <- ggplotly(tempo_dens_plot, tooltip=c("fill", "x", "y")) %>% 
    style(showlegend = FALSE, traces=c(seq(from=artist_levels+1, to=artist_levels+artist_levels)))
  
  return(tempo_dens_plot)
}

# Define the function to generate the loudness density plot
loudness_dens_plot <- function(df_tempo_loudness){
  median_df <- median_tempo_loudness_function(df_tempo_loudness)
  
  max_dens_loudness <- df_tempo_loudness %>% group_by(artist_name) %>% summarise(dens=max(density(loudness)$y)) %>% top_n(1) %>% pull("dens")
  artist_levels <- nlevels(df_tempo_loudness$artist_name)
  
  df_tempo_loudness <- df_tempo_loudness %>% rename(`Artist Name`=artist_name)
  
  loudness_dens_plot <- ggplot() +
    geom_density(data=df_tempo_loudness, aes(x = loudness, color=`Artist Name`, fill=`Artist Name`), alpha=0.3) +
    geom_vline(data=median_df, aes(xintercept=medianLoudness, color=artist_name), linetype="dashed", size=1.25, show.legend = FALSE) +
    theme_minimal_hgrid(12) +
    geom_text(data=median_df, aes(x=medianLoudness, y=max_dens_loudness+0.25, label=medianLoudness, color=artist_name), show.legend=FALSE)  +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    xlab("Loudness") + ylab("Density") + theme(legend.title=element_blank())
  
  loudness_dens_plot <- ggplotly(loudness_dens_plot, tooltip=c("fill", "x", "y")) %>% 
    style(showlegend = FALSE, traces=c(seq(from=artist_levels+1, to=artist_levels+artist_levels)))
  
  return(loudness_dens_plot)
}

#Functie die de contact chip UI's returned voor artiesten
artistchipUI <- function(artistname, n){
  
  # Get the image of the artist
  src <- get_artist(get_artist_audio_features(artistname)$artist_id[1])$images$url[3]
  # Generate the click function for the wikipedia of the artist
  wiki <- paste0('https://en.wikipedia.org/wiki/', artistname)
  
  # Generate the Contact Chip UI
  ui <- fluidRow(id=paste0("ui", n), 
                 # Set the spaces between the contact chips
                 div(style = paste0('padding-left:10px; padding-top:5px; padding-bottom:5px'),
                     # Create a block for the UI
                     div(style = paste0('display: inline-block;
                                        padding: 0 25px;
                                        height: 50px;
                                        font-size: 12px;
                                        line-height: 50px;
                                        border-radius: 25px;
                                        background-color: #f1f1f1;'),
                         # Include the picture of the artist
                         img(style = paste0('float: left;
                                              margin: 0 10px 0 -25px;
                                              height: 50px;
                                              width: 50px;
                                              border-radius: 50%;'),
                             src=src, 
                             alt="Person", width="80", height="80"),
                         # Create the information (wikipedia) button
                         a(artistname, href=wiki, class='wiki'),
                         span( 
                           # Create the remove button
                           actionButton(class = 'rmvbtn',
                                        inputId = paste0('ui_rmv', n), label = '', icon = icon('fas fa-trash-alt')),
                         )
                     ),
                     immediate = TRUE
                 )
  )
  return(list(ui = ui))
}
#====================================================================================================================

#Note that this script can be improved by:
# 1) Optimizing performance of functions
# 2) Naming conventions
# 3) Combining functions to a single function with an extra argument 
