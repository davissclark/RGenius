################################################################################
#' General song information
#' Get song with provided ID
#'
#' @param id the requested song ID
#'
#' @param ... extra parameters, see https://docs.genius.com/#songs-h2
#'
#' @param ctx the genius context object
#'
#' @return requested song
get.song <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("songs", id, sep = "/"))
    content(GET(url,token))
}

get.song.media <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("songs", id, sep = "/"))
    content(GET(url,token))$response$song$media
}

get.song.stats <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("songs", id, sep = "/"))
    content(GET(url,token))$response$song$stats
}

annotation.count <- function(id, token) {
    stats <- get.song.stats(id, token)
    stats$accepted_annotations + stats$unreviewed_annotations + stats$verified_annotations
}

################################################################################

get.annotation.summary <- function(token, song_id) {
    refs <- get.referent(token, song_id, "song", NULL, 50, 1)$response$referents
    if(length(refs)>0) {
    
        c <- vector(length = length(refs))
        w <- vector(length = length(refs))
        v <- vector(length = length(refs))
    
        for(r in 1:length(refs)){
            c[r] <- length(refs[[r]]$annotations[[1]]$authors)
            w[r] <- length(unlist(strsplit(refs[[r]]$annotations[[1]]$body$plain, split=" ")))
            v[r] <- refs[[r]]$annotations[[1]]$votes_total
        }
    
        data.frame(author.count = c, word.count = w, votes = v)
    } else {
        NULL
    }
}

get.song.metrics <- function(songs, token) {
    
    idx <- vector(length = length(songs))
    song.id <- vector(length = length(songs)) 
    title <- vector(length = length(songs))
    full.title <- vector(length = length(songs))
    artist.id <- vector(length = length(songs))
    artist.name <- vector(length = length(songs))
    artist.iq <- vector(length = length(songs))
    pyongs.count <- vector(length = length(songs))
    release.date <- vector(length = length(songs))
    pageviews <- vector(length = length(songs))
    concurrents <- vector(length = length(songs))
    contributors <- vector(length = length(songs))
    iqearners <- vector(length = length(songs))
    transcribers <- vector(length = length(songs))
    accepted.annotations <- vector(length = length(songs))
    unreviewed.annotations <- vector(length = length(songs))
    verified.annotations <- vector(length = length(songs))
    album.id <- vector(length = length(songs))
    album.name <- vector(length = length(songs))
    mean.votes <- vector(length = length(songs))
    median.votes <- vector(length = length(songs))
    mean.word.count <- vector(length = length(songs))
    median.word.count <- vector(length = length(songs))
    
    for(i in 1:length(songs)) {
        song <- get.song(songs[i], token)$response$song
        
        idx[i] <- i
        
        if(!(length(song$id)>0)) {
            next
        } else {
            song.id[i] <- ifelse(length(song$id)>0, song$id, NA)
            title[i] <- ifelse(length(song$title)>0, song$title, NA)
            full.title[i] <- ifelse(length(song$full_title)>0, song$full_title, NA)
            artist.id[i] <- ifelse(length(song$primary_artist$id)>0, song$primary_artist$id, NA)
            artist.name[i] <- ifelse(length(song$primary_artist$name)>0, song$primary_artist$name, NA)
            artist.iq[i] <- ifelse(length(song$primary_artist$iq)>0, song$primary_artist$iq, NA)
            pyongs.count[i] <- ifelse(length(song$pyongs_count)>0, song$pyongs_count, NA)
            release.date[i] <- ifelse(length(song$release_date)>0, song$release_date, NA)
            pageviews[i] <- ifelse(length(song$stats$pageviews)>0, song$stats$pageviews, NA)
            concurrents[i] <- ifelse(length(song$stats$concurrents)>0, song$stats$concurrents, NA)
            contributors[i] <- ifelse(length(song$stats$contributors)>0, song$stats$contributors, NA)
            iqearners[i] <- ifelse(length(song$stats$iq_earners)>0, song$stats$iq_earners, NA)
            transcribers[i] <- ifelse(length(song$stats$transcribers)>0, song$stats$transcribers, NA)
            accepted.annotations[i] <- ifelse(length(song$stats$accepted_annotations)>0, song$stats$accepted_annotations, NA)
            unreviewed.annotations[i] <-ifelse(length(song$stats$unreviewed_annotations)>0, song$stats$unreviewed_annotations, NA)
            verified.annotations[i] <- ifelse(length(song$stats$verified_annotations)>0, song$stats$verified_annotations, NA)
            album.id[i] <- ifelse(length(song$album$id)>0, song$album$id, NA)
            album.name[i] <- ifelse(length(song$album$name)>0, song$album$name, NA)
        
            d <- get.annotation.summary(token, songs[i])
        
            if(!is.null(d)) {
                mean.votes[i] <- mean(d$votes, na.rm = TRUE)
                median.votes[i] <- median(d$votes, na.rm = TRUE)
                mean.word.count[i] <- mean(d$word.count, na.rm = TRUE)
                median.word.count[i] <- median(d$word.count, na.rm = TRUE)
            } else {
                mean.votes[i] <- NA
                median.votes[i] <- NA
                mean.word.count[i] <- NA
                median.word.count[i] <- NA
            }
        }
    }
    data.frame(idx = idx, 
               song.id = song.id,
               title = title,
               full.title = full.title,
               artist.id = artist.id,
               artist.name = artist.name, 
               artist.iq = artist.iq,
               pyongs.count = pyongs.count,
               release.date = release.date,
               pageviews = pageviews,
               concurrents = concurrents, 
               contributors = contributors, 
               iqearners = iqearners,               
               transcribers = transcribers, 
               accepted.annotations = accepted.annotations, 
               unreviewed.annotations = unreviewed.annotations, 
               verified.annotations = verified.annotations, 
               album.id = album.id, 
               album.name = album.name,
               mean.votes = mean.votes,
               median.votes = median.votes,
               mean.word.count = mean.word.count,
               median.word.count = median.word.count
               )
}