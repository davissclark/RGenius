#' Get an artist
#'
#' @param id the artist id
#' 
#' @param ... extra parameters, see https://docs.genius.com/#artists-h2 for details.
#'
#' @param ctx the genius context object
#'
#' @return the artist information
get.artist <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("artists", id, sep = "/"))
    content(GET(url,token))
}
################################################################################
# songs

#' Documents (songs) for the artist specified. By default, 20 items are returned for each request.
#'
#' @param id the artist id
#'
#' @param ... extra parameters, see https://docs.genius.com/#artists-h2 for details.
#'
#' @param ctx the genius context object
#'
#' @return the song list
get.artist.songs <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("artists", id, "songs", sep = "/"))
    content(GET(url,token))
}
################################################################################