#' Search Genius
#' @template srch
#' @examples \dontrun{
#' search.repositories("tetris language:assembly")
#' }
search <- function(q, token, base_url="https://api.genius.com") {
    query=URLencode(paste("q=", q, sep = ""))
    url <- modify_url(base_url, path = "search")
    content(GET(url, token, query=query))
}
################################################################################