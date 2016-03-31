################################################################################
#' General account information
#' Account information includes general contact information and Genius-specific details about a user.
#'
#' Get account information for the currently authenticated user.
#'
#' @param ... extra parameters, see https://docs.genius.com/#account-h2
#'
#' @param ctx the genius context object
#'
#' @return Information about the user's account
get.account <- function(token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = "account")
    c <- content(GET(url,token))
    if(c$meta$status == 200) {
        c$response
    } else {
        "Error"
    }
}

get.user <- function(token, id=421685, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("users", id, sep = "/"))
    c <- content(GET(url,token))
    if(c$meta$status == 200) {
        c$response
    } else {
        "Error"
    }
}
################################################################################
get.account.avatar <- function(token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = "account")
    c <- content(GET(url,token))
    if(c$meta$status == 200) {
        c$response$user$avatar
    } else {
        "Error"
    }
}