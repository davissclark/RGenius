#' An annotation is a piece of content about a part of a document. The document may be a song (hosted on Genius) or a web page (hosted anywhere). The part of a document that an annotation is attached to is called a referent.
#' 
#' Annotation data returned from the API includes both the substance of the annotation and the necessary information for displaying it in its original context.
#'
#' Data for a specific annotation.
#'
#' @param id annotation id
#' 
#' @param ... extra parameters, see https://docs.genius.com/#artists-h2 for details.
#'
#' @param ctx the githb context object
#'
#' @return the organization information
get.annotation <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("annotations", id, sep = "/"))
    content(GET(url,token))
}
#' Creates a new annotation on a public web page. The returned value will be the new annotation object, in the same form as would be returned by GET /annotation/:id with the new annotation's ID.
#' Requires scope: create_annotation
#'
#' @param content the JSON object describing the team. See http://developer.github.com/v3/orgs/teams/#create-team for details.
#'
#' @param ctx the github context object
#'
#' @return new annotation object, in the same form as would be returned by GET /annotation/:id with the new annotation's ID.
create.annotation <- function(c, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = "annotations")
    content(POST(url, token, body = c))
}
#' Updates an annotation created by the authenticated user. Accepts the same parameters as POST /annotation above.
#' Requires scope: manage_annotation
#'
#' @param id annotation id
#'
#' @param ctx the github context object
#'
#' @return none
update.annotation <- function(id, c, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("annotations", id, sep = "/"))
    content(PUT(url,token, body=c))
}
#' Deletes an annotation created by the authenticated user.
#' Requires scope: manage_annotation
#'
#' @param id annotation id
#'
#' @param ... extra parameters, see https://docs.genius.com/#artists-h2 for details.
#'
#' @param ctx the genius context object
#'
#' @return none
delete.annotation <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("annotations", id, sep = "/"))
    content(DELETE(url, token))
}
#' Votes positively for the annotation on behalf of the authenticated user.
#' Requires scope: vote
#' 
#' @param id annotation id
#'
#' @param ctx the github context object
#'
#' @return None
up.vote <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("annotations", id, "upvote", sep = "/"))
    content(PUT(url, token))
}
#' Removes the authenticated user's vote (up or down) for the annotation.
#' Requires scope: vote
#' 
#' @param id annotation id
#'
#' @param ctx the github context object
#'
#' @return None
un.vote <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("annotations", id, "unvote", sep = "/"))
    content(PUT(url, token))
}
#' Votes negatively for the annotation on behalf of the authenticated user.
#' Requires scope: vote
#' 
#' @param id annotation id
#'
#' @param ctx the github context object
#'
#' @return None
down.vote <- function(id, token, base_url="https://api.genius.com") {
    url <- modify_url(base_url, path = paste("annotations", id, "downvote", sep = "/"))
    content(PUT(url, token))
}
################################################################################