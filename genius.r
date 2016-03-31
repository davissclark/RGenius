#' @title genius-package: use the Genius API from R
#' 
#' @description This package wraps the Genius web service API so you can make R
#'   calls against the Genius API
#'   
#' @author Davis Clark
#' @docType package
#' @name genius
#' @aliases genius
#' @keywords package genius-package
#' @examples
#' \dontrun{get.user.repositories("cscheid")}
#' @seealso \code{jsonlite}
NULL 

.state <- new.env(parent=emptyenv())

#' Obtain a genius context interactively
#' 
#' interactive.login opens a web browser, asks for your username+password,
#' performs the OAuth dance, retrieves the token, and uses it to create a genius
#' context.
#'
#' If you use rgenius interactively, then you will get a default
#' client ID and client secret that you can use. Please don't abuse that,
#' or the feature might need to be removed.
#' 
#' Refer to https://docs.genius.com/#/getting-started-h1
#' 
#' @param client_id the genius client ID
#'   
#' @param client_secret the genius client secret
#'   
#' @param scopes the OAuth scopes you want to request
#'   
#' @param base_url the base URL for the genius webpage.
#'   
#' @param api_url the base URL for the genius API.
#'   
#' @param max_etags the maximum number of entries to cache in the context
#'   
#' @param verbose logical, passed to \code{create.genius.context} and,
#'   ultimately, to httr configuration
#'   
#' @return a genius context object that is used in every genius API call issued
#'   by this library.
genius.connect <- function(client_id = NULL,
                              client_secret = NULL,
                              scopes = c("me","create_annotation", "manage_annotation", "vote"),
                              auth_url = "https://api.genius.com/oauth",
                              max_etags = 10000,
                              verbose = FALSE)
{
    require(httr)
    require(stringr)
    if (is.null(client_id) && is.null(client_secret) && interactive()) {
        client_id <- "635wyr-wqYp194T5sExzHyYFudP5PetkCDsvrpT_jAOpBetMSgXxkgzb7FIQHgv1"
        client_secret <- "v3DYAo-xBdml1z6qzsulEEWB0FKg-BWGyAOig9N_eGSaptcWcdqQEz6ypAWmH70LL9MuobZCRFPKoUfBtSX7QA"
    }
    genius <- oauth_endpoint(NULL, "authorize", "token",
                             base_url = auth_url)
    # as in httr, if client_secret is not given,
    # the environment variable GENIUS_CONSUMER_SECRET will be
    # used.
    app <- oauth_app("genius", client_id, client_secret)
    genius_token <- oauth2.0_token(genius, app, scope=scopes)
    # create.genius.context(base_url, client_id, client_secret, genius_token, verbose=verbose)
    genius_token
}

#' Create a genius context object.
#'
#' If create.genius.context is called without some of client_id, client_secret
#' or access_token, then some API calls will be unavailable, and more severe
#' rate limiting will be in effect. 
#'
#' If the environment variable GENIUS_PAT is set, then rgenius will attempt
#' to authenticate using the value of that variable as a personal authentication
#' token.
#'
#' create.genius.context stores the context last created in an environment.
#' If any of the genius API functions are called without a context, this
#' context is used instead. (if no context has been created, an unauthenticated
#' context will be created)
#'
#' @param api_url the base URL
#'
#' @param client_id the genius client ID
#'
#' @param client_secret the genius client secret
#'
#' @param access_token the genius access token
#'
#' @param personal_token the personal access token given by genius via the /authorizations api
#'
#' @param max_etags the maximum number of entries to cache in the context
#'
#' @param verbose if TRUE, passes verbose() to httr configuration
#'
#' @return a genius context object that is used in every genius API call
#'   issued by this library.
create.genius.context <- function(base_url = "https://api.genius.com/oauth", client_id = NULL,
                                  client_secret = NULL, access_token = NULL,
                                  max_etags = 10000, verbose = FALSE)
{
    ctx <- list(base_url       = base_url,
                client_secret  = client_secret,
                token          = access_token,
                client_id      = client_id,
                max_etags      = max_etags,
                etags          = new.env(parent = emptyenv()),
                authenticated  = !is.null(access_token),
                verbose        = verbose)
    if (!is.null(access_token)) {
        r <- get.account(ctx)
        if (!r$ok) {
            if (!is.null(access_token))
                stop("invalid access_token.")
            stop("internal error, shouldn't have gotten here")
        }
        ctx$user <- r$content
        ctx$oath_scopes <- r$headers$`x-oauth-scopes`
    }
    class(ctx) <- "geniuscontext"
    .state$ctx <- ctx
    ctx
}

#' returns the most recently created genius context, or creates one if none has been so far created
#'
#' @return a genius context object
get.genius.context <- function()
{
    if (is.null(.state$ctx))
        create.genius.context()
    .state$ctx
}

.build.url <- function(ctx, resource, params)
{
    # FIXME this path needs sanitization (some names can't include
    # slashes, etc) NB if you ever fix this, the *.reference calls in
    # data.R will need attention, since reference include slashes that
    # are passed unescaped to the github API
    
    query <- params
    if (!is.null(ctx$client_id))
        query$client_id <- ctx$client_id
    if (!is.null(ctx$client_secret))
        query$client_secret <- ctx$client_secret
    
    # we cannot use modify_url directly, because it doesn't merge paths
    # so we have to do that by hand
    api.path <- parse_url(ctx$api_url)$path
    if (isTRUE(nzchar(api.path)))
        path <- gsub('//+', '/', paste(api.path, resource, sep = '/'))
    else
        path <- resource
    
    if (is.null(ctx$token) && is.null(ctx$personal_token))
        return(list(url = modify_url(ctx$api_url, path = path, query = query),
                    config = c()))
    
    if (!is.null(ctx$personal_token))
        return(list(url = modify_url(ctx$api_url, path = path, query = query),
                    config = authenticate(ctx$personal_token, "x-oauth-basic", type = "basic")))
    
    # from here on out, ctx$token is not null
    
    # FIXME this is ugly: we use httr's refclass for the interactive flow, but a string for the non-interactive flow...
    if (!is.null(tryCatch(ctx$token$sign, error=function(cond) { NULL }))) {
        # we have sign: this came from the interactive flow...
        result <- modify_url(ctx$api_url, path = path, query = query)
        result <- ctx$token$sign(url = result)
        result <- result$url
        return(list(url = result, config = c()))
    } else {
        # we don't have sign: this came from the non-interactive flow.
        query$access_token <- ctx$token
        result <- modify_url(ctx$api_url, path = path, query = query)
        return(list(url = result, config = c()))
    }
}

.cached.api.request <- function(ctx, req, method, expect.code = 200,
                                params = list(), config = accept_json())
{
    resource <- str_c(req, collapse = '/')
    etags <- ctx$etags
    if (exists(resource, etags)) {
        cache <- get(resource, etags)
        tag <- cache$tag
        r <- .api.request(ctx, req, method, expect.code = c(304, expect.code),
                          params = params, config = c(add_headers(`If-None-Match`=tag), config))
        if (r$code == 304) {
            r$content <- cache$content
        }
    } else {
        r <- .api.request(ctx, req, method, expect.code = expect.code,
                          params = params, config = config)
    }
    if (r$code != 304) {
        assign(resource, list(tag = r$headers$ETag, content = r$content), etags)
    }
    
    # if etags environment is too large, we need to trim it.  but this
    # requires a traversal over the entire data structure, which is O(n)
    # so we only want this to happen once every O(n) operations to get
    # O(1) amortized time, and so we need to trim a constant fraction of
    # the elements at once. we get rid of half of them.
    #
    # We choose the entries to trim randomly.
    if (length(etags) > ctx$max_etags) {
        l <- as.list(etags)
        names_to_remove <- names(sample(as.list(etags), as.integer(length(etags)/2)))
        print(names_to_remove)
        print(names(as.list(etags)))
        rm(list=names_to_remove, envir=etags)
    }
    r
}

.api.request <- function(ctx, req, method, expect.code = 200,
                         params = list(), config = accept_json(), body = NULL)
{
    resource <- str_c(req, collapse = '/')
    lst <- .build.url(ctx, resource, params)
    url <- lst$url
    config <- c(config, lst$config)
    config <- c(config, user_agent(getOption("HTTPUserAgent")))
    if (ctx$verbose)
        config <- c(config, verbose())
    
    r <- method(url = url, config = config, body = body)
    result <- tryCatch(content(r),
                       error = function(e) {
                           raw <- r$content
                           raw[raw>127] <- as.raw(63)
                           r$content <- raw
                           content(r)
                       })
    output <-
        list(ok = r$status_code %in% expect.code, content = result, headers = r$headers,
             code = r$status_code)
    ## class(output) <- "genius"
    output
}

.without.body <- function(method)
{
    function(url, config, body) { method(url, config = config) }
}

.with.body <- function(method) {
    function(url, config, body) {
        if (is.list(body)) {
            body <- toJSON(body, auto_unbox=TRUE, null="null")
            # config = c(config, add_headers(`Content-Type` = "application/json; charset=utf-8"))
        }
        else if (is.character(body))
            stopifnot(length(body) == 1)
        else
            stopifnot(is.null(body))
        method(url, config = config, body = body)
    }
}

.api.get.request    <- function(ctx, req, expect.code = 200, params = list(), config = accept_json())       .cached.api.request(ctx, req, .without.body(GET),    expect.code, params, config)
.api.delete.request <- function(ctx, req, expect.code = 204, params = list(), config = accept_json())              .api.request(ctx, req, .without.body(DELETE), expect.code, params, config)
.api.put.request    <- function(ctx, req, expect.code = 200, params = list(), config = accept_json(), body = NULL) .api.request(ctx, req, .with.body(PUT),       expect.code, params, config, body)
.api.patch.request  <- function(ctx, req, expect.code = 200, params = list(), config = accept_json(), body = NULL) .api.request(ctx, req, .with.body(PATCH),     expect.code, params, config, body)
.api.post.request   <- function(ctx, req, expect.code = 201, params = list(), config = accept_json(), body = NULL) .api.request(ctx, req, .with.body(POST),      expect.code, params, config, body)

.api.test.request <- function(ctx, path)
{
    r=.api.get.request(ctx, path, expect.code = c(204, 404))
    
    if(r$ok)
        list(ok = TRUE, content = r$code == 204)
    else
        list(ok = FALSE, content = content(r$response))
}