################################################################################
#' General referent information
#'
#' Get referent with provided ID
#'
#' @param ... extra parameters, created_by_id, song_id, web_page_id, see https://docs.genius.com/#referents-h2
#'
#' @param ctx the genius context object
#'
#' @return requested referent
get.referent <- function(token,
                         id = NULL,  
                         type = NULL, 
                         user_id = NULL,
                         per_page = NULL, 
                         page = NULL, 
                         text_format = "plain",
                         base_url="https://api.genius.com") 
    {
    
    if(is.null(id) && is.null(user_id)) {
        
        "Error: no ID provided"
        
    } else {
        
        query <- list()
        
        if(!is.null(user_id)) {
            query$created_by_id <- user_id
        }
        
        if(!is.null(type)) {
            if(type == "song") {
                query$song_id <- id
            } else if(type == "web") {
                query$web_page_id <- id
            }
        }
        
        query$text_format <- text_format
        
        if(!is.null(per_page)) {
            query$per_page <- per_page
        }
        
        if(!is.null(page)) {
            query$page <- page
        }
        
        url <- modify_url(base_url, path = "referents", query = query)
        content(GET(url, token))
    }
}
################################################################################

total.user.referents <- function(token, user_id = me) {
    
    idx <- vector(length = 704)
    id <- vector(length = 704)
    song_id <- vector(length = 704)
    type <- vector(length = 704)
    classification <- vector(length = 704)
    title <- vector(length = 704)
    annotation.count <- vector(length = 704)
    referent.word.count <- vector(length = 704)
    annotation.word.count <- vector(length = 704)
    # author.count <- vector(length = 704)
    # attribution <- vector(length = 704)
    votes <- vector(length = 704)
    state <- vector(length = 704)
    comment.count <- vector(length = 704)
    
    # idx <- vector(length = 100)
    # id <- vector(length = 100)
    # song_id <- vector(length = 100)
    # type <- vector(length = 100)
    # classification <- vector(length = 100)
    # title <- vector(length = 100)
    # annotation.count <- vector(length = 100)
    # referent.word.count <- vector(length = 100)
    # annotation.word.count <- vector(length = 100)
    # votes <- vector(length = 100)
    # state <- vector(length = 100)
    # comment.count <- vector(length = 100)
    
    call <- expression(get.referent(token = token, user_id = user_id, per_page = 50, page = i)$response$referents)
    i <- 1
    l <- 0
    while(length(eval(call)) > 0) {
    # while(i < 3) {
        content <- eval(call)
        offset <- 50 * (i - 1)
        n <- length(eval(call))
        l <- l + n
        for(num in 1:n) {
            index <- num + offset
            
            idx[index] <- index
            id[index] <- ifelse(length(content[[num]]$id)>0, content[[num]]$id, NA)
            song_id[index] <- ifelse(length(content[[num]]$song_id)>0, content[[num]]$song_id, NA) 
            type[index] <- ifelse(length(content[[num]]$annotatable$type)>0, content[[num]]$annotatable$type, NA) 
            classification[index] <- ifelse(length(content[[num]]$classification)>0, content[[num]]$classification, NA) 
            title[index] <- ifelse(length(content[[num]]$annotatable$title)>0, content[[num]]$annotatable$title, NA)
            
            annotation.count[index] <- ifelse(length(content[[num]]$annotations)>0, length(content[[num]]$annotations), 0)
            referent.word.count[index] <- ifelse(length(content[[num]]$fragment)>0, length(unlist(strsplit(content[[num]]$fragment, split=" "))), NA)
            annotation.word.count[index] <- ifelse(length(content[[num]]$annotations[[1]]$body$plain)>0, length(unlist(strsplit(content[[num]]$annotations[[1]]$body$plain, split=" "))), NA)
            # author.count <- ifelse(length(content[[num]]$annotations[[1]]$authors)>0, length(content[[num]]$annotations[[1]]$authors), NA)
            # attribution <- ifelse(length(content[[num]]$annotatable$title)>0, content[[num]]$annotatable$title, NA)
            votes[index] <- ifelse(length(content[[num]]$annotations[[1]]$votes_total)>0, content[[num]]$annotations[[1]]$votes_total, NA)
            state[index] <- ifelse(length(content[[num]]$annotations[[1]]$state)>0, content[[num]]$annotations[[1]]$state, NA)
            comment.count[index] <- ifelse(length(content[[num]]$annotations[[1]]$comment_count)>0, content[[num]]$annotations[[1]]$comment_count, NA)
        }
        i <- i + 1
    }
    df <- data.frame(idx = idx, id = id, song_id = song_id, type = type, classification = classification, title = title, annotation.count = annotation.count, referent.word.count = referent.word.count, annotation.word.count = annotation.word.count, votes = votes, state = state, comment.count = comment.count)
    df
}
