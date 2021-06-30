#' @importFrom purrr map discard
#' @importFrom magrittr %>%
#' @importFrom jsonlite toJSON unbox fromJSON
#' @importFrom methods is

NULL # break for roxygen2

#' send discord message
#' 
#' @description a discord message on a given channel id
#' 
#' @param message message content
#' @param channel_id id of the channel
#' @param bot an instance of DiscordrBot
#' 
#' @export
send_message = function(message, channel_id, bot){
  stopifnot(is(bot, "DiscordrBot"))
  
  path <- paste_url("channels", channel_id, "messages")
  body <- list(content=message)
  bot$discord_post_api(path, body)
}


#' send a discord embed
#' 
#' @param embed a discordr_embed object
#' @seealso [new_embed]
#'  
#' @param channel_id id of the channel where the embed should be sent
#' @param bot an instance of DiscordrBot
#' 
#' @export  
send_embed = function(embed, channel_id, bot){
  stopifnot(is(embed, "discordr_embed"), is(bot, "DiscordrBot"))
  
  path <- paste_url("channels", channel_id, "messages")
  body <- list(embeds=I(list(embed))) # I needed to preserve a 1 element array
  bot$discord_post_api(path, body)
}

#' Makes a discord embed
#' @description for more info check 
#' https://discord.com/developers/docs/resources/channel#embed-object-embed-structure
#' 
#' @param title title
#' @param type (will be deprecated by discord)
#' @param description description (main content)
#' @param url url
#' @param timestamp timestamp
#' @param color side color
#' @param footer footer
#' @param image list with image props
#' @param thumbnail thumbnail
#' @param video list with image props
#' @param provider provider
#' @param author author
#' @param fields list of fields
#' @return the embed object with class discordr_embed
#' @export
new_embed <- function(title=NULL, type="rich", description=NULL, url=NULL,
                      timestamp=NULL, color=NULL, footer=NULL, image=NULL,
                      thumbnail=NULL, video=NULL, provider=NULL, author=NULL,
                      fields=NULL){
  # The reason for this function is that all the   
  embed <- list(title=title, type=type, description=description, url=url,
                timestamp=timestamp, color=color, footer=footer, image=image,
                thumbnail=thumbnail, video=video, provider=provider, author=author,
                fields=fields) %>% 
    discard(is.null) %>% # removes removes fields that are null
    map(to_unboxed) # unbox elements to in json they won't be arrays
  class(embed) <- c("discordr_embed", "list")
  return(embed)
  
}

#' recursively unbox each element
#' @param el single element or list
to_unboxed <- function(el){
  # this is a list so need to unbox each element
  if (length(el) > 1 || is(el, "list")){
    map(el, to_unboxed)
  }
  else{
    unbox(el)
  }
}
