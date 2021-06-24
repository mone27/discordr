#' @importFrom bitops %<<%
#' @rawNamespace export(DiscordrBot)
#' @importFrom logging logdebug loginfo logwarn
#' @importFrom jsonlite toJSON fromJSON

### --- code to handle Discord websocket events

# OP code reference
OP_DISPATCH <- 0 # receiving normal events from Discord
OP_HB <- 1 # heartbeat
OP_IDENTIFY <- 2 # identify used at bot setup
# 3-4 are for updating the guild status, ignored for now
OP_RESUME <- 6 # to ask to resume session
OP_RECONNECT <- 7 # need to reconnected to ws
# 8 request guild member
OP_INVALID_SESSION <- 9 # session is invalid
OP_HELLO <- 10 #first message received
OP_HEARTBEAT_ACK <- 11 # acknowledge heartbeat


# Discord gateway intents for more info https://discord.com/developers/docs/topics/gateway#gateway-intents
# to use more than one intent you need to sum them
GUILDS <- 1 %<<% 0
GUILD_MEMBERS <- 1 %<<% 1  
GUILD_BANS <- 1 %<<% 2
GUILD_EMOJIS <- 1 %<<% 3
GUILD_INTEGRATIONS <- 1 %<<% 4
GUILD_WEBHOOKS <- 1 %<<% 5
GUILD_INVITES <- 1 %<<% 6
GUILD_VOICE_STATES <- 1 %<<% 7
GUILD_PRESENCES <- 1 %<<% 8
GUILD_MESSAGES <- 1 %<<% 9
GUILD_MESSAGE_REACTIONS <- 1 %<<% 10
GUILD_MESSAGE_TYPING <- 1 %<<% 11
DIRECT_MESSAGES <- 1 %<<% 12
DIRECT_MESSAGE_REACTIONS <- 1 %<<% 13
DIRECT_MESSAGE_TYPING <- 1 %<<% 14


### ---
base <- "https://discord.com/api"

#' Paste two url components
#' 
#' shorthand to call paste with sep="/", useful when creating url paths
#' 
#' @param ... elements to paste
#' 
#' @examples 
#' paste_url("rest", "api", "path")
#' 
#' @export
paste_url <- function(...){
  paste(..., sep="/")
}




#' @title Discordr Bot
#' 
#' @description this is the main class for the discord bot
#' it handles all the incoming messages and events
#' @export
DiscordrBot <- R6::R6Class("DiscordBot",
    public = list(
      #' Create a new bot
      #' the bot won't be started yet, but it is possible to register event handlers
      #' The users should ensure that the bot has the right permissions
      #' 
      #' @param token The discord bot token
      #' @param intent the intent(s) that the bot should access.
      #' Discordr defines constant for the possible intents 
      initialize = function(token, intent = GUILD_MESSAGES) {
        private$token = token
        private$intent = intent
        ### registers default handlers for event
        
        
        private$header <- httr::add_headers(Authorization = paste("Bot", token),
                                     "User-Agent" = "R-Discord-bot/0.1",
                                     "Accept" = "application/json")
        
        # first connection
        self$register_op_handler(OP_HELLO, private$handle_hello)
        
        
        # Heartbeat ACK, for now ignoring it
        self$register_op_handler(OP_HEARTBEAT_ACK, function(msg){})
        
        # respond with heartbeat if requested. Should be used rarely
        self$register_op_handler(OP_HB, private$send_heartbeat)
        
        # reestablish connection if case the websocket closes
        self$register_op_handler(OP_RECONNECT, private$handle_reconnect_request)
        self$register_op_handler(OP_INVALID_SESSION, private$handle_invalid_session)
        
        # Ignoring the signal that the resume is complete
        self$register_op_handler(OP_RESUME, function(msg){})
        
        # Handler of all normal events
        self$register_op_handler(OP_DISPATCH, private$event_switcher)
        
        self$register_event_handler("READY", private$handle_ready)
        
        invisible(self)
      },
      
      #'Start the bot
      #' 
      #' @description starts the bot and enters in an infinite later loop
      #' if in a interactive session returns as the later event loop will be run while R is idle
      start = function(){
        private$connect_ws()
        print("Started bot")
        # in interactive session returns control
        if (interactive()){return(invisible(private))}
        while(TRUE){
          # won't return until a later callback is executed
          later::run_now(timeoutSecs = Inf)
        }
      },
      
      #' @description register op handler
      #' 
      #' @param op one of the opcodes from the discord websocket
      #' https://discord.com/developers/docs/topics/opcodes-and-status-codes
      #' 
      #' @param callback a function that would be called with the event data
      register_op_handler = function(op, callback){
        stopifnot(is.function(callback))
        stopifnot(is.numeric(op)) # OP Codes must be integers
        private$op_handlers[[as.character(op)]] <- callback
        invisible(self)
      },
      #' register event handlers
      #' @param event_type one of the events from the discord websocket
      #' https://discord.com/developers/docs/topics/gateway#commands-and-events
      #' 
      #' @param callback a function that would be called with the event data
      register_event_handler = function(event_type, callback){
        stopifnot(is.function(callback))
        private$event_handlers[[event_type]] <- callback
        invisible(self)
      },
      
      #' send payload directly to the websocket
      #' @param opcode websocket OP CODE
      #' @param data list that represents data to be sent in payload 
      send_payload = function(opcode, data){
        payload <- toJSON(list(op=opcode, d=data), auto_unbox = T)
        logdebug("Sending payload on websocket")
        logdebug(payload)
        private$ws$send(payload)
      },
      
      ### - REST API
      
      #' Send discord message
      #' 
      #' sends a discord message on a given channel id
      #' 
      #' @param message message content
      #' @param channel_id id of the channel
      send_message = function(message, channel_id){
        path <- paste_url("channels", channel_id, "messages")
        body <- list(content=message)
        self$discord_post_api(path, body)
      },
      
      get_ws_endpoint = function(){
        res <- self$discord_get_api("gateway")
        url <- httr::content(res, "parsed")$url
        return(httr::modify_url(url, query=list(encoding = "json", v=9)))
      },
      
      #' general GET request to discord
      #' 
      #' @param path the path from the discord base api.
      discord_get_api = function(path){
        url <- paste_url(base, path)
        res <- httr::GET(
          url = url,
          config = private$header
        )
        httr::warn_for_status(res) # in case the request fails
        invisible(res)
      },
      
      #' general POST request to discord
      #' 
      #' @param path the path from the discord base api.
      #' 
      #' @param body a list that will the body of the http request.
      discord_post_api = function(path, body){
        url <- paste_url(base, path)
        res <- httr::POST(
          url = url,
          config = private$header,
          body = body,
          encode = "json"
        )
        httr::warn_for_status(res) # in case the request fails
        return(res) 
      }
      
    ),
    
    private = list(
      token = NULL,
      op_handlers = list(),
      event_handlers = list(),
      saved_ready = NULL, # individually fields should be saved here
      ws = NULL,
      first_connection = TRUE, # needed to know if send identify or resume on hello
      last_s = "Null", # keep track of last seq number sent by discord
      intent = NULL,
      header = NULL,
      
      #' create a new websocket and connect to it
      connect_ws = function(){
        # this makes an API call to the discord server, should be cached
        ws_endpoint <- self$get_ws_endpoint()
        # initialize websocket
        private$ws <- websocket::WebSocket$new(ws_endpoint, autoConnect = FALSE)
        private$ws$onMessage(function(event) {
          private$receiver_switcher(event$data)
        })
        private$ws$onClose(private$handle_ws_close)
        private$ws$onError(function(event) {
          logwarn("Client failed to connect: ", event$message, "\n")
          # error dugint connection to trying again to connect
          private$connect_ws()
        })
        #connect to websocket to start bot
        private$ws$connect()
      },
      
      handle_ws_close = function(event){
        logwarn("Client disconnected with code ", event$code,
                " and reason ", event$reason, "\n", sep = "")
        # trying to reconnect which then will send a resume request
        private$connect_ws() # this creates a new websocket a reconnects
      },
      
      update_seq = function(msg){
        # need to update the last s for the heartbeat and resume
        # may need to find a better place for this
        if (!is.null(msg$s)) { # seems that during ACK s in null, so avoid saving it
          private$last_s <- msg$s
        }
      },
      
      #' switcher for op code
      receiver_switcher = function(msg){
        # parse message
        msg <- fromJSON(msg)
        
        logdebug("Received message")
        logdebug(msg)
        
        private$update_seq(msg)
        
        # switching for op
        op <- as.character(msg$op)
        if (op %in% names(private$op_handlers)){
          private$op_handlers[[op]](msg)
        }
        else {
          logwarn("Unhandled OPCODE")
          logwarn(msg)
        }
      },
      
      #' Switch according to received events
      event_switcher = function(event){
        
        event_name <- event$t
        
        if (event_name %in% names(private$event_handlers)){
          private$event_handlers[[event_name]](event$d)
        }
      },
      
      #' hello is the first message sent by discord on the websocket
      #' need to setup heartbeat and intent
      handle_hello = function(hello_msg){
        logdebug("handling hello")
        # first thing to do is sending an heartbeat
        interval <- hello_msg$d$heartbeat_interval/1000 # milliseconds to seconds
        # closure to capture interval
        send_heartbeat <- function(){
          self$send_payload(OP_HB, data=private$last_s)
          # set up function to send next heartbeat to keep connection open
          later::later(send_heartbeat, interval)
        }
        
        # send first heartbeat and schedule them in background
        send_heartbeat()
        
        # send the intent to start receiving events
        if (private$first_connection){
          logdebug("first connection: so identify")
          private$send_identify()
        }
        else {
          private$send_resume()
        }
      },
      
      send_heartbeat = function(msg){
        self$send_payload(OP_HB, data=private$last_s)
      },
      
      #' connect to discord sending intents and 
      send_identify = function() {
        logdebug("sending indentify")
        logdebug(private$intent)
        data <- list(
          token = private$token,
          properties = list(
            `$os` = Sys.info()["sysname"],
            `$browser` = "R",
            `$device` = "R"
          ),
          intents = private$intent
        )
        self$send_payload(OP_IDENTIFY, data)
        private$first_connection <- F
      },
      
      #' The session is invalid for discord
      #' need to reconnect again
      handle_invalid_session = function(msg){
        lodebug("Handling invalid session")
        private$first_connection <- T # reset connection
        private$send_identify()
      },
      
      #' resumes previous sessions after they are interrupted
      send_resume = function(){
        loginfo("Resuming connection")
        resume <- list(token=private$token,
                       session_id=private$saved_ready$session_id,
                       seq = private$last_s)
        self$send_payload(OP_RESUME, resume)
      },
      
      #' reconnect socket if asked by discord
      handle_reconnect_request = function(msg){
        loddebug("Handling reconnect request")
        # close and then reopen the websocket
        private$ws$close()
      },
      
      
      #' save the ready event 
      handle_ready = function(ready_event){
        private$saved_ready <- ready_event
      }
      
      
    )
)
