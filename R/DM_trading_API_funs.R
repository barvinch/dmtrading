library(httr)
library(tidyverse)
library(sodium)
library(PKI)

# help function for signature
hex2raw <- function(hex) {
  hex <- gsub("[^0-9a-fA-F]", "", hex)
  if(length(hex) == 1) {
    if(nchar(hex) < 2 || nchar(hex) %% 2 != 0) {
      stop("hex is not a valid hexadecimal representation")
    }
    hex <- strsplit(hex, character(0))[[1]]
    hex <- paste(hex[c(TRUE, FALSE)], hex[c(FALSE, TRUE)], sep = "")
  }
  if(!all(vapply(X = hex, FUN = nchar, FUN.VALUE = integer(1)) == 2)) {
    stop("hex is not a valid hexadecimal representation")
  }
  as.raw(as.hexmode(hex))
}

#' Dmarket signature
#'
#' Creates signature to send to Dmarket API
#' @param string_to_sign string to sign
#' @param secret_key user's secret key
#' @return signature
#' @export
dm_signature <- function(string_to_sign = NULL,
                         secret_key     = NULL) {
  stopifnot(is.character(secret_key), is.character(secret_key))

  secret_bytes <- hex2raw(secret_key)

  encoded <- charToRaw(string_to_sign)

  sig_raw <- sodium::sig_sign(encoded,secret_bytes)

  signature <- paste0(sig_raw, collapse = '')
  return(signature)
}

#' query parameter function
#'
#' Creates query parameter for API call
#' uses quote function to get parameter name
#' and crete charecter string '{x_name}={x_value}'
#' @param x parameter to use in API call
#' @return query parameter
#' @export
q_parameter  <- function(x=NULL) {

  if (!is.null(x)) {
    paste0(quote(x),'=', x)
  } else {
  }
}

# constants ------------------------------
signature_prefix  <-  "dmar ed25519 "
rootApiUrl <-  "https://api.dmarket.com"




# create target -------------------------
#' Create target
#'
#' Creates target for buying items
#' @param gameId id of the game, CS:GO - a8db, Team Fortress 2 - tf2, Dota 2 - 9a92.
#' @param title title of the item AKA item name
#' @param price target price of the item
#' @param items_to_buy number of items to buy
#' @return success of the operation
#' @export
# TODO update for few items!
dm_create_target <- function(gameId,
                             title,
                             price,
                             items_to_buy) {

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))


  api_url_path  <-  "/marketplace-api/v1/user-targets/create"
  method1  <-  "POST"
  # example
  # gameId <- 'a8db' #
  # title <- "Falchion Case"
  # price_cents <- 0.12
  # categoryPath <- 'misc/container'
  # image <-  "https://steamcommunity-a.akamaihd.net/economy/image/-9a81dlWLwJ2UUGcVs_nsVtzdOEdtWwKGZZLQHTxDZ7I56KU0Zwwo4NUX4oFJZEHLbXU5A1PIYQNqhpOSV-fRPasw8rsUFJ5KBFZv668FF8ugPDMIWpAuIq1w4KIlaChZOyFwzgJuZNy3-2T89T0jlC2rhZla2vwIJjVLFHz75yKpg"
  body <- list(Targets = list(list(
    Amount= items_to_buy,
    gameId= gameId, Price= list(Amount= price, Currency= 'USD'),
    Attributes= list(list(Name = 'gameId', Value = gameId),
                     list(Name= 'title' ,   Value= title )
    )
  )))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             jsonlite::toJSON(body, auto_unbox = TRUE),
                             nonce)
  # cat(string_to_sign)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  POST(paste0(rootApiUrl, api_url_path),
                body=body,
                add_headers(headers),
                encode = 'json' #,               verbose()
  )

  resp_c <- content(resp)
  # is successful

  print(resp_c$Result[[1]]$Successful)
  resp_c$Result[[1]]$Successful
}




# GET balance -----------------------------
#' Get my balance
#'
#' Get user's balance
#' Balance in USD is of interest
#' @return balance
#' @export
#' @examples
#' blnc <- get_my_balance()
dm_get_my_balance <- function() {
  api_url_path  <-  "/account/v1/balance"
  method1  <-  "GET"
  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)


  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  GET(paste0(rootApiUrl, api_url_path),
               add_headers(headers),
               verbose())

  # check response status
  if (resp$status_code == 200) {
    resp_c <- content(resp)

    res_data <- resp_c %>%
      as_tibble() %>%
      mutate(across(.cols = everything(), .fns = as.numeric)) %>%
      mutate(usd = usd/100,
             usdAvailableToWithdraw = usdAvailableToWithdraw/100)
    return(res_data)
  } else {
    err_msg <-  paste0('error in query, code: ', resp$status_code,
                       '. Method: ', resp$request$method, ', url: ', resp$request$url)
  }

}




# GET user info -----------------------------
#' Get my account
#'
#' Get user's account info
#' @return account info
#' @export
#' @examples
#' my_acc <- get_my_acc()
dm_get_my_acc <- function() {
  api_url_path  <-  "/account/v1/user"
  method1  <-  "GET"
  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)


  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  GET(paste0(rootApiUrl, api_url_path),
               add_headers(headers),
               verbose())

  # check response status
  if (resp$status_code == 200) {
    resp_c <- content(resp)


    return(resp_c)
  } else {
    err_msg <-  paste0('error in query, code: ', resp$status_code,
                       '. Method: ', resp$request$method, ', url: ', resp$request$url)
  }



}

#

# get targets ---------------------------------------------
#' Get Targets from DMarket
#'
#' This function retrieves a list of targets from DMarket based on various filters.
#' @param GameID Character. Game identifier on market.
#' @param PriceFrom Numeric. Minimum price for filtering.
#' @param PriceTo Numeric. Maximum price for filtering.
#' @param Currency Character. Currency code to use with price range filter.
#' @param Title Character. Filter user targets by asset's title.
#' @param TargetID Character. Filter by unique target identifier.
#' @param Status Character. Target status. Default is 'TargetStatusActive'.
#' @param SortType Character. Sort type for user targets. Default is 'UserTargetsSortTypeDefault'.
#' @param Offset Character. Offset for response items.
#' @param Limit Character. Limit for number of returned items.
#' @param Cursor Character. Cursor for the next page identifier.
#' @param is_active Logical. Alternative for BasicFilters.Status. Default is NULL.
#' @param add_extras Logical. Whether to add extra information. Default is FALSE.
#' @return A tibble containing the list of targets.
#' @export
dm_get_targets  <- function(GameID                 = NULL, # char
                            PriceFrom              = NULL, # numeric
                            PriceTo                = NULL, # numeric
                            Currency               = NULL, # char
                            Title                  = NULL, # char
                            TargetID               = NULL, # char
                            Status                 = 'TargetStatusActive', # char, 'TargetStatusActive' or 'TargetStatusInactive'
                            SortType               = 'UserTargetsSortTypeDefault', # char
                            Offset                 = NULL, # char
                            Limit                  = NULL, # char
                            Cursor                 = NULL, # char
                            is_active              = NULL,  # BasicFilters.Status alternative
                            add_extras             = FALSE
) {
  # GameID   string   (query)	  Game identifier on market.
  # BasicFilters.PriceFrom number($float)  (query)	  Price range filtering.
  # BasicFilters.PriceTo   number($float)  (query)	  Price range filtering.
  # BasicFilters.Currency  string  (query)	  Currency code to use with price range filter.
  # BasicFilters.Title     string  (query)	  Filter user targets by asset's title.
  # BasicFilters.TargetID  string (query)	Filter by unique target identifier.
  # BasicFilters.Status    string (query)	Target current status.
  #                                     TargetStatusInactive: Target currently inactive.
  #                                     TargetStatusActive: Target currently active and can be matched with complaint offers.
  # SortType               string (query)	Sort user targets by one of predefined sort types.
  #                                     UserTargetsSortTypeDefault: Default sort type for user targets.
  # Offset                 string($uint64) (query) Offsets items in response.
  # Limit                  string($uint64)  (query)	Limits number or returned items in response.
  # Cursor                 string (query)	Cursor is next page identifier.

  if (!is.null(is_active )) {
    if  (is_active == TRUE) {
      Status    <-  'TargetStatusActive' # char, 'TargetStatusActive' or 'TargetStatusInactive'
    } else if (is_active == FALSE) {
      Status    <-  'TargetStatusInactive'
    }
  }

  defined <- c(GameID                = GameID                ,
               BasicFilters.PriceFrom= PriceFrom,
               BasicFilters.PriceTo  = PriceTo  ,
               BasicFilters.Currency = Currency ,
               BasicFilters.Title    = Title    ,
               BasicFilters.TargetID = TargetID ,
               BasicFilters.Status   = Status   ,
               SortType              = SortType              ,
               Offset                = Offset                ,
               Limit                 = Limit                 ,
               Cursor                = Cursor                )




  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))



  print(query_params)
  # query_params <- ''

  api_url_path  <-  "/marketplace-api/v1/user-targets"
  method1  <-  "GET"
  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)


  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)


  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               add_headers(headers),
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$Items  # $TargetID

  # return(x)

  if (length(x) > 0) {

    my_names = c('TargetID', 'Title', 'Amount', 'Status',  'GameID', 'GameType')

    nested_name <- 'Price'

    save_for_debug <<- x

    y <- map_df(lapply(x, "[",  my_names), as_tibble) %>%
      bind_cols(lapply(x, "[[",  nested_name) %>%
                  lapply(., "[") %>%
                  map_df(., as_tibble)  %>%
                  rename_with(.fn = ~ paste0(tolower(nested_name), '_', tolower(.x))))

    # if ( add_extras) {
    #   nested_name2 <- 'Attributes'
    #   y <- y %>%
    #     bind_cols(lapply(x, "[[",  nested_name2) %>%
    #                 lapply(., "[") %>%
    #                 map_df(., as_tibble)  %>%
    #                 rename_with(.fn = ~ paste0(tolower(nested_name2), '_', tolower(.x))) %>%
    #                 nest(data = extra_stickers, .names_sep = NULL))
    # }



    return(y)
  } else {
    return(x)
  }


}



# DELETE targets -----------------------------
#' Delete Target on DMarket
#'
#' This function deletes a target on DMarket based on the provided target ID.
#' @param target_id Character. The ID of the target to be deleted.
#' @return A list with the response from the API.
#' @export
dm_delete_target <- function(target_id) {

  stopifnot(is.character(target_id))
  stopifnot(is.vector(target_id))

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))


  api_url_path  <-  "/marketplace-api/v1/user-targets/delete"
  method1  <-  "POST"

  body <- list(Targets =tibble(TargetID = target_id))
  # body <- list(Targets = list(list(TargetID = target_id) ) )





  string_to_sign  <- paste0( method1,
                             api_url_path,
                             jsonlite::toJSON(body, auto_unbox = TRUE),
                             nonce)
  cat(string_to_sign)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  POST(paste0(rootApiUrl, api_url_path),
                body=body,
                add_headers(headers),
                encode = 'json',
                verbose())



  # check response status
  if (resp$status_code == 200) {
    resp_c <- content(resp)


    return(resp_c)
  } else {
    err_msg <-  paste0('error in query, code: ', resp$status_code,
                       '. Method: ', resp$request$method, ', url: ', resp$request$url)
    print(err_msg)
  }

}

# item_name <- c("Master Artificer's Hammer", "Artificer's Hammer")

# dm_get_aggregated_price -----------------------------------------------------
#' Get Aggregated Price for Items
#'
#' This function retrieves aggregated price information for specified items on DMarket.
#' @param item_name Character. The name of the item(s) to retrieve prices for.
#' @param Limit Character. Limit for number of returned items. Default is '100'.
#' @param Offset Character. Offset for response items. Default is '0'.
#' @return A tibble containing aggregated price information for the items.
#' @export
dm_get_aggregated_price <- function(item_name              = NULL,  # char
                                    Limit                  = '100', # char
                                    Offset                 = '0'    # char
) {

  stopifnot(is.character(item_name))
  stopifnot(is.vector(item_name))

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))


  api_url_path  <-  "/price-aggregator/v1/aggregated-prices"
  method1  <-  "GET"

  if (all(item_name == '')) {
    ttl <- '?'
  } else {
    ttl <- paste0('?Titles=', paste0(sapply(item_name,  URLencode, reserved=TRUE),
                                     collapse = '&Titles='), '&')
  }

  query_params <- paste0(ttl,'Limit=', Limit, '&', 'Offset=', Offset)
  # %>% URLencode( )


  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)
  # cat(string_to_sign)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               add_headers(headers),
               verbose())

  resp_c <- content(resp)

  # check response status
  if (resp$status_code == 200) {
    my_names = c('MarketHashName',  'GameID') # 'Offers.BestPrice', 'Offers.Count', 'Orders.BestPrice', 'Orders.Count',

    nested_name <- c( 'Offers', 'Orders' )


    y <- map_df(lapply(resp_c$AggregatedTitles, "[",  my_names), as_tibble) %>%
      bind_cols(lapply(resp_c$AggregatedTitles, "[[",  nested_name[1]) %>%
                  lapply(., "[") %>%
                  map_df(., as_tibble)  %>%
                  rename_with(.fn = ~ paste0(tolower(nested_name[1]), '_', tolower(.x))),
                lapply(resp_c$AggregatedTitles, "[[",  nested_name[2]) %>%
                  lapply(., "[") %>%
                  map_df(., as_tibble)  %>%
                  rename_with(.fn = ~ paste0(tolower(nested_name[2]), '_', tolower(.x)))
      )
    return(y)

  } else {
    err_msg <-  paste0('error in query, code: ', resp$status_code,
                       '. Method: ', resp$request$method, ', url: ', resp$request$url)
    print(err_msg)
    return(err_msg)
  }

}



# wrap API call
#' Wrap API Call
#'
#' This helper function wraps an API call and returns the data or an error message.
#' @param api_fun Function. The API function to call.
#' @param ... Additional arguments to pass to the API function.
#' @return The data returned by the API function or an error message.
#' @export
dm_wrap_api_get <- function(api_fun, ...){

  stopifnot(is.function(api_fun))

  ret_data <-  NULL

  ret_1 <- try({
    ret_data <- api_fun(...)
  })

  if (any(class(ret_1) == 'try-error')) {
    print('error in API call')
    print(ret_1)
  } else {
    ret_data <- ret_1
  }

  return(ret_data)
}


#' Get Aggregated Price for Items (Wrapper)
#'
#' This function retrieves aggregated price information for specified items on DMarket.
#' If the number of items is greater than 100, the function will make multiple API calls to retrieve the data.
#' @param item_names Data frame. A data frame containing the names of the items to retrieve prices for.
#' @return A tibble containing aggregated price information for the items.
#' @export
dm_get_agg_price_wrap <- function(item_names){

  stopifnot(is.data.frame(item_names))

  it_rows <- nrow(item_names)

  if (it_rows <= 100) {
    agg_price <- dm_wrap_api_get(dm_get_aggregated_price,
                                 item_name =  unique(item_names$item_name),
                                 Limit = 100)
  } else {

    agg_price <- NULL

    for (i in 1:ceiling(it_rows/100)) {
      fr <- (i-1)*100+1
      to <- if_else(i*100<it_rows, i*100, as.numeric(it_rows))

      agg_part <- dm_wrap_api_get(dm_get_aggregated_price,
                                  item_name =  unique(item_names$item_name[fr:to]),
                                  Limit = 100)

      if (!is.null(agg_part) && any(class(agg_part) == "data.frame")) {
        agg_price  <-  agg_price %>%
          bind_rows(agg_part )
      }
    }

    if (nrow(agg_price) > 0) {
      agg_price  <-  agg_price %>%
        mutate(offers_bestprice = as.numeric(offers_bestprice),
               orders_bestprice = as.numeric(orders_bestprice))
    }
  }


  return(agg_price)
}

# dm_get_agg_price_all
#' Get Aggregated Price for All Items
#'
#' This function retrieves aggregated price information for all items on DMarket.
#' If the number of items is greater than 1000, the function will make multiple API calls to retrieve the data.
#' @param limit Numeric. The maximum number of items to retrieve in a single API call.
#' @return A tibble containing aggregated price information for all items.
#' @export
dm_get_agg_price_all <- function(limit = 1000){
  # Adapted from MO

  # limit <-  10000
  batch_len  <-  limit
  i  <-  0
  agg_price <- NULL

  while (batch_len == limit) {

    prices <- dm_get_aggregated_price(item_name =  '',
                                      Limit = as.character(limit) ,
                                      Offset = as.character(i) )

    batch_len  <- nrow(prices)

    agg_price  <-  agg_price %>%
      bind_rows(prices )
    i <- i+batch_len
    print(i)
  }

  agg_price  <-  agg_price %>%
    mutate(offers_bestprice = as.numeric(offers_bestprice),
           orders_bestprice = as.numeric(orders_bestprice))


  return(agg_price)
}


# offers by title ----------------------------
#' Get Offers by Title
#'
#' This function retrieves offers by title on DMarket.
#' If item have extra columns, the function will add them to the data.
#' @param Title Character. The title of the item to retrieve offers for.
#' @param Limit Numeric. The maximum number of offers to retrieve.
#' @param add_extras Logical. Whether to add extra columns to the data.
#' @param extra_col Character. The extra columns to add to the data.
#' @return A tibble containing offers by title.
#' @export
dm_get_offers_by_title <- function(Title = NULL,
                                   Limit = 100,
                                   add_extras = FALSE,
                                   extra_col = NULL) {

  api_url_path  <-  "/exchange/v1/offers-by-title"
  method1  <-  "GET"

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  if (Title == '') {
    ttl <- '?'
  } else {
    ttl <- paste0('?Title=', paste0(sapply(Title,  URLencode, reserved=TRUE),
                                    collapse = '&Titles='), '&')
  }

  query_params <- paste0(ttl,'Limit=', Limit)
  # %>% URLencode( )


  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)
  # cat(string_to_sign)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json' ,
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce
  )


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               add_headers(headers),
               verbose())

  resp_c <- content(resp)



  if (add_extras) {

    my_names = c('itemId', 'gameId',  'title', 'discount', 'status')

    nested_name <- 'price'


    y <- map_df(lapply(resp_c$objects, "[",  my_names), as_tibble) %>%
      bind_cols(price_cols(resp_c$objects, 'price')     )

    # because of stickers, works only on CSGO
    if (!is.null(extra_col)) {
      nested_name2 <- 'extra'
      y <- y %>%
        bind_cols(lapply(resp_c$objects, "[[",  nested_name2) %>%
                    lapply(., "[", extra_col) %>%
                    map_df(., as_tibble)  %>%
                    rename_with(.fn = ~ paste0(tolower(nested_name2), '_', tolower(.x))) # %>%
                  # nest(data = extra_paintseed, .names_sep = NULL)
        )
    } else {
      nested_name2 <- 'extra'
      y <- y %>%
        bind_cols(lapply(resp_c$objects, "[[",  nested_name2) %>%
                    lapply(., "[") %>%
                    map_df(., as_tibble)  %>%
                    rename_with(.fn = ~ paste0(tolower(nested_name2), '_', tolower(.x))) # %>%
                  # nest(data = extra_paintseed, .names_sep = NULL)
        )
    }
  } else {
    my_names = c('itemId', 'gameId',  'title', 'discount', 'status') # , 'extra'

    nested_name <- 'price'


    y <- map_df(lapply(resp_c$objects, "[",  my_names), as_tibble) %>%
      bind_cols(price_cols(resp_c$objects, 'price')     )
  }

  return(y)

}


# create target -------------------------
# TODO update for few items!
#' Create Advanced Target
#'
#' This function creates an advanced metadata target on DMarket.
#' @param gameId Character. The game ID of the item to create a target for.
#' @param title Character. The title of the item to create a target for.
#' @param price Numeric. The price of the item to create a target for.
#' @param items_to_buy Numeric. The number of items to buy.
#' @param phase Character. The phase of the item to create a target for.
#' @param paintSeed Numeric. The paint seed of the item to create a target for.
#' @param floatPartValue Character. The float part value of the item to create a target for.
#' @param verbose1 Logical. Whether to print the response.
#' @return A tibble containing the response.
#' @export
dm_create_advanced_target <- function(gameId,
                                      title,
                                      price,
                                      items_to_buy,
                                      phase = '',
                                      paintSeed = 0,
                                      floatPartValue = '',
                                      verbose1 = FALSE) {

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))


  attr_list <- list( )

  if (phase != '') {
    attr_list <- append(attr_list, list(phase = phase))
  }

  if (paintSeed > 0) {
    attr_list <- append(attr_list, list(paintSeed = paintSeed))
  }

  if (floatPartValue != '') {
    attr_list <- append(attr_list, list(floatPartValue = floatPartValue))
  }

  api_url_path  <-  "/marketplace-api/v1/user-targets/create"
  method1  <-  "POST"

  body <- list(Targets = list(list(
    Amount= items_to_buy,
    gameId= gameId, Price= list(Amount= price, Currency= 'USD'),
    Attributes= list(list(Name = 'gameId', Value = gameId),
                     list(Name= 'title' ,   Value= title )

    ),
    Attrs = attr_list
  )))



  string_to_sign  <- paste0( method1,
                             api_url_path,
                             jsonlite::toJSON(body, auto_unbox = TRUE),
                             nonce)
  if(verbose1) {
    print(body)

    print(jsonlite::fromJSON( jsonlite::toJSON(body, auto_unbox = TRUE)))

    cat(string_to_sign)
  }

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  if(verbose1) {
    resp <-  POST(paste0(rootApiUrl, api_url_path),
                  body=body,
                  add_headers(headers),
                  encode = 'json' ,
                  verbose()
    )
  } else {
    resp <-  POST(paste0(rootApiUrl, api_url_path),
                  body=body,
                  add_headers(headers),
                  encode = 'json'
    )
  }

  resp_c <- content(resp)
  # is successful

  rr <- resp_c$Result[[1]]

  if (rr$Successful == TRUE) {
    print(paste(gameId, title, paste(names(attr_list), attr_list), rr$TargetID, sep = ' >> ', collapse = ';'))
    return(rr$TargetID)
  } else {
    print(rr$Error)
    return(rr$Error)
  }
}

#' price_cols
#'
#' This function unnests the price columns from the nested list.
#' @param data A list. The data to unnest.
#' @param nested_name Character. The name of the nested list.
#' @return unnseted data
#' @export
price_cols <- function(data, nested_name) {
  lapply(data, "[[",  nested_name) %>%
    lapply(., "[") %>%
    map_df(., as_tibble)  %>%
    rename_with(.fn = ~ paste0(tolower(nested_name), '_', tolower(.x))) %>%
    select(ends_with('usd')) %>%
    mutate(across(.fns =  function(x){as.numeric(x)/100} ))
}



# GET items from market -----------------------------
#' Get Items
#'
#' This function gets items from the market.
#' @param GameID Character. The game ID of the item to get.
#' @param title Character. The title of the item to get.
#' @param limit Integer. The number of items to get.
#' @param offset Integer. The offset of the items to get.
#' @param orderBy Character. The order of the items to get.
#' @param orderDir Character. The direction of the order.
#' @param treeFilters Character. The tree filters of the items to get.
#' @param currency Character. The currency of the items to get.
#' @param priceFrom Numeric. The price from of the items to get.
#' @param priceTo Numeric. The price to of the items to get.
#' @param types Character. The types of the items to get.
#' @param cursor Character. The cursor of the items to get.
#' @param add_extras Logical. Whether to add extras.
#' @return A tibble containing the response.
#' @export
dm_get_items <- function(GameID                 = NULL, # char
                         title                  = NULL, # char  filter %title%
                         limit                  = NULL, # integer
                         offset                 = NULL, # integer
                         orderBy                = NULL, # string c('title', 'price')
                         orderDir               = NULL, # string c('asc', 'desc')
                         treeFilters            = NULL, # string
                         currency               = 'USD',#char
                         priceFrom              = NULL, # numeric
                         priceTo                = NULL, # numeric
                         types                  = NULL, # string
                         cursor                 = NULL, # char
                         add_extras             = FALSE
) {
  api_url_path  <-  "/exchange/v1/market/items"
  method1  <-  "GET"

  defined <- c(gameId                = GameID                ,
               title                 = URLencode(title)      ,
               currency              = currency              ,
               limit                 = limit                 ,
               offset                = offset                ,
               orderBy                = orderBy,
               orderDir               = orderDir,
               treeFilters            = treeFilters,
               priceFrom              = priceFrom, # numeric
               priceTo                = priceTo, # numeric
               types                  = types,
               cursor                = cursor                )




  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)


  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)


  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$objects[[1]]  #


  # return(resp_c)

  my_names = c('itemId', 'gameId',  'lockStatus','title', 'discount', 'status', 'owner', 'ownersBlockchainId')

  nested_name <- 'price'


  y <- map_df(lapply(resp_c$objects, "[",  my_names), as_tibble) %>%
    bind_cols(price_cols(resp_c$objects, 'price')     )

  if (add_extras) {
    # because of stickers, works only on CSGO
    nested_name2 <- 'extra'
    y <- y %>%
      bind_cols(lapply(resp_c$objects, "[[",  nested_name2) %>%
                  lapply(., "[") %>%
                  map_df(., as_tibble)  %>%
                  rename_with(.fn = ~ paste0(tolower(nested_name2), '_', tolower(.x))) %>%
                  nest(data = extra_stickers, .names_sep = NULL))
  }


  return(y)

}




# GET items from my inventory -----------------------------
#' Get My Items
#'
#' This function gets items from the user's inventory.
#' @param GameID Character. The game ID of the item to get.
#' @param title Character. The title of the item to get.
#' @param limit Integer. The number of items to get.
#' @param offset Integer. The offset of the items to get.
#' @param orderBy Character. The order of the items to get.
#' @param orderDir Character. The direction of the order.
#' @param treeFilters Character. The tree filters of the items to get.
#' @param currency Character. The currency of the items to get.
#' @param priceFrom Numeric. The price from of the items to get.
#' @param priceTo Numeric. The price to of the items to get.
#' @param classIds Character. The class IDs of the items to get.
#' @param cursor Character. The cursor of the items to get.
#' @param add_extras Logical. Whether to add extras.
#' @return A tibble containing the response.
#' @export
dm_get_my_items <- function(GameID                 = NULL, # char
                            title                  = NULL, # char  filter %title%
                            limit                  = NULL, # integer
                            offset                 = NULL, # integer
                            orderBy                = NULL, # string c('title', 'price')
                            orderDir               = NULL, # string c('asc', 'desc')
                            treeFilters            = NULL, # string
                            currency               = 'USD',#char
                            priceFrom              = NULL, # numeric
                            priceTo                = NULL, # numeric
                            classIds               = NULL, # string
                            cursor                 = NULL, # char
                            add_extras             = FALSE
) {
  api_url_path  <-  "/exchange/v1/user/items"
  method1  <-  "GET"

  defined <- c(gameId                = GameID                ,
               currency              = currency              ,
               title                 = URLencode(title)      ,
               limit                 = limit                 ,
               offset                = offset                ,
               orderBy               = orderBy,
               orderDir              = orderDir,
               treeFilters           = treeFilters,
               priceFrom             = priceFrom, # numeric
               priceTo               = priceTo, # numeric
               classIds              = classIds,
               cursor                = cursor                )




  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             # jsonlite::toJSON(body, auto_unbox = TRUE),
                             nonce)


  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)


  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$objects[[1]]  #


  # return(resp_c)

  my_names = c('itemId', 'gameId',  'lockStatus','title', 'discount', 'status', 'owner', 'ownersBlockchainId')

  nested_name <- 'price'




  y <- map_df(lapply(resp_c$objects, "[",  my_names), as_tibble) %>%
    bind_cols(price_cols(resp_c$objects, 'price')       ) %>%
    bind_cols(price_cols(resp_c$objects, 'instantPrice')       ) %>%
    bind_cols(price_cols(resp_c$objects, 'exchangePrice')       ) %>%
    bind_cols(price_cols(resp_c$objects, 'suggestedPrice')       )
  # %>%
  #   bind_cols(price_cols(resp_c$objects, 'recommendedPrice')       )

  if (add_extras) {
    # because of stickers, works only on CSGO
    nested_name2 <- 'extra'
    y <- y %>%
      bind_cols(lapply(resp_c$objects, "[[",  nested_name2) %>%
                  lapply(., "[") %>%
                  map_df(., as_tibble)  %>%
                  rename_with(.fn = ~ paste0(tolower(nested_name2), '_', tolower(.x))) %>%
                  nest(data = extra_stickers, .names_sep = NULL))
  }


  return(y)

}



# GET customized fees -----------------------------
#' Get Customized Fees
#'
#' This function gets customized fees.
#' @param GameID Character. The game ID of the item to get.
#' @param offerType Character. The offer type of the item to get.
#' @param limit Integer. The number of items to get.
#' @param offset Integer. The offset of the items to get.
#' @param add_extras Logical. Whether to add extras.
#' @return A tibble containing the response.
#' @export
dm_get_customized_fees <- function(GameID                 = NULL, # char
                                   offerType              = "dmarket", # char  c( "dmarket", "p2p")
                                   limit                  = NULL, # integer
                                   offset                 = NULL, # integer
                                   add_extras             = FALSE
) {
  api_url_path  <-  "/exchange/v1/customized-fees"
  method1  <-  "GET"

  defined <- c(gameId                = GameID                ,
               offerType                 = offerType                 ,
               limit                 = limit                 ,
               offset                = offset                )
  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  # print(query_params)

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               add_headers(headers),
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$reducedFees  #


  # return(resp_c)

  my_names = c('title', 'fraction',  'minPrice','maxPrice', 'expiresAt')


  y <- map_df(lapply(x, "[",  my_names), as_tibble) %>%
    mutate(fraction = as.numeric(fraction),
           minPrice = as.numeric(minPrice)/100,
           maxPrice = as.numeric(maxPrice)/100,
           expiresAt = lubridate::as_datetime(expiresAt))

  #
  return(y)

}



# GET default fees -----------------------------
#' Get Default Fees
#'
#' This function gets default fees.
#' @param GameID Character. The game ID of the item to get.
#' @param offerType Character. The offer type of the item to get.
#' @param limit Integer. The number of items to get.
#' @param offset Integer. The offset of the items to get.
#' @param add_extras Logical. Whether to add extras.
#' @return A tibble containing the response.
#' @export
dm_get_default_fees <- function(GameID                 = NULL, # char
                                offerType              = "dmarket", # char  c( "dmarket", "p2p")
                                limit                  = NULL, # integer
                                offset                 = NULL, # integer
                                add_extras             = FALSE
) {
  api_url_path  <-  "/exchange/v1/customized-fees"
  method1  <-  "GET"

  defined <- c(gameId                = GameID                ,
               offerType                 = offerType                 ,
               limit                 = limit                 ,
               offset                = offset                )
  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$defaultFee  #


  # return(x)


  y <- as_tibble(x) %>%
    mutate(minAmount = as.numeric(minAmount)/100,
           fraction = as.numeric(fraction))

  #
  return(y)

}


# GET closed targets -----------------------------
#' Get Closed Targets
#'
#' This function gets closed targets.
#' @param limit Integer. The number of items to get.
#' @param offset Integer. The offset of the items to get.
#' @param OrderDir Character. The order direction.
#' @return A tibble containing the response.
#' @export
dm_get_closed_targets <- function(limit                = NULL, # integer
                                  offset                 = NULL, # integer
                                  OrderDir               ='desc'  # c('asc', 'desc')
) {
  api_url_path  <-  "/marketplace-api/v1/user-targets/closed"
  method1  <-  "GET"

  defined <- c(Limit                 = limit                 ,
               OrderDir                 = OrderDir                 ,
               Offset                = offset                )
  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$Trades  #


  # return(x)

  my_names = c('OfferID', 'TargetID',  'AssetID','Title',  'Amount')

  nested_name <- 'Price'




  y <- map_df(lapply(x, "[",  my_names), as_tibble) %>%
    bind_cols(lapply(x, "[[",  nested_name) %>%
                lapply(., "[") %>%
                map_df(., as_tibble)  %>%
                rename_with(.fn = ~ paste0(tolower(nested_name), '_', tolower(.x)))  )

  #
  return(y)

}




# get targets with attributes ---------------------------------------------
#' Get User Targets with Attributes
#'
#'  This function gets user targets with attributes.
#'  @param GameID Character. The game ID of the item to get.
#'  @param PriceFrom Numeric. The price from of the item to get.
#'  @param PriceTo Numeric. The price to of the item to get.
#'  @param Currency Character. The currency of the item to get.
#'  @param Title Character. The title of the item to get.
#'  @param TargetID Character. The target ID of the item to get.
#'  @param Status Character. The status of the item to get.
#'  @param SortType Character. The sort type of the item to get.
#'  @param Offset Character. The offset of the item to get.
#'  @param Limit Character. The limit of the item to get.
#'  @param Cursor Character. The cursor of the item to get.
#'  @param is_active Logical. The status of the item to get.
#'  @param attributes Logical. The attributes of the item to get.
#'  @return A tibble containing the response.
#'  @export
dm_get_user_targets_attr  <- function(GameID                 = NULL, # char
                                      PriceFrom              = NULL, # numeric
                                      PriceTo                = NULL, # numeric
                                      Currency               = NULL, # char
                                      Title                  = NULL, # char
                                      TargetID               = NULL, # char
                                      Status                 = 'TargetStatusActive', # char, 'TargetStatusActive' or 'TargetStatusInactive'
                                      SortType               = 'UserTargetsSortTypeDefault', # char
                                      Offset                 = NULL, # char
                                      Limit                  = NULL, # char
                                      Cursor                 = NULL, # char
                                      is_active              = NULL,  # BasicFilters.Status alternative
                                      attributes             = FALSE
) {

  if (!is.null(is_active )) {
    if  (is_active == TRUE) {
      Status    <-  'TargetStatusActive' # char, 'TargetStatusActive' or 'TargetStatusInactive'
    } else if (is_active == FALSE) {
      Status    <-  'TargetStatusInactive'
    }
  }

  defined <- c(GameID                = GameID                ,
               BasicFilters.PriceFrom= PriceFrom,
               BasicFilters.PriceTo  = PriceTo  ,
               BasicFilters.Currency = Currency ,
               BasicFilters.Title    = Title    ,
               BasicFilters.TargetID = TargetID ,
               BasicFilters.Status   = Status   ,
               SortType              = SortType              ,
               Offset                = Offset                ,
               Limit                 = Limit                 ,
               Cursor                = Cursor                )




  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))


  print(query_params)
  # query_params <- ''

  api_url_path  <-  "/marketplace-api/v1/user-targets"
  method1  <-  "GET"
  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             # jsonlite::toJSON(body, auto_unbox = TRUE),
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$Items  # $TargetID

  return(x)

  my_names = c('TargetID', 'Title', 'Amount', 'Status',  'GameID', 'GameType')

  nested_name <- 'Price'

  y <- map_df(lapply(x, "[",  my_names), as_tibble) %>%
    bind_cols(lapply(x, "[[",  nested_name) %>%
                lapply(., "[") %>%
                map_df(., as_tibble)  %>%
                rename_with(.fn = ~ paste0(tolower(nested_name), '_', tolower(.x))))

  if ( attributes) {

    # convert pairs (from stackoverflow answer)
    conv_pairs <- function(pairs) sapply(pairs, \(x) setNames(x[2], x[1]))

    nested_name2 <- 'Attributes'

    y <- y %>%
      bind_cols(lapply(x, "[[",  nested_name2) %>%
                  lapply(., conv_pairs)  %>%
                  map_df(., as_tibble)  %>%
                  select(exterior, category, gameId, title,  name,
                         itemSlug, categoryPath,   tradable,  isAdvanced,
                         floatPartValue, paintSeed ) %>%
                  mutate(tradable = as.logical(toupper(tradable)),
                         isAdvanced =  as.logical(toupper(isAdvanced))) %>%
                  rename_with(.fn = ~ paste0( 'attr_', tolower(.x))) )
    #
  }

  return(y)
}



# get targets sandbox ---------------------------------------------
#' Get targets from the sandbox.
#'  @param PriceTo Numeric. The price to of the item to get.
dm_get_targets_test  <- function(GameID                 = NULL, # char
                                 PriceFrom              = NULL, # numeric
                                 PriceTo                = NULL, # numeric
                                 Currency               = NULL, # char
                                 Title                  = NULL, # char
                                 TargetID               = NULL, # char
                                 Status                 = 'TargetStatusActive', # char, 'TargetStatusActive' or 'TargetStatusInactive'
                                 SortType               = 'UserTargetsSortTypeDefault', # char
                                 Offset                 = NULL, # char
                                 Limit                  = NULL, # char
                                 Cursor                 = NULL, # char
                                 is_active              = NULL,  # BasicFilters.Status alternative
                                 attributes             = FALSE
) {

  if (!is.null(is_active )) {
    if  (is_active == TRUE) {
      Status    <-  'TargetStatusActive' # char, 'TargetStatusActive' or 'TargetStatusInactive'
    } else if (is_active == FALSE) {
      Status    <-  'TargetStatusInactive'
    }
  }

  defined <- c(GameID                = GameID                ,
               BasicFilters.PriceFrom= PriceFrom,
               BasicFilters.PriceTo  = PriceTo  ,
               BasicFilters.Currency = Currency ,
               BasicFilters.Title    = Title    ,
               BasicFilters.TargetID = TargetID ,
               BasicFilters.Status   = Status   ,
               SortType              = SortType              ,
               Offset                = Offset                ,
               Limit                 = Limit                 ,
               Cursor                = Cursor                )




  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))


  print(query_params)
  # query_params <- ''

  api_url_path  <-  "/marketplace-api/v1/user-targets"
  method1  <-  "GET"
  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             # jsonlite::toJSON(body, auto_unbox = TRUE),
                             nonce)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$Items  # $TargetID

  return(x)
}

# helper funs ----------------------------
#' Create a nonce.
#' @description Create a nonce.
#' @return A nonce.
#' @export
mk_nonce <- function() as.character(round(as.numeric(as.POSIXct(Sys.time()))))

#' Create a header.
#'
#' @description Create a header.
#' @param method1 The method.
#' @param api_url_path The api url path.
#' @param query_params The query params.
#' @param body The body.
#' @param secret_key The secret key.
#' @param nonce The nonce.
#' @return A header.
mk_header <- function(method1,
                      api_url_path,
                      query_params = NULL,
                      body = NULL,
                      secret_key,
                      nonce) {


  string_to_sign  <- paste0( method1,
                             api_url_path,
                             if_else(is.null(query_params), '', query_params),
                             if_else(is.null(body), '', as.character(jsonlite::toJSON(body, auto_unbox = TRUE))),
                             nonce)

  print(string_to_sign)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)

  print(signature)

  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )
}

# create offer -------------------------
# TODO update for few items!
#' Create an offer.
#'
#' @description Create an offer.
#' @param asset_id The asset id.
#' @param price The price.
#' @return A response.
#' @export
dm_create_offer <- function(asset_id = NULL, # char
                            price = NULL     # numeric, in USD (not cents)
) {

  stopifnot(!is.null(asset_id))
  stopifnot(!is.null(price))
  stopifnot(is.numeric(price))
  stopifnot(price > 0)
  stopifnot(is.character(asset_id))

  nonce <- mk_nonce()


  api_url_path  <-  "/marketplace-api/v1/user-offers/create"
  method1  <-  "POST"

  body <- list(Offers = list(list(
    AssetID= asset_id,
    Price= list(Currency= 'USD', Amount= price )
  )
  ))


  headers <- mk_header(method1 = method1,
                       api_url_path = api_url_path,
                       query_params = '',
                       body = body,
                       secret_key,
                       nonce )

  # print(headers)

  resp <-  POST(paste0(rootApiUrl, api_url_path),
                body=body,
                add_headers(headers),
                encode = 'json' ,
                verbose()
  )

  resp_c <- content(resp)
  # is successful

  print(resp_c$Result[[1]]$Successful)
  resp_c$Result[[1]]$Successful
}

# get user inventory -------------------------
#' Get user inventory.
#'
#' @description Get user inventory.
#' @param limit The limit.
#' @param offset The offset.
#' @param BasicFilters.InMarket The basic filters.
#' @return A response.
#' @export
dm_get_user_inventory <- function(limit                 = NULL, # integer
                                  offset                 = NULL, # integer
                                  BasicFilters.InMarket  = TRUE  # bulean
) {
  api_url_path  <-  "/marketplace-api/v1/user-inventory"
  method1  <-  "GET"

  defined <- c(Limit                 = limit                 ,
               BasicFilters.InMarket = BasicFilters.InMarket                 ,
               Offset                = offset                )
  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- mk_nonce()

  headers <- mk_header(method1 = method1,
                       api_url_path = api_url_path,
                       query_params = query_params,
                       body = NULL,
                       secret_key,
                       nonce = nonce)


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  y <- resp_c
  # return(y)

  x <- resp_c$Items  #


  # return(x)
  if (y$Total > 0) {
    my_names = c('AssetID', 'VariantID',
                 'ImageURL', 'Title',
                 'GameID', 'GameType', 'Location',
                 "Withdrawable", "Depositable",  "Tradable",
                 'ClassID')

    nested_name <- 'MarketPrice'

    # x <- m_inv

    y <- map_df(lapply(x, "[",  my_names), as_tibble)  %>%
      bind_cols(lapply(x, "[[",  nested_name) %>%
                  lapply(., "[") %>%
                  map_df(., as_tibble)  %>%
                  rename_with(.fn = ~ paste0(tolower(nested_name), '_', tolower(.x)))  )

    y <- as_tibble(y) %>%
      mutate(marketprice_amount = as.numeric(marketprice_amount))

    #
    return(y)
  } else {
    return(NULL)
  }

}



# get closed targets -------------------------
# TODO
# Presentation # string
# (query)
# Inventory asset presentation type.
#
# Available values : InventoryPresentationSimple, InventoryPresentationDetailed
#
# Default value : InventoryPresentationSimple

#' Get closed targets.
#'
#' @description Get closed targets.
#' @param limit The limit.
#' @param offset The offset.
#' @param OrderDir The order direction.
#' @param Presentation The presentation.
#' @return A response.
#' @export
dm_get_closed_targets <- function(limit     = NULL, # integer
                                  offset    = NULL, # integer
                                  OrderDir  = 'desc',  # char
                                  Presentation = 'InventoryPresentationSimple' # char
) {
  api_url_path  <-  "/marketplace-api/v1/user-targets/closed"
  method1  <-  "GET"

  defined <- c(Limit                 = limit                 ,
               OrderDir              = OrderDir              ,
               Offset                = offset                )
  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- mk_nonce()

  headers <- mk_header(method1 = method1,
                       api_url_path = api_url_path,
                       query_params = query_params,
                       body = NULL,
                       secret_key,
                       nonce = nonce)


  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  y <- resp_c
  # return(y)
  x <- resp_c$Trades  #


  # return(x)

  my_names = c('OfferID', 'TargetID',
               'AssetID',
               'CurrencyCode',  'Amount',
               'Title' )

  nested_name <- 'Price'

  # x <- m_inv

  y <- map_df(lapply(x, "[",  my_names), as_tibble)  %>%
    bind_cols(lapply(x, "[[",  nested_name) %>%
                lapply(., "[") %>%
                map_df(., as_tibble)  %>%
                rename_with(.fn = ~ paste0(tolower(nested_name), '_', tolower(.x)))  )

  y <- as_tibble(y) %>%
    mutate(price_amount = as.numeric(price_amount))

  #
  return(y)

}


# GET user offers -----------------------------
#' Get user offers.
#'
#' @description Get user offers.
#' @param limit The limit.
#' @param offset The offset.
#' @param Status The status.
#' @param GameID The game ID.
#' @return A response.
#' @export
dm_get_user_offers <- function(limit                  = NULL, # integer
                               offset                 = NULL, # integer,
                               Status                 = 'OfferStatusActive',  # OfferStatusDefault, OfferStatusActive, OfferStatusSold, OfferStatusInactive, OfferStatusIn_transfer
                               GameID                 = NULL  # c('asc', 'desc')
) {
  api_url_path  <-  "/marketplace-api/v1/user-offers"
  method1  <-  "GET"

  defined <- c(Limit                 = limit                 ,
               Status                 = Status                 ,
               Offset                = offset                )
  qp <- NULL

  for (i in 1:length(defined)) {
    qp <- c(qp, paste0(force(names(defined)[i]),'=', force(defined[i])))
  }

  query_params <- paste0('?', paste0( qp, collapse = '&'))

  print(query_params)

  nonce <- mk_nonce()

  headers <- mk_header(method1 = method1,
                       api_url_path = api_url_path,
                       query_params = query_params,
                       body = NULL,
                       secret_key,
                       nonce = nonce)

  resp <-  GET(paste0(rootApiUrl, api_url_path, query_params),
               add_headers(headers),
               verbose())

  stopifnot(resp$status_code == 200)

  resp_c <- content(resp)

  x <- resp_c$Items  #


  # return(x)
  my_names = c("AssetID",
               "Title", "GameID", "GameType",
               "Location","Withdrawable",    "Depositable",  "Tradable")




  # Flatten each list element and combine field3's nested list with the outer list
  flattened_list <- map(x, ~ c(.x[my_names], .x$Offer$Price, OfferID = .x$Offer$OfferID, CreatedDate =.x$Offer$CreatedDate   ))

  # Bind the rows into a data frame
  final_df <- bind_rows(flattened_list) %>%
    select(asset_id = AssetID,
           item_name = Title,
           game_id = GameID,
           GameType,
           Location,
           Withdrawable,
           Depositable,
           Tradable,
           offer_id = OfferID,
           price = Amount,
           created_date= CreatedDate) %>%
    mutate(created_date = as_datetime(as.numeric(created_date)))

  return(final_df)

}

# edit offer -------------------------
# TODO update for few items!
#' Edit offer.
#'
#' @description Edit offer.
#' @param offer_id The offer ID.
#' @param asset_id The asset ID.
#' @param price The price.
#' @return A response.
#' @export
dm_edit_offer <- function(offer_id = NULL, # char
                          asset_id = NULL, # char
                          price = NULL     # numeric, in USD (not cents)
) {

  stopifnot(!is.null(asset_id))
  stopifnot(!is.null(offer_id))
  stopifnot(!is.null(price))
  stopifnot(is.numeric(price))
  stopifnot(price > 0)
  stopifnot(is.character(asset_id))

  nonce <- mk_nonce()


  api_url_path  <-  "/marketplace-api/v1/user-offers/edit"
  method1  <-  "POST"

  body <- list(Offers = list(list(
    OfferID= offer_id,
    AssetID= asset_id,
    Price= list(Currency= 'USD', Amount= price )
  )
  ))


  headers <- mk_header(method1 = method1,
                       api_url_path = api_url_path,
                       query_params = '',
                       body = body,
                       secret_key,
                       nonce )

  print(headers)

  resp <-  POST(paste0(rootApiUrl, api_url_path),
                body=body,
                add_headers(headers),
                encode = 'json' ,
                verbose()
  )

  resp_c <- content(resp)
  # is successful

  print(resp_c$Result[[1]]$Successful)
  resp_c$Result[[1]]$Successful
}


# get targets by title -------------------------
#' Get targets by title.
#'
#' @description Get targets by title.
#' @param game_id The game ID.
#' @param title The title.
#' @return A response.
#' @export
dm_get_targets_by_title <- function(game_id, title) {

  stopifnot(is.character(game_id))
  stopifnot(is.character(title))

  # Define API endpoint with parameters
  api_url_path <- paste0("/marketplace-api/v1/targets-by-title")
  query_params <- paste0("/",game_id, "/", title)



  method1 <- "GET"

  nonce <- as.character(round(as.numeric(as.POSIXct(Sys.time()))))

  string_to_sign  <- paste0( method1,
                             api_url_path,
                             query_params,
                             nonce)
  print(string_to_sign)

  signature <- dm_signature(string_to_sign = string_to_sign,
                            secret_key = secret_key)


  headers = c(
    `X-Api-Key` =  public_key,
    `Accept` = 'application/json',
    `X-Request-Sign` = paste0(signature_prefix, signature), # signature
    `X-Sign-Date` = nonce )

  # encoded_title HERE!
  resp <-  GET(paste0(rootApiUrl, api_url_path, URLencode(query_params)),
               # body=body,
               add_headers(headers),
               # encode = 'json',
               verbose())


  # Check response status
  if (resp$status_code == 200) {
    resp_c <- content(resp, "parsed")

    # return(resp_c)


    # Assuming response format is as mentioned
    if (!is.null(resp_c$orders)) {

      df <- purrr::map_dfr(resp_c$orders, ~tibble::tibble(
        amount = .x$amount,
        price = .x$price,
        title = .x$title,
        attributes = list(.x$attributes)
      )) %>%
        mutate(title = as.character(title),
               amount = as.integer(amount),
               price = as.numeric(price)/100)
      # %>%
      #   unnest(attributes) %>%
      #   select(title, amount, price, everything())

      return(df)
    } else {
      return(data.frame()) # Empty data frame in case there are no orders
    }


  } else {
    return(resp)
    # stop(paste0('Error in query, code: ', resp$status_code,
    #             '. Method: ', resp$request$method, ', URL: ', resp$request$url))
  }
}


# for tests
# tgts <- dm_get_targets_by_title(game_id = '9a92', title = 'Manifold Paradox')
#
# tgts <- dm_get_targets_by_title(game_id = 'a8db', title = '★ StatTrak™ Skeleton Knife | Slaughter (Minimal Wear)')
#
# tgts  %>%
#   unnest_wider(attributes) %>%
#   view()

