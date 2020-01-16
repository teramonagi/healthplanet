#
#' Get Access Token for Health Planet API.
#'
#' Get Access Token for Health Planet API. You need to have your own account on [Health Planet](https://www.healthplanet.jp/).
#'
#' @export
get_token <- function(user_id=NULL, user_password=NULL){
  redirect_uri <- "https://www.healthplanet.jp/success.html"
  client_id <= "329.EcahSegA4b.apps.healthplanet.jp"
  client_secret <- "1458478017380-cJjbCdWURa8JOyV3TNEWnEvd0qpvXRmGMtu8EW8m"

  code <- if(!is.null(user_id) && !is.null(user_password)){
    #Stop warnings...
    old <- options(warn = -1)
    #Login -> Accept the API -> Get the code for access token.
    page_login <- rvest::html_session(auth_url(client_id))
    form_login <- rvest::set_values(rvest::html_form(page_login)[[1]], loginId=user_id, passwd=user_password)
    page_approval <- suppressMessages(rvest::submit_form(page_login, form_login))
    form_approval <- rvest::set_values(rvest::html_form(page_approval)[[1]], approval="true")
    #Adhoc for rvest pakcage to misrecognized that there is a submit form...
    form_approval$fields[[3]] <- form_approval$fields[[1]]
    form_approval$fields[[3]]$type <- "submit"
    page_code <- suppressMessages(rvest::submit_form(page_approval, form_approval))
    options(old)
    rvest::html_text(rvest::html_node(page_code, "#code"))
  } else{
    utils::browseURL(auth_url(client_id))
    if(exists(".rs.askForPassword")) .rs.askForPassword("Paste code here: ") else readline("Paste code here: ")
  }
  request_token(code, client_id, client_secret, redirect_uri)
}

request_token <- function(code, client_id, client_secret, redirect_uri){
  #Get Access token
  body <- list(
    client_id=client_id,
    client_secret=client_secret,
    redirect_uri=redirect_uri,
    code=code,
    grant_type="authorization_code")
  response <- httr::POST(url="https://www.healthplanet.jp/oauth/token", body=body)
  #Get access token from
  httr::content(response)$access_token
}

auth_url <- function(client_id){
  scope <- "innerscan,sphygmomanometer,pedometer,smug"
  glue::glue("https://www.healthplanet.jp/oauth/auth?client_id={client_id}&redirect_uri={REDIRECT_URI}&scope={scope}&response_type=code")
}

#' Get the innerscan data
#'
#' Get the innerscan data via HelthPlanet API with Access Token.
#'
#' @param access_token the token given by getToken()
#' @export
innerscan <- function(token)
{
  #Constants
  #See the API document if you want to know the detail: https://www.healthplanet.jp/apis/api.html
  tag <- "6021,6022,6023,6024,6025,6026,6027,6028,6029"
  table <- c(
  "6021"="weight",
  "6022"="body_fat",
  "6023"="muscle_mass",
  "6024"="muscle_score",
  "6025"="visceral_fat_level",
  "6026"="visceral fat level",
  "6027"="basal_metabolic_rate",
  "6028"="body_age",
  "6029"="bone_mass")
  query <- list(
    access_token=token,
    date="1",
    tag=paste(names(table), collapse = ","))

  #Get response depending on the query
  response <- httr::GET("https://www.healthplanet.jp/status/innerscan.json", query=query)

  #Convert the response into data.frame format in R
  content <- httr::content(response)
  df <- dplyr::bind_rows(content$data)
  df$date <- as.POSIXct(strptime(df$date, "%Y%m%d%H%M"))
  df$keydata <- as.numeric(df$keydata)
  df$tag <- stringr::str_replace_all(df$tag, table)
  cbind(
    sex=content$sex,
    birth_date=strptime(content$birth_date, "%Y%m%d"),
    height=as.numeric(content$height),
    tidyr::pivot_wider(df, names_from = tag, values_from=keydata))
}

#
#' Get Shinichi Takayanagi's innerscan data
#'
#' Get Shinichi Takayanagi's innerscan data via HelthPlanet API with Access Token.
#'
#' @export
stakaya <- function()
{
  token <- "1579156185763/66CktVaYJoijpcv4xpa6spQbVB7J6ZKdysduOg6v"
  innerscan(token)
}
