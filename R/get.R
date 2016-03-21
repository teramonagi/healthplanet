#
#' Get Access Token for Health Planet API.
#'
#' Get Access Token for Health Planet API. You need to have your own account on [Health Planet](https://www.healthplanet.jp/).
#'
#' @param user_id user id on [Health Planet](https://www.healthplanet.jp/)
#' @param user_password passward on [Health Planet](https://www.healthplanet.jp/)
#' @param client_id client_id for the application you registed on [Health Planet](https://www.healthplanet.jp/)
#' @param client_secret client_secret for the application you registed on [Health Planet](https://www.healthplanet.jp/)
#' @export
getToken <- function(user_id, user_password, client_id, client_secret){
  #Constants
  redirect_uri <- "https://www.healthplanet.jp/success.html"
  scope <- "innerscan,sphygmomanometer,pedometer,smug"
  uri <- sprintf(
    "https://www.healthplanet.jp/oauth/auth?client_id=%s&redirect_uri=%s&scope=%s&response_type=code",
    client_id, redirect_uri, scope)

  #Stop warnings...
  old <- options(warn = -1)
  #Login -> Accept the API -> Get the code for access token.
  page_login <- html_session(uri)
  form_login <- html_form(page_login)[[1]] %>% set_values(loginId=user_id, passwd=user_password)
  page_approval <- suppressMessages(submit_form(page_login, form_login))
  form_approval <- html_form(page_approval)[[1]] %>% set_values(approval="true")
  #Adhoc for rvest pakcage to misrecognized that there is a submit form...
  form_approval$fields[[3]] <- form_approval$fields[[1]]
  form_approval$fields[[3]]$type <- "submit"
  page_code <- suppressMessages(submit_form(page_approval, form_approval))
  code <- html_node(page_code, "#code") %>% html_text
  #Get Access token
  body <- list(
       client_id = client_id,
       client_secret = client_secret,
       redirect_uri = redirect_uri,
       code = code,
       grant_type="authorization_code")
  response <- POST(url="https://www.healthplanet.jp/oauth/token", body=body)
  #Recover warnings
  options(old)
  #Get access token from
  content(response)$access_token
}

#
#' Get the innerscan data
#'
#' Get the innerscan data via HelthPlanet API with Access Token.
#'
#' @param access_token the token given by getToken()
#' @export
get <- function(access_token)
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
    access_token=access_token,
    date="1",
    tag=tag)

  #Get response depending on the query
  response <- GET("https://www.healthplanet.jp/status/innerscan.json", query=query)

  #Convert the response into data.frame format in R
  content <- content(response)
  df <- rbind_all(content$data)
  df$date <- strptime(df$date, "%Y%m%d%H%M")
  df$keydata <- as.numeric(df$keydata)
  df$tag <- str_replace_all(df$tag, table)
  cbind(
    sex=content$sex,
    birth_date=strptime(content$birth_date, "%Y%m%d"),
    heght=content$height,
    spread(df, tag, keydata))
}
