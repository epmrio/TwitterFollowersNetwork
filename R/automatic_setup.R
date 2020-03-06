# This function aims to automaticly prepare the scrap by settings up authorization, and loading necessary packages


auto_setup <- function(variables) {
  # Checking rtweet package and loading it
  require(rtweet)
  # Setting authentification using rtweet method
  cons_key <- readline(prompt = "copy/paste your API Key: ")
  cons_secret <- readline(prompt = "copy/paste your API Secret Key: ")
  acc_token <- readline(prompt = "copy/paste your Access Token: ")
  acc_secret <- readline(prompt = "copy/paste your Access Token Secret: ")
  app_twitter <- readline(prompt = "What is the name of your Twitter App ? ")
  # Using "create_token" function from rtweet to get Twitter authorization
  twitter_token <- create_token(app = app_twitter, consumer_key = cons_key, consumer_secret = cons_secret, access_token = acc_token, access_secret = acc_secret)
  # Checking if authentification is correct. Je dois trouver un autre moyen de vérifier si c'est correct + il faut que ça ne me fasse pas de retour dans le terminal
  test<-invisible(search_users("epmrio",token = twitter_token))
  if (nrow(test)==0) {
    print("wrong authentification, please try again")
  } else if (nrow(test)!=0) {
    print("Success ! Your credentials has been saved under the name 'twitter_token'")
  }
  return(twitter_token)
}

auto_setup()
