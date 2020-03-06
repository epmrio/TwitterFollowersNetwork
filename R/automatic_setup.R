# This function aims to automaticly prepare the scrap by settings up authorization, and loading necessary packages

automatic_setup <- function(cons_key,cons_secret,acc_token,acc_secret,app_name) {
  # Checking rtweet package and loading it
  require(rtweet)
  # Setting authentification using rtweet method
  # Using "create_token" function from rtweet to get Twitter authorization
  twitter_token <- create_token(app = app_name, consumer_key = cons_key, consumer_secret = cons_secret, access_token = acc_token, access_secret = acc_secret)
  # Checking if authentification is correct. Je dois trouver un autre moyen de vérifier si c'est correct + il faut que ça ne me fasse pas de retour dans le terminal
  test<-invisible(search_users("epmrio",token = twitter_token))
  if (nrow(test)==0) {
    print("wrong authentification, please try again")
  } else if (nrow(test)!=0) {
    print("Success ! Your credentials has been saved under the name 'twitter_token'")
  }
  return(twitter_token)
}
