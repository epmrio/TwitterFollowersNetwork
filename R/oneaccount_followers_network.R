#' Twitter followers network
#'
#' Create a network of Twitter followers from a one person or account
#'
#' @param x The screennam of ONE account from whom the followers and following will be retreive
#' @param token A variable containing the twitter API credentials. If you used automatic_setup, you can put "twitter_token"
#' @param max.accounts A maximum number of followers/following. If the account has many followers, it can bring to a million followers/following to retreive for the network. Default is set to 50.000, which is roughly 1 hour long
#'
#' @return A dataframe with 2 columns : an Source node and Target node. This dataframe can be exported in csv and used in a network analysing software like Gephi
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Get a network of the following list :
#' ACCOUNT_SCREEN_NAME <- "account-1"
#' # Use the Function to retrieve the network
#' users_network <- oneaccount_followers_network(ACCOUNT_SCREEN_NAME, token = twitter_token, max_accounts = 100000)
#' # Export the network to your computer and remove the first column (rownames)
#' write.csv(users_network, "/PATH_TO_DIRECTORY/users_network.csv", rownames = FALSE)
#'
#' }
#'
#' @export

oneaccount_followers_network <- function(x,token=NULL,max.accounts=50000) {
  require(rtweet)
  # On récupère les infos basiques de nos comptes et on le range dans un df
  infos_liste_utilisateurs<-lookup_users(get_followers(x,n=1000000000,retryonratelimit = TRUE,parse = TRUE)$user_id)
  infos_liste_utilisateurs_ORDER<-infos_liste_utilisateurs[order(infos_liste_utilisateurs$followers_count,decreasing = TRUE),c("screen_name","followers_count","friends_count")]
  donnees<-as.vector(as.character(infos_liste_utilisateurs$screen_name))
  longueur_liste_utilisateurs<-length(donnees)
  # on comptabilise le nombre total de follow.er.ing
  somme_friends_follow<-sum(infos_liste_utilisateurs$followers_count)+sum(infos_liste_utilisateurs$friends_count)
  # Il faut ici ajouter liste utilisateurs revu. Si on ne l'ajoute pas ici, le script bug lorsque les comptes à récup sont plus petits
  # que la limite initiale
  liste_utilisateurs_REVU<-infos_liste_utilisateurs_ORDER$screen_name
  ## On met ici une première estimation du temps pour que ça le fasse même si le nombre est plus petit que la limite
  #temps_attente_estime=round((longueur_liste_utilisateurs/12)*15,digits = 0)
  # autre manière d'exprimer le temps
  temps_attente_estime=round((somme_friends_follow/5000)*2,digits = 0)
  print(paste0("Le nombre de comptes à récupérer correspond à ", somme_friends_follow, ". Le délai d'attente est estimé à environ ", temps_attente_estime, " minutes"))
  # Maintenant on commence la condition si le nombre de comptes est trop grand, on crée un loop tant que c'est trop grand
  while (somme_friends_follow > max.accounts) {
    # on calcule une estimation du temps d'attente, en minutes. Tous les 14 comptes (environ), on tape l'API. Donc il faut savoir
    # combien de fois on a 14 comptes dans notres liste pour savoir combien de fois on va taper l'API. On multiplie ensuite par 15
    # temps_attente_estime=round((longueur_liste_utilisateurs/12)*15,digits = 0)
    # autre manière d'exprimer le temps
    temps_attente_estime=round((somme_friends_follow/5000)*2,digits = 0)
    print(paste0("Le nombre de comptes à récupérer est supérieur à ", max.accounts, " (", somme_friends_follow, ").", " Le délai d'attente est estimé à environ ", temps_attente_estime, " minutes"))
    print(paste0("Le compte le plus gros est ", infos_liste_utilisateurs_ORDER$screen_name[1], ". Il comptabilise en tout ", infos_liste_utilisateurs_ORDER$followers_count[1], " followers. Souhaitez vous le retirer de la liste afin de réduire le temps de récupération ?"))
    # On intègre un input de l'usager pour qu'il dise si il veut enlever les comptes trop gros ou non
    decision_1 <- readline(prompt="Type yes for removal - Type no to continue: ")
    # Si il répond oui, on retire le premier compte et on lui présente le nouveau temps d'attente
    if (decision_1 == "yes") {
      # J'ai modifié ici un élément, j'ai écrit nrow(infos_liste_utilisateurs_ORDER). Avant il y avait la longueur de la liste
      # le problème c'est que avec la longueur de la liste, si il y avait des faux comptes ou des comptes mal écris ou qui
      # n'existent pas, ça faisiat bugger le process. La différence ici c'est que les comptes qui n'existent pas ne sont pas
      # intégré au dataframe au moment de lookup_users. Donc en me basant sur la longueur du dataframe créé et pas de la liste
      # de départ, je contourne le problème des comptes mal orthographiés
      infos_liste_utilisateurs_ORDER<-infos_liste_utilisateurs_ORDER[2:nrow(infos_liste_utilisateurs_ORDER),]
      somme_friends_follow = sum(infos_liste_utilisateurs_ORDER$followers_count)+sum(infos_liste_utilisateurs_ORDER$friends_count)
      temps_attente_estime_REVU<-round((somme_friends_follow/5000)*2,digits = 0)
      print(paste0("Le nouveau délai d'attente est estimé à ", temps_attente_estime_REVU, " minutes"))
      longueur_liste_utilisateurs<-nrow(infos_liste_utilisateurs_ORDER)
    } else if (decision_1 == "no") {
      print(paste0("The process will continue with ", somme_friends_follow, " accounts and an estimated time of ", temps_attente_estime, " minutes. Hang in there..."))
      break
    }
  }
  # On continue la suite du code : on crée le dataframe dans lequel on va ranger les données
  followers_total<-as.data.frame(matrix(0, ncol = 2, nrow = 0))
  colnames(followers_total)<-c("Source","Target")
  friends_total<-as.data.frame(matrix(0, ncol = 2, nrow = 0))
  colnames(friends_total)<-c("Source","Target")
  #####
  #On stocke les infos liées à la liste initiale
  accounts_list_init<-lookup_users(x)
  accounts_list_init<-accounts_list_init[,c("screen_name","source","name","location","protected","followers_count","friends_count","listed_count","statuses_count","favourites_count","account_created_at","verified")]
  colnames(accounts_list_init)<-c("Id","source","name","location","protected","followers_count","friends_count","listed_count","statuses_count","favourites_count","account_created_at","verified")
  # Je rajoute ici un dataframe qui récupérera toutes les infos sur les différents comptes
  accounts_total<-as.data.frame(matrix(0, ncol = 12, nrow = 0))
  colnames(accounts_total)<-c("Id","source","name","location","protected","followers_count","friends_count","listed_count","statuses_count","favourites_count","account_created_at","verified")
  #####
  liste_utilisateurs_REVU<-infos_liste_utilisateurs_ORDER$screen_name
  compteur<-length(liste_utilisateurs_REVU)
  for (element in liste_utilisateurs_REVU) {
    print(paste0("nous sommes à ", element, ". Il reste ", compteur, " éléments à récupérer"))
    try(recup<-lookup_users(get_followers(element, n = 1000000000, retryonratelimit = TRUE, parse = TRUE, verbose = TRUE, token = NULL)$user_id))
    followers<-data.frame()
    followers<-data.frame(Source=1:nrow(recup),Target=1:nrow(recup))
    followers$Source<-recup$screen_name
    followers$Target<-element
    followers_total<-rbind(followers_total,followers)
    #####
    # Je complète ici le df
    accounts<-data.frame()
    accounts<-data.frame(Id=1:nrow(recup),source=1:nrow(recup),name=1:nrow(recup),location=1:nrow(recup),protected=1:nrow(recup),followers_count=1:nrow(recup),friends_count=1:nrow(recup),listed_count=1:nrow(recup),statuses_count=1:nrow(recup),favourites_count=1:nrow(recup),account_created_at=1:nrow(recup),verified=1:nrow(recup))
    accounts$Id<-recup$screen_name
    accounts$source<-recup$source
    accounts$name<-recup$name
    accounts$location<-recup$location
    accounts$protected<-recup$protected
    accounts$followers_count<-recup$followers_count
    accounts$friends_count<-recup$friends_count
    accounts$listed_count<-recup$listed_count
    accounts$statuses_count<-recup$statuses_count
    accounts$favourites_count<-recup$favourites_count
    accounts$account_created_at<-recup$account_created_at
    accounts$verified<-recup$verified
    accounts_total<-rbind(accounts_total,accounts)
    #####
    try(recup<-lookup_users(get_friends(element, n = 1000000000, retryonratelimit = TRUE, parse = TRUE, verbose = TRUE, token = NULL)$user_id))
    friends<-data.frame()
    friends<-data.frame(Source=1:nrow(recup),Target=1:nrow(recup))
    friends$Target<-recup$screen_name
    friends$Source<-element
    friends_total<-rbind(friends_total,friends)
    compteur<-compteur-1
  }
  edges_table<-rbind(followers_total,friends_total)
  print("The process is over. Even if some errors might have occurred, you should now have a file containing 2 dataframes : one Node Dataframes (accounts to be shown on the graph) and one Edges dataframes (links between accounts on the graph). You can save them on your computer by using the write_dataset_list function and open them in a graph visualisation software such as Gephi")
  accounts_total<-rbind(accounts_total,accounts_list_init)
  node_table<-accounts_total[!duplicated(accounts_total$Id),]
  return(list(edges_table,node_table))
}
