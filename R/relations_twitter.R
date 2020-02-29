###########################################################################
#
#
# Ce script est écrit pour ytransformer la fonction et ne pas taper l'api #
# à chauqe recherche
#
###########################################################################

# On charge les packages une fois qu'ils sont installés
library(rtweet)
###
# MODIF POUR NE RÉCUPÉRER EN FILTRANT LES COMPTES LES PLUS GROS Récupérer les followers et les following d'un coup
###

## Le problème c'est que dans les 2 principaes fonctions que j'utilise, get_followers et lookup_users, on tape l'api avant de
# Pourvoir faire un tri. En gros il faut pouvoir trier avant. C'est surtout le get follower qui fait tout foirer. Du coup il faut
# que je trouve comment sélectionner à l'avance.
## Si on a une liste de base, il faudrait pouvoir faire défiler cette liste, checker les comptes certifier ou les comptes trop gros,
# et les supprimer de la liste. À partir de là, on pourra lancer la fonction normale.

## La question du compte certifié ne règle pas le problème des très gros comptes comme elon musk...
## Je vais faire un test d'abord du filtre de compte certifiés parce que c'ets le plus simple, puis le tri des gros comptes
## Pour le tri des gros comptes, j'aurai juste à les classer dans l'ordre et j'estime que si la requete est plus grosse que mettons
# 800000 comptes EN TOUT (~ 200 minutes) j'enlève les plus gros en partant du haut. Je pense qu'on ne perd pas beaucoup d'info car
# ce ne sont pas les plus gros compte suivi par une personne qui vont nous apprndre grand chose.


# Maintenant je dois écrire un code qui filtre une première fois la liste en enlevant les trop gros
liste_utilisateurs<-c("EricaChenoweth","Mehdi_Moussaid","VINCI_fr","cliffordstott","ProfJohnDrury","GKStill","enlOccurrence","DrAnneTempleton","Affluences_App","GTheraulaz","m2CrowdSafety","wwcrowds","qucit","DigitalBarriers","densityio","deepomatic","brickstream","CrowdControlGuy","CrowdSafetyMag","emmaparkinson","Ecocompteur","Irisys","IntCrowdCon","SMSStoretraffic","SenSource","crowd_matters","xovis_ag","Bert_Bruyninckx","Vaelsys","quantaflow_fr","RetailSensing","ProdcoAnalytics","TomasApeltauer","peoplecounting","quantaflow","Acorel_Comptage","3DCOUNTING","EURECAM","Foxstream_group","TCSBI")

## Cette fonction fonctionne !

get_twitter_network <- function(x,token=NULL,max.accounts=20000) {
  longueur_liste_utilisateurs<-length(x)
  # On récupère les infos basiques de nos comptes et on le range dans un df
  infos_liste_utilisateurs<-lookup_users(x, token = NULL)
  infos_liste_utilisateurs_ORDER<-infos_liste_utilisateurs[order(infos_liste_utilisateurs$followers_count,decreasing = TRUE),c("screen_name","followers_count","friends_count")]
  # on comptabilise le nombre total de follow.er.ing
  somme_friends_follow<-sum(infos_liste_utilisateurs$followers_count)+sum(infos_liste_utilisateurs$friends_count)
  # Maintenant on commence la condition si le nombre de comptes est trop grand, on crée un loop tant que c'est trop grand
  while (somme_friends_follow > max.accounts) {
    # on calcule une estimation du temps d'attente, en minutes. Tous les 14 comptes (environ), on tape l'API. Donc il faut savoir
    # combien de fois on a 14 comptes dans notres liste pour savoir combien de fois on va taper l'API. On multiplie ensuite par 15
    temps_attente_estime=round((longueur_liste_utilisateurs/12)*15,digits = 0)
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
      temps_attente_estime_REVU<-round((nrow(infos_liste_utilisateurs_ORDER)/12)*15,digits = 0)
      print(paste0("Le nouveau délai d'attente est estimé à ", temps_attente_estime_REVU, " minutes"))
      liste_utilisateurs_REVU<-infos_liste_utilisateurs_ORDER$screen_name
      longueur_liste_utilisateurs<-length(liste_utilisateurs_REVU)
      somme_friends_follow = sum(infos_liste_utilisateurs_ORDER$followers_count)+sum(infos_liste_utilisateurs_ORDER$friends_count)
    } else if (decision_1 == "no") {
      print(paste0("The process will continue with ", somme_friends_follow, " accounts and an estimated time of ", temps_attente_estime, " minutes. Hang in there..."))
      liste_utilisateurs_REVU<-liste_utilisateurs
      break
    }
  }
  # On continue la suite du code : on crée le dataframe dans lequel on va ranger les données
  followers_total<-as.data.frame(matrix(0, ncol = 2, nrow = 0))
  colnames(followers_total)<-c("Source","Target")
  friends_total<-as.data.frame(matrix(0, ncol = 2, nrow = 0))
  colnames(friends_total)<-c("Source","Target")
  compteur<-length(liste_utilisateurs_REVU)
  for (element in liste_utilisateurs_REVU) {
    print(paste0("nous sommes à ", element, ". Il reste ", compteur, " éléments à récupérer"))
    recup<-lookup_users(get_followers(element, n = 1000000, retryonratelimit = TRUE, parse = TRUE, verbose = TRUE, token = twitter_token)$user_id)
    followers<-data.frame()
    followers<-data.frame(Source=1:nrow(recup),Target=1:nrow(recup))
    followers$Source<-recup$screen_name
    followers$Target<-element
    followers_total<-rbind(followers_total,followers)
    recup<-lookup_users(get_friends(element, n = 1000000, retryonratelimit = TRUE, parse = TRUE, verbose = TRUE, token = twitter_token)$user_id)
    friends<-data.frame()
    friends<-data.frame(Source=1:nrow(recup),Target=1:nrow(recup))
    friends$Target<-recup$screen_name
    friends$Source<-element
    friends_total<-rbind(friends_total,friends)
    compteur<-compteur-1
  }
  base_totale<-rbind(followers_total,friends_total)
  return(base_totale)
}

test<-get_twitter_network(liste_utilisateurs,max.accounts = 100000)
View(test)

# La fonction a l'air bien comme ça. On peut ensuite envisager d'ajouter une option de temps (attention, risque de conflit avec
# l'option max.accounts)
