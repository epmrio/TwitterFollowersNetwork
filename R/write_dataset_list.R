#' Write dataset list
#'
#' Exports the results of the get_followers_network function under 2 csv file in your working directory (!! May not yet work with windows)
#'
#' @param dataframes_list The result of the get_followers_network function. A variable consisting in a list of two dataframes : one for the nodes of the network and the other for the edges
#'
#' @return  The csv files are exported in the working directory and can be then used in a network analysing software like Gephi
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Get a network of the following list :
#' network <- get_followers_network(users_list, token = twitter_token, max_accounts = 100000)
#' # Export the network to your computer and remove the first column (rownames)
#' write_dataset_list<-(network)
#'
#' }
#'
#' @export
write_dataset_list <- function(dataframes_list) {
  path_1<-getwd()
  name<-c("edges_table","nodes_table")
  compteur<-1
  for (df in x) {
    write.csv(df,paste0(path_1,"/",name[compteur],".csv"),row.names = FALSE)
    compteur=compteur+1
  }
}
