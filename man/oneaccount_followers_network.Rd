% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oneaccount_followers_network.R
\name{oneaccount_followers_network}
\alias{oneaccount_followers_network}
\title{Twitter followers network from one account}
\usage{
oneaccount_followers_network(x, token = NULL, max.accounts = 50000)
}
\arguments{
\item{x}{The screen name of ONE account from which the followers and following will be retreive}

\item{token}{A variable containing the twitter API credentials. If you used automatic_setup, you can put "twitter_token"}

\item{max.accounts}{A maximum number of followers/following. A list of 100 accounts can bring to a million followers/following to retreive for the network. Default is set to 50.000, which is roughly 1 hour long}
}
\value{
A dataframe with 2 columns : an Source node and Target node. This dataframe can be exported in csv and used in a network analysing software like Gephi
}
\description{
Create a network of Twitter followers from ONE account
}
\examples{

\dontrun{

## Get a network of the following list :
USER_NAME <- "account_name"
# Use the Function to retrieve the network
users_network <- oneaccount_followers_network(USER_NAME, token = twitter_token, max_accounts = 100000)
# Export the network to your computer and remove the first column (rownames)
write.csv(users_network, "/PATH_TO_DIRECTORY/users_network.csv", rownames = FALSE)

}

}
