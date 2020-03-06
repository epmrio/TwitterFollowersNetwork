\name{relations_twitter}
\alias{relations_twitter}
\title{Get a twitter network}
\usage{
relations_twitter(
    x,
    token=NULL,
    max_accounts=50000
)
}
\arguments{
\item{x}{Screen name or list of screen names from which the followers will be retrieved.}

\item{token}{A Twitter API token is necessary to use this function. If you used auto_setup function, the token is under the name "twitter_token".}

\item{max_accounts}{The constrcution of the network can be very long for a big number of accounts (hours or maybe much more). To limit the time, you can setup a limit of number of accounts you want to stop to. If you reach this limit, the function will ask you if you want to remove the biggest accounts from your list}
}
\description{
Get a twitter network
}
\examples{
relations_twitter(liste_utilisateurs,token=twitter_token,max_accounts=100000)
}
