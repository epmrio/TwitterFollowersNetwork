# TwitterFollowersNetwork

This package aims to create Twitter followers and following networks using R software and Twitter API.  
I've found that those function existed on Python but not R so I try to develop my own  

Since it is based on the API and that getting the names of accounts can be pretty heavy if there is a lot, the process is sometime very long : 20 minutes for ~ 15 accounts, hours for ~ 50 accounts and maybe days for more than 100 accounts  

Those function needs rtweet package in order to work properly  

Every comment or suggestion is welcome !

    

In order to work, you have to use automatic_setup once you've loaded the package  
  
You have to give your Twitter credentials to automatic_setup. It will then check if those are valid by trying to make a search in the API. This is why you'll get a message saying "searching tweets"  
  
Once automatic_setup is done you can use get_followers_network() function
