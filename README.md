# GoAvocado
For STA 523 final project

## Introduction

The Hass avocado with the dark green bumpy skin, weighed 200-300 grams, is one of the most popular fruit in the United States. 
It was originally grown in Southern California by Rudolph Hass, an amateur horticulturist. Nowadays, the Hass cultivar has accounted 
for more than 80% of the avocado crop in the United States and 95% in California (Wikipedia, 2018). A dataset with 18,249 records is 
obtained from Kaggle.com. Time series modeling is applied to predict the avocado price. Moreover, we scrap the avocado recipe 
information such as introduction and calories from loveonetoday.com, and use the obtained data to create a shiny app, from which 
users can search for the recipes that satisfy their needs. 

Note: since it takes a while to run the data scrapping code, we save the obtained data frame in an avocado_recipe.csv. The shiny app will read the saved csv file to get the data. 

## Shiny Dashboard

This shiny app is developed specifically for avocado lovers(just like us!) to filter avocado recipes from the whole 160 recipes we previously scrapped from https://loveonetoday.com/avocado-recipes/. 

To run this app, first, choose the `Search` tag, what you need to do is specifing the 1)calories range, 2)cooking time range and 3) whatever labels you like. By clicking the run button, you will then get all the avocado recipes meeting your requirements! You can get more information about each recipe from a dialog modal by clicking the link.

In the `Table` tag, you can view the detailed information for the filtered recipes in a table output.

In the `Codes` tag, you can view the codes we used to build this shiny dashboard.

