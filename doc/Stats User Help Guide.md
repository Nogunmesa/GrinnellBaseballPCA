**User Help Guide**   
***Getting Started***

* ***How Do I Switch Between Pitchers and Hitters?***   
  * Underneath Grinnell PCA Explorer sidebar under select player type you can choose between pitcher and hitter  
  * If you want to select for a pitcher, make sure you click the blue dot that selects pitcher   
  * If you want to select for a hitter, make sure you click the blue dot that selects hitter   
* ***How do I Add a Year?***   
  * Click on the getting started tab in the sidebar  
  * Underneath step 1, select a year to add and input the year you want to add  
* ***How do I Download a Dataset with New Years?***  
  * After the user uploads data they can press on the download file underneath the input year box   
  * They will click on download HitterName file if they are on the hitters tab and the download PitcherName file if they are on the pitchers tab   
* ***How Do I View Past Years without uploading?***   
  * If you click on the interactive graphs tab in the side panel, the user should be able to view the previously updated data 

***Upload Data***

* ***How Do I Prep Dataset to Upload?***   
  * You only want to upload a dataset if you added years and downloaded its  csv (see *How Do I Add a Year* and *How Do I Download a Dataset with New Years* underneath the Getting Started help guide)  
  * When you click to download the year the dataset will give the user a dataset with all of the names and NA ids   
* ***How Do I Add ID numbers to players in the Dataset you Created?***   
  * To add an ID number to the dataset you created you’re going to have to go to the Inside Edge website   
  * In the Inside Edge website search up the players name  
  * Then open any of the tables that come up  
  * When opening one of the tables you’ll want to the copy the url from one of the tables and you’ll see something like this in the url  
  * [https://quest.inside-edge.com/HitterEvaluationReport/HitterEvaluation?id=86389\&reg\_season\_start=2025-01-01\&reg\_season\_end=2025-12-31\&last\_season\_reg\_season\_start=undefined\&last\_season\_reg\_season\_end=undefined\&drange=undefined\&vs=B\&mlb\_sportcode=amateur\&StatYear=2025\&GameIds=\&GameDates=\&charterteamid=15\#panelShareButtonsList](https://quest.inside-edge.com/HitterEvaluationReport/HitterEvaluation?id=86389&reg_season_start=2025-01-01&reg_season_end=2025-12-31&last_season_reg_season_start=undefined&last_season_reg_season_end=undefined&drange=undefined&vs=B&mlb_sportcode=amateur&StatYear=2025&GameIds=&GameDates=&charterteamid=15#panelShareButtonsList)  
  * The url is a bit confusing, but the only relevant part you need to worry about is the id number  
  * In this example, we are looking at Sean Tashjian’s id number and his [id=86389](https://quest.inside-edge.com/HitterEvaluationReport/HitterEvaluation?id=86389&reg_season_start=2025-01-01&reg_season_end=2025-12-31&last_season_reg_season_start=undefined&last_season_reg_season_end=undefined&drange=undefined&vs=B&mlb_sportcode=amateur&StatYear=2025&GameIds=&GameDates=&charterteamid=15#panelShareButtonsList) (so you would want to replace the NA value next to Sean Tashjian’s name with [86389](https://quest.inside-edge.com/HitterEvaluationReport/HitterEvaluation?id=86389&reg_season_start=2025-01-01&reg_season_end=2025-12-31&last_season_reg_season_start=undefined&last_season_reg_season_end=undefined&drange=undefined&vs=B&mlb_sportcode=amateur&StatYear=2025&GameIds=&GameDates=&charterteamid=15#panelShareButtonsList))  
  * In the csv or datafile where the NAs that it created, you will want to replace the NA value with the corresponding ID value to the corresponding name for each player   
* ***How Do I Upload a Dataset?***   
  * Navigate to the upload data part in the sidebar   
  * You will want to upload a dataset for the new year that you just created  
  * This will add to our preexisting years of 2015-2023 (excluding 2020 because of COVID)  
* ***Why Am I Getting Kicked Off of the Server?***   
  * You are probably getting kicked off of the server if you have one or more NAs in your dataset   
  * Make sure that you entered all of the ids from Inside Edge for the players and that there are no NAs  
* ***Why Is the Year I Added Data For Not Adding to the Dataset?***   
  * If the year is not adding to the dataset, it is most likely because you are uploading the wrong file   
  * For example, if you upload a pitcher file, but are on the hitters tab then the data won’t upload to the graphs   
  * If you upload a hitter file, but are on the pitchers tab then the data won’t upload to the graphs either

***Interactive Graphs***

* ***How do I View Graphs?***  
  * Click the interactive graphs tab in the side panel    
* ***How Do I Change which Axis the PCAs are on for the Biplot and PCA plot?***   
  * There are 3 PCAs which are representing the players’ different skills stored in our website  
  * In the sidebar inside the interactive tab there is an X variable and Y Variable selection  
  * Underneath both of these variables, the user can choose either PC1, PC2, or PC3 to be on the X axis or Y axis  
  * The goal of the PCAs is to see the relationship between two different skills   
    * For example, you want to see the relationships between PC1 and PC2, PC2 and PC3, and PC1 and PC3  
    * You DON’T want to plot the same PCs on both the x and y axis. For example, plotting PC1 on both the x and y axis won’t tell you anything  
* ***How do I Change the Range of Years?***   
  * In the sidebar inside the interactive tab, there is a tab that selects a start year and end year   
  * You can enter a start year and end year   
* ***Can I View for Only One Year?***   
  * Yes you can view for only one year if you put the same start and end year   
* ***Can I View and Compare Two Years?***   
  * You can compare two years if they are in the same range and consecutive  
  * For example if the range is 2016-2017, then you can compare 2 years   
  * However, if the range is 2022-2024 and you want to compare it to the 2016 team the app doesn’t allow the user to do this   
* ***How Do I Remove Certain Players From a Year?***   
  * If you want to remove certain players from a year  
* ***How Do I Add and Remove Players to a Graph?***   
  * Locate all of the player names under select players to display  
  * If you uncheck a player’s checkbox, the player will be removed from the dataset  
  * If you check a player’s checkbox, the player will be added to the dataset  
  * **CAUTION**  
    * If you remove a player it will automatically update the PCA plot in the PCA plot tab  
    * However, when you remove a player on the density and hover plot you will have to click the button on the corresponding plot before it updates  
* ***How do I Interpret The Biplots?***  
  * Underneath the biplots, there is a description about what the arrows represent in relation to each variable    
* ***How do I Remove Names from PCA plot?***   
  * Locate the show labels button In the sidebar underneath end year in the side panel for interactive graphs  
  * **NOTE**  
    * Show labels only works for the PCA plot in the PCA tab   
    * If you uncheck the show labels button, you can remove the names only for the PCA plot in the PCA tab  
    * If you check the show labels button you can remove the names only for the PCA plot in the PC  tab  
* ***How Do I change which PCAs I’m Viewing in the Density Plots and Hover Plots?***  
  * In both the Density and Hover Plots, there are 3 corresponding buttons at the top: PC1, PC2, and PC3  
  * Click on the corresponding PC1, PC2, or PC3 button to see trends for specific skills   
  * **NOTE**  
    * If you use the same exact set of players, the PCAs will be the same for the same for both density plots and hover plots  
    * However, if you change the amount of players you’re analyzing the PCAs may change   
* ***How Do I Change the Bandwidth of the Density Plot?***  
  * At the bottom of the tab after all of the player’s names are displayed there is a label called density bandwidth  
  * If you want the density plot to be thinner drag the cursor to closer to 0.1 and if you want the density plot to be thicker drag the cursor to be closer to 3  
  * To repopulate the bandwidth of the density plots, make sure you are on the density plot between the PCA plot and Hover plot   
  * For the density plot to repopulate, click on the corresponding density plot button of PC1, PC2, or PC3 (see *How Do I change which PCAs I’m Viewing in the Density Plots and Hover Plots?**)***  
    

