# PigPeaks
data-driven surveillance based on on-farm records from swine production farms.


When an animal owner uses WinPig as a herd management software, any backups are saved as a ".WPC" file. 
These codes will _not_ work directly from a .WPC file.

These codes will work in a computer where the software is installed. 
The software works by creating a SQL database in the backend. 
In the Settings of the program it is possibe to find the local address to that server. 
Within the server, the one or various farms being managed by the software will be available by the name given by the user.

Steps to use these codes:

## A) Set up the full application in a new computer, for the first time

1. open the *Settings* file, and provide at least _database connection settings_ (by looking for them in the WinPig settings). UNCOMMENT them.

2. open the *Definitions* file and check that you are comfortable with all defaults given, or change the as wished

3. Use the file *"Table_Indicators_CSV.csv"* to tailor which indicators should be used and subjected to syndromic surveillance (set to TRUE or FALSE)

4.Inside the folder R, run the file *"0-install.r"*. It is hidden there because you will only need to do this once, and you should have read the instructions to be able to do it. 

Note that this will create a folder called _data/_. That folder is declared as a folder to ignore, so that we can continue to share codes without sharing anyone's data.


## B) update the results systematically (run new syndromic surveillance when new data is available, active systematic monitoring)

1. Set up a task schedule or pipeline to run the file *"Pig Peaks eninge.r"* (root of the repository) as often as updates are desired. The codes are set up assuming a weekly update, with weeks starting on Monday (all can be changed in settings).

## C) run all codes once in order to see a dashboard with all indicators based on the total amount of data available (snapshot)

Steps 1-3 above

4.Run the file *"Data to Dashboard setup.r"*

## D) If you are not installing the application in any computer with real data, you just want to run the codes using the pseudo-data provided as example:

The data example provided (_Data_Example/farmExample.RData_) is just to show the STRUCTURE of the data, as we could not share data from real farms.

It is possible to plot and visualize all the RESULTS (already aggregated data) by looking at the (_Data_Example/indicatorsExample.RData_).

All the database connections settings have been commented out, so that if a database connection is not set, the codes just expect the data to already exist in a folder called data. If you have access to data from any farm, you can paste it there by copying the data tables directly form the SQL server, and pasting then in the folder following the structure provided as an example in (_Data_Example/farmExample.RData_) . 

One ther exists real data in the folder data,

RUn any of the scenarios above desired, always SKIPPING the file *"1-extract data from database.r"*
 


