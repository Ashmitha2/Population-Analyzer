# PopulationAnalyzer
An application designed to automate CRUD operations and develop interactive dashboards to visualize global population trends

What it does?

The project is developed using R Shiny for the front end, Javascript, CSS, HTML for styling and validations and MySQL Workbench for Database management. The system lets the user interact with database using an UI to insert, delete, update or retrieve data from different tables in the database via different tab panels. The system is validated to accept and process appropriate datatypes without NULL values. It also provides an interactive dashbaord that lets the users to visually analyze the world population trends across the different continents, gender and age groups.

How is the Database organised?

The data is stored in 6 SQL tables corresponding the the different continents namely Asia, Eurpoe, North America, South America, Africa and Oceania. Each table contains information about the countries in that particular continent along the following columns :
-> Rank
-> CCA3
-> Continent
-> Country/Territory
-> Area
-> Density
-> Growth Rate
-> World Population Percentage
-> MalePopulation
-> WorkingClassPopulation
-> FemalePopulation
-> ChildrenPopulation
-> TotalPopulation

How is the system designed?

The UI and its corresponding server code can be viewed in the file app.R that contains the code with necessary validations and the command to run the shiny App
The User Interface contains the following menu items each of which renders an UI as a fluidPage:
-> Home Page
-> View Tables
-> Update Tables
-> Insert Entries
-> Delete Entries
-> Dashboard
-> Map
