# January-2020-to-Present----Criminal-Strategies-Unit----State-s-Attorney-s-Office-of-Baltimore-City
 
Summary: Technical projects I have worked on as a Community Liaison for the Baltimore City State's Attorney's Office.

Installation: Each lettered header represents a different folder and a different project.

A. crime_map
	
	This Shiny App visualizes the Victim-based Crime data released by the Baltimore City Police Department (BPD).
	
	1. The app is built using the 'app.R' and helper.R' files
	2. These files can be found within the 'crime_map' folder
	3. The 'api.login.txt' file contains the log-in information for OpenBaltimore's API. This access is needed to gather the crime data for the map.
	4. The 'Baltimore City District Boundary Map.kml' file is a mapping file used to draw the city police district boundaries on the map.
	5. The app is hosted by RStudio Connect at jmathena1.shinyapps.io/crime_map
	6. To install the files neccessary to run the app locally:
		i. Download the crime_map folder
		ii. Open RStudio
		iii. Open the 'app.R' file
		iv. Click the 'Run App' button next to the green arrow at the top of the file editor panel to run the app