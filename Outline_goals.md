Outlining Goals and Objectives of Project

Data source reference: www.baseball-reference.com

Assumptions:
- Loading .csv from source to R (direct from .csv)
    - If major load issues occur, can add step .csv -> Excel -> R
- Cleaning of data in R
    - Non-sensical values, handle nulls, concatenated strings, etc.
- Only observing years 2000-2020
    - Do we want to include 2020?
        - Could include, but make note of COVID changes
- Only look at MVP, Cy Young, Rookie of the Year awards
- Use both AL (American League), NL (National League) leagues for comparison


High-Level Goals:
#1 : Compare historical award winners for MVP, Cy Young, Rookie of the Year
  - MVP and Rookie can be any player
  - Cy Young is only for pitchers
#2 : Building comparison diagram of award categories
  - Compare across selected players (3-5 players)
      - Web diagram based on user selection
  - Player comparison against league averages (compute total average?)
#3 : Create user web-portal to navigate data, diagrams, etc.
  - Selection for data and diagrams
      - By league, by award
      - Example -> Cy Young tab, sub tab of league?
  - Student interaction
      - Comparison of personal stats to professionals stats?


Why are analyzing MLB data?
There is a surplus of raw data from the MLB dating back 1876 which gives us plenty of statistics to look at.  We think this plethora of data could yield some beutiful displays of historical statistics as well as fun ways to predict awardees through various models.

How do we know if we'll be successful?
A functioninig website with displays of our findings, interactivity of the website and a user, and making the class go "WOW!"






