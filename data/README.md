# Files used for this analysis

In this folder you will find all the data I have used for this analysis.

## Raw Folder

This folder contains the original file.

| Variable | Description |
| --- | --- |
| Date.Day | The day of the month that this performance's week ended on |
| Date.Full | The full date representation that this performance's week ended on in "Month/Day/Year" format |
| Date.Month | 	The numeric month that this performance's week ended in (1 = January, 2 = February, etc.) |
| Date.Year | The year that this week of performances occurred in |
| Show.Name | The name of the production |
| Show.Theater | The name of the theatre |
| Show.Type | Whether it is a "Musical", "Play", or "Special" |
| Statistics.Attendance | The total number of people who attended performances over the week |
| Statistics.Capacity | The percentage of the theatre that was filled during that week |
| Statistics.Gross | The "Gross Gross" of this performance, or how much it made in total across the entire week. Measured in dollars |
| Statistics.Gross Potential | 	The Gross Potential is the maximum amount an engagement can possibly earn based on calculations involving ticket prices, seating capacity, and the number of performances. This number is expressed here as a percentage of what could have been achieved (Gross Gross / Gross Potential). In case the GP could not be calculated, it was replaced with 0% |
| Statistics.Performances | The number of performances that occurred this week |

## Clean Folder

In this folder you can find the final data table created through the cleaning process.
It also includes the train and test datasets. These are the same as the original, just the number of observations are split up.

| Variable | Description |
| --- | --- |
| show_name | The name of the production  |
| show_type | Whether it is a "Musical", "Play", or "Special" |
| num_of_attendance | The total number of people who attended performances over the duration of the production |
| occupancy_percentage | The average percentage of the theatre that was filled during the duration of the production |
| revenue | The "Gross Gross" of this performance, or how much it made in total across the duration of the production. Measured in dollars |
| percentage of_poss_revenue | The Gross Potential is the maximum amount an engagement can possibly earn based on calculations involving ticket prices, seating capacity, and the number of performances. |
| num_of_performances | Total number of performances during the run of the show |
