# Schwenkler-Zheng-2019
Public codes for my paper 'The Network of Firms Implied by the News'.

#Steps_1_and_2.R
This code uses open source NLP toolkit to identify named entities that belong to 'Organization' in sentences. Then we develop a methodology to further stemmize the results. For example, 'Toyota Motor' and 'Toyota' should be the same company, therefore their stems should be 'Toyota'.

#Step_3.R
After we have stemmized results, we match them to Compustat/CRSP database in order to get their official company names and tickers.
