# votechange
Analyzing the Change in Total Votes Between 2016 and 2018

The results from the United States 2018 election allowed both political parties to claim victory. Republicians picked up seats in the Senate, while the Demoracts have secured a majority in the House. Was this election a split-chamber decision?

Micah Cohen of FiveThirtyEight, amongst others, has suggested that the House popular vote should be used as the sentiment of the country. Since voter registration and turnout varies so greatly, I'm not sure this is the right metric, but even if it is, the Electral College overrides the popular vote. For those interested in the strategy of the 2020 election, it's important to determine where and why the vote changed. Was it a change in sentiment in the country or a Trumpless ticket? 

Nate Silver started to address this question in an article on [November 8](https://fivethirtyeight.com/features/the-2018-map-looked-a-lot-like-2012-and-that-got-me-thinking-about-2020/) by using the 2018 House vote for the 2020 Electral College. I wanted to take the next step and analyze changes in vote totals in several key swing states.

For each congressional district, how did the total votes for each party change? Were the relative changes in Democratic and Republician votes similar or were they different? For those districts where the shift wasn't proprotional, could we attribute it to Trump? Does the difference in vote proportion between Trump and the 2016 candidate correlate with the 2018 difference? Does Trump score?

Election results where obtained from [MIT Elections Science and Data Lab](https://electionlab.mit.edu/). The 2016 results are in their [`elections` R package](https://github.com/MEDSL/elections), and 2018 Indiana and Pennsylvania from their [2018-elections Github repo](https://github.com/MEDSL/2018-elections). Due to issues with the raw files, other 2018 data was obtained from OpenSecrets and CNN. The [Trump Scores](https://projects.fivethirtyeight.com/congress-trump-score/house/) were [scrapped](https://github.com/peterwsetter/votechange/blob/master/get-data.R) from FiveThirtyEight.

This analysis will start with Indiana, Minnesota, and Pennsylvania. As results are available, I will expand the analysis to Arizonia, Iowa, Florida, Michigan, New Hampshire, North Carolina, Ohio, Texas, Wisconsin, and Virginia. 