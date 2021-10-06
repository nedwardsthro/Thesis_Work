# Thesis_Work

9/21/21 - Downloaded Basketball Reference Data and set up github

603 rows of traded players, 39 double traded, 1 triple traded
603+39+1
643 +4762

5512 in the original Sloan Article. there are 5405 in ours. In data that matters (players with more than 30 games), we have slightly 68 more observations, likely again due to the traded feature, but working the opposite direction now. Say a player plays 25 games with one team and 25 games with another. The Sloan article won't pick that player up because he didn't play more than 30 games for either team. We will pick that player up because it will show up as 50 games for us. 

Because of the data structure (our data having one line per season even if the player is traded vs. their data likely having multiple lines per season if the player is traded), our density plots will be different.

Update 10/6 below

See new Geom_density with scraped data

Used Mclust
Also useful mclustBIC and mclustBICupdate
mclustModel

Potentially looking into doing dimension reduction in the future?


