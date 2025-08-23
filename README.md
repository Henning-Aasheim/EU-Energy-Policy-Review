# EU-Energy-Policy-Review
This is a repository for files associated with the EU Energy Policy Review project. Throug the summer of 2025 we have gathered data on the energy policy of the EU memeber states &ndash; except Cyprus and Malta &ndash; as well as Norway, Switzerland, and the United Kingdom. This data is gathered on behalf of the [Wind In My BackYard (WIMBY)](https://wimby.eu/) project to create models for renewable energy build-out that is more in tune with social and political realities.

The main dataset (.csv) can be found here: [EU policy data](data/energy_review.csv)

The map below shows a cursory glance at the targets for each nation. They are divided into solar and onshore- and offshore wind, and for the years 2030, 2040, and 2050. Not all countries have targets, and some of the targets seen in this map are expected development paths, rather than explicit targets.[^1] Note that this map only shows some of the data. Some countries only have data up until 2030, others have only to 2035, etc. We have made the decision to only show the totals at the start of each decade to keep the figure readable. The specific information is available in our main dataset.

<p align='center'>
  <img src='/img/target_capacity.png' width=40%>
  <br>
  <i>Figure 1: target capacity map for 2030, 2040, and 2050 (GW)</i>
</p>

We also include another visualisation aid in Figure 2, which shows the targets of the respective countries in a bar chart. The chart is dominated by the large countries which does make it hard to read for smaller values, however, we have decided to present the targets as is to see the big picture of the capacity build-out. 


<p align='center'>
  <img src='/img/bar_chart.png' width=50%>
  <br>
  <i>Figure 2: Target capacity bar chart for latest observed data (2023), 2030, and 2040 (GW)</i>
</p>

## Regions
The countries are also divided into regions. This is mostly based on the regions by van Greevenbroek et al.[^2], with the countries not found in van Greevenbroek et al. added in, generally by our own understanding of their socio-geographic position. We are careful in nothing that this is not based on any objective criteria, like the structure of the European powergrid.

<p align='center'>
  <img src='/img/regions.png' width=40%>
  <br>
  <i>Figure 3: map of the regions</i>
</p>

[^1]: Our data includes a variable that excludes all values that are not concrete targets called *target_exp*, if this is a necessary difference to make.
[^2]: Koen van Greevenbroek et al., ‘Trading off Regional and Overall Energy System Design Flexibility in the Net-Zero Transition’, Nature Sustainability 8, no. 6 (2025): 629–41, https://doi.org/10.1038/s41893-025-01556-2.
