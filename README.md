# About the project
The main goal of this project was to provide Expedia return users with cutomized hotel recommendations to increase booking rates. We segmented users into three groups using clustering methods and accordingly customized recommendations.

# Methodology
* Data was provided by [Expedia](https://www.expedia.com/) (data size: 1.3GB)
* Reduced data dimensionality with PCA
* Used k-means clustering algorithm using three principle components
* Ran algorithm several times to ensure robustness
* Picked number of clusters using within groups sum of squares and elbow method (# of cluster: 3)
* Visualized clusters with 3D scatter plot using Plotly
* Identified users' behavior within each group and customized recommendations correspondingly

This project was done in 48 hours, and was presented to statisticians and Expedia data scientists. <br />

# Further Details
For more information: [Deck](https://github.com/jhuo831alex/DataFest2017_Expedia/blob/master/Presentation%20Deck.pdf)

# About DataFest
ASA DataFestTM is a data hackathon for undergraduate students, sponsored by the American Statistical Association and founded at UCLA, in 2011. <br /> <br />
For more information: [DataFest@UCLA](http://datafest.stat.ucla.edu/)

