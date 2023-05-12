# A Markov Decision Process modeling approach to Parkinson Disease 



## Introduction
 Parkinson is a chronic disease, meaning patients usually suffer from it for a very long time.  Normally a perfect cure does not exsist and patients focus on maintaining the current state, making sure it does not get worse.  In this effort both medication and regualar life pattern plays a major role.  Markov models show advantage in explaining long-term series of events, and since medication plays a major role this model is expanded into a Markov Decision Model.
 
 This study focuses on trying to explain the progress of Parkinson disease using MDP.  Data 

## Data
Features

* MDS UPDRS1
* MDS UPDRS2
* MDS UPDRS3
* MDS UPDRS4

 Patient Info
* death
* age
* diagnosis
* education
 
 Medication
* LEDD concomitant medication


## Preprocessing
 additional columns are created for further research
* DATE  - YYMM format of date : June 2017 -> 1706
* DATE2 - column DATE where 12 month mapped to scale of 100 : 1706 -> 1750
* DATE_I - Date interval, the time passed since the prior visit
* NUM - visit number, integer from 1
* N_MAX - the total number of visits for a particular patient
 
## Project Pipeline

* 376 ( recheck) patients who have been recorded for more than 7+ years are selected from PPMI dataset
* dataset is changed to a yearly-scale, features and previous medication is used to define the current state, medication used afterwards defines the action
* k-means clustering approach
* dimesion reduction : PCA / t-SNE / UMAP (PCA works the best) -> visualization of clusters
* heatmap summarizing/visualizing the clusters 
* state holding time + state transition probability tested 
* MDP based explanation

## Methods Used
* PCA
* K-mean clustering
* Markov Decision Process
