A Markov Decision Process modeling approach to Parkinson Disease 

23 features used

Concomitant medication log

Features
Non-motor
-Neuropsychological 신경심리
Benton Judgment of Line orientation - 공간지각
Hopkins verbal learning test - 인지능력
Symbol Digit Modalities test - 
Modified Semantic Fluency - 
Montreal Cognitive Assessment - 
-Neurobehavior 신경행동
Geriatic depression scale - 기분/우울
State-Trait Anxiety Inventory - 
QUIP
-Sleep disorder 수면
Epworth Sleepiness Scale - 피곤함
-Autonomic  자율
SCOPA-AUT - 
RBD
LNS
UPSIT
Motor
MDS UPDRS1
MDS UPDRS2
MDS UPDRS3
MDS UPDRS4

Patient info
death
age
diagnosis
education


additional columns:
DATE  - YYMM format of date : June 2017 -> 1706
DATE2 - column DATE where 12 month mapped to scale of 100 : 1706 -> 1750
DATE_I - Date interval, the time passed since the prior visit
NUM - visit number, integer from 1
N_MAX - the total number of visits for that patient


376 ( recheck) patients who have been recorded for more than 7+ years are selected from PPMI dataset

dataset is configed to

visualization by
 - dimesnion reduction : PCA / t-SNE / UMAP (PCA works the best)
 - heatmap summarizing the clusters
 - 
