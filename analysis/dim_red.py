import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import seaborn as sns

%matplotlib inline

def PCA(inputdata):
  df = pd.read_csv("df1.csv")
  features = df.loc[:,'MOCA':'M4']
  pca = PCA(n_components = 2)
  principalComponents = pca.fit_transform(df.loc[:,'MOCA':'M4'])
  principalDf = pd.DataFrame(data = principalComponents
             , columns = ['principal component 1', 'principal component 2'])
  pca = PCA(random_state=1107)
  X_p = pca.fit_transform(features)
  pd.Series(np.cumsum(pca.explained_variance_ratio_))
  percent_variance = np.round(pca.explained_variance_ratio_* 100, decimals =2)
  columns = []
  for i in range(len(percent_variance)):
    columns.append(f'PC{i+1}')
  
  ax = plt.bar(x = range(len(percent_variance)), height=percent_variance, tick_label=columns)
  plt.ylabel('Percentate of Variance Explained')
  plt.xlabel('Principal Component')
  plt.title('PCA Scree Plot')
  plt.show()

  finalDataFrame = pd.concat([principalDf, df[['cluster']]], axis=1)
  fig, ax = plt.subplots(figsize=(20,16))
  sns.scatterplot(x='principal component 1', y='principal component 2', hue = 'cluster', data = finalDataFrame, s=50)
  plt.show()
