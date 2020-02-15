# Guide et méthode pour implémenter une ACP

## STEP 1: STANDARDIZE VARIABLES

from sklearn.preprocessing import StandardScaler
X = StandardScaler().fit_transform(X)

## STEP 2: DIRECT METHOD WITH SKLEARN

from sklearn.decomposition import PCA
pca = PCA()

* Do the PCA: X_pca = pca.fit_transform(X) ; X_pca is the transfer matrix. This means we can capture the principal components thanks to this !
    * Each component is expressed according to the different features.

* Capture the explained variance : explained_variance = explained_variance_ratio_

