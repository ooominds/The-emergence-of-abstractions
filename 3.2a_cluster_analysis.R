
require(pvclust)

########################################################################
### Cluster Analysis & Dendrogram for Widrow-Hoff confusion matrices ###
########################################################################

# Raw dataset
load('wh_raw.rda')
wh_raw.clust = pvclust(t(wh_raw),
    method.dist='euclidean', method.hclust='ward.D2', quiet=TRUE)
plot(wh_raw.clust, main='', sub='', xlab='')

#########################################################################
### Cluster Analysis & Dendrogram for Memory-Based confusion matrices ###
#########################################################################

# Raw dataset
load('mbl_raw.rda')
mbl_raw.clust = pvclust(t(mbl_raw),
    method.dist='euclidean', method.hclust='ward.D2', quiet=TRUE)
plot(mbl_raw.clust, main='', sub='', xlab='')

