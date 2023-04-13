
require(ade4)

# Raw data
load('mbl_raw.rda')
load('wh_raw.rda')
load('td_raw.rda')

# Random data
load('mbl_rand.rda')
load('wh_rand.rda')
load('td_rand.rda')

# Gaussian data
load('mbl_gauss.rda')
load('wh_gauss.rda')
load('td_gauss.rda')

# feature matrix
load('features.rda')

# Check if the row names are identical
# identical(rownames(features), rownames(mbl_raw)) # TRUE
# identical(rownames(features), rownames(mbl_rand)) # TRUE
# identical(rownames(features), rownames(mbl_gauss)) # TRUE

mantel.randtest(dist(mbl_raw), dist(features), nrepet=1000)
mantel.randtest(dist(wh_raw), dist(features), nrepet=1000)
mantel.randtest(dist(td_raw), dist(features), nrepet=1000)

mantel.randtest(dist(mbl_rand), dist(features), nrepet=1000)
mantel.randtest(dist(wh_rand), dist(features), nrepet=1000)
mantel.randtest(dist(td_rand), dist(features), nrepet=1000)

mantel.randtest(dist(mbl_gauss), dist(features), nrepet=1000)
mantel.randtest(dist(wh_gauss), dist(features), nrepet=1000)
mantel.randtest(dist(td_gauss), dist(features), nrepet=1000)


