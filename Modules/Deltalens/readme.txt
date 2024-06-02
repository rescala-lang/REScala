This module is based on quicklens.
To make the quicklens history available use the following:


# add the quicklens remote
git remote add quicklens git@github.com:softwaremill/quicklens.git

# fetch the commit we want to merge
git fetch quicklens d4c24cab20067b393985caf129c41c70f40a3640

# go to the starting point in this repository
git checkout d269a46d0138c2799f18b9168f23e2a95f415105

# potentially you need to clear the path
# rm -r Modules/Deltalens

# recreate the subtree merge
git subtree add --prefix="Modules/Deltalens" d4c24cab20067b393985caf129c41c70f40a3640

# store the generated ID somewhere (it’s different because of the different commit time and author)
set -l merged (git rev-parse HEAD)

# graft replace the history less merge with the subtree version
git replace --graft 30fbc6544bd803b894efc3bf993cd84705a92e6b $merged
