set -e

cd Parks_PA

export GIT_SSH_COMMAND='ssh -i ~/.ssh/parks_deploy'

git checkout gh-pages

Rscript code/events.R

git commit -am "Update map"
git checkout master
git merge gh-pages

git push --all

echo 'Done'
