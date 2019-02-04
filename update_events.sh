set -e

cd Parks_PA
git checkout master
export GIT_SSH_COMMAND='ssh -i ~/.ssh/parks_deploy'


Rscript code/events.R

git commit -am "Update map"
git checkout gh-pages
git merge master
git checkout master
git push --all

echo 'Done'
