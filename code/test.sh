set -e


git checkout gh-pages

Rscript code/events.R

git commit -am "Update map"
git push
git checkout master
