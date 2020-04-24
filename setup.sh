echo "Bootstrapping packrat..."
R -e "options(repos='https://cloud.r-project.org')" --args --bootstrap-packrat
echo "Restoring packrat..."
R -q -e "packrat::restore(overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE)"
echo "Setting upstream remote..."
git remote add upstream git@github.com:scRNA-tools/scRNA-tools.git
