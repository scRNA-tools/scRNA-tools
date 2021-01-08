echo "Installing renv..."
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"
echo "Restoring renv..."
Rscript -e "renv::restore()"
echo "Setting upstream remote..."
git remote add upstream git@github.com:scRNA-tools/scRNA-tools.git
