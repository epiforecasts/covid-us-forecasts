#!bin/bash

#define date
ForecastDate=$(date +'%Y-%m-%d')

# Clone the hub repository if not already present
#git clone --depth 1 https://github.com/reichlab/covid19-forecast-hub.git

# install GitHub CLI
# https://cli.github.com/

# Authenticate with GitHub
# gh auth login

# Update the hub repository
cd ../covid19-forecast-hub
git checkout master 
git pull 
# Switch to submission branch
git checkout -b submission
git merge -Xtheirs master

# Move back into forecast repository
cd ../covid-us-forecasts

# Copy your forecast from local folder to submission folder
cp -R -f "./submissions/submitted/$ForecastDate-epiforecasts-ensemble1.csv" \
      "../covid19-forecast-hub/data-processed/epiforecasts-ensemble1/"

# Commit submission to branch
cd ../covid19-forecast-hub
git add --all
git commit -m "submission"

# Create PR
gh pr create --title "$ForecastDate - EpiForecast submission" --body "This is an automated submission."

# Remove local submission branch 
git checkout master
git branch -D submission
cd ../covid-us-forecasts