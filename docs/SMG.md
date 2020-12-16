
# System Maintenance Guide 



## Using Azure

  - Log in to Azure portal and start [epiforecasts-general-purpose VM](https://portal.azure.com/#@epiforecastsoutlook.onmicrosoft.com/resource/subscriptions/2d9a656e-d2ff-4b55-9f35-99bddf874f1b/resourceGroups/epiforecasts-general-purpose/providers/Microsoft.Compute/virtualMachines/epiforecasts-general-purpose/overview)
  - Connect to the azure VM over ssh, with IP 13.92.183.137 and given username and password
  - Run: `sudo docker start epinow2`
  - Open a local internet browser, navigate to http://13.92.183.137:8787/, and login with epinow2
  - Use R Studio (checkout this repository, create forecasts as below)
  - When finished, reopen the ssh terminal (logged in to azure VM) and run `sudo docker stop epinow2`
  - Back in Azure portal, stop the VM

## Create fresh forecasts

For an end-to-end run:

- Run `bash/forecast.sh` from the terminal
- Check the output by looking plots saved in the most recent folder: `evaluation/plots/yyyy-mm-dd
- A properly formatted csv, ready to be submitted to the Reich forecast hub, will be saved in 
  - `submissions/submitted/yyyy-mm-dd-epiforecasts-ensemble1.csv`

Note that the final submission file is a QRA ensemble with weighted quantiles by default. To check or change this:
  - Open `submissions/finalise.R`
  - Set global variables:
    - alter line 12 to filter for a different model.
  - If doing this:
    - _Before running_ `bin/forecast.sh`: save the script and then run `bin/forecast.sh`
    - _After running_ `bin/forecast.sh`: save and then source the script.

## Runtime

Using the Epiforecasts azure VM (72 core), if `update.sh` runs perfectly then the runtime is typically 1-2 hours

## Debugging

- If `bin/forecast.sh` errors: open  as a script, run each command in sequence to check where it fails