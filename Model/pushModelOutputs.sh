# This script pushes the model output file contents from the local machine to Graham.
# To use, change "hbrauner" to your username, and update the file paths
# Note: each iteration will require you to enter your Graham password unless you've connected your ssh key to Graham.

# Enter your model scenario numbers / names here (ensure you have a consistent format for the "scenario" name following)
nums='1 2 3 1-P 2-P'
user=username
project_folder=foldername

for scenario_num in $nums
do
  scenario="Scenario${scenario_num}"

#Switch between dry run and regular by commenting/uncommenting the lines below

#Dry Run
rsync -rultvzi --dry-run $scenario/Output/ $user@graham.computecanada.ca:~/projects/rpp-hwheater/username/${project_folder}/Model/${scenario}/Output

#Regular
#rsync -rultvzi $scenario/Output/ $user@graham.computecanada.ca:~/projects/rpp-hwheater/username/$project_folder/Model/$scenario/Output

done
