user=hbrauner
path=projects/rpp-hwheater/hbrauner/MESH_Project_Baker_Creek/Data/Raw

rsync -rultvzi --dry-run $user@graham.computecanada.ca:~/$path/ ./Raw/
