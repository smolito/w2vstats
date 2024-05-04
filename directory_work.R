# script to check for directories

# root file control

if (!dir.exists("data-input")){
  dir.create("data-input")
}

if (!dir.exists("data-output")){
  dir.create("data-output")
}

if (!dir.exists("data-interim")){
  dir.create("data-interim")
}

if (!dir.exists("plots")){
  dir.create("plots")
}