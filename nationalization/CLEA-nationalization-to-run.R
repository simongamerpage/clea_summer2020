##-------------------------------------------------------##
##          Applying Nationalization Script              ##
##-------------------------------------------------------##
##                    Simon Page                         ##
##-------------------------------------------------------##
##                                                       ##
## This file is meant for aggregating CLEA data, then    ##
## applying Fabricio Vasselai's application of           ##
## nationalization measures function.                    ##
##                                                       ##
##-------------------------------------------------------##
## FOR INSTRUCTIONS/EXAMPLE, see README.txt              ##
##-------------------------------------------------------##

##-----------------------------------##
## File Pathways and Script Sourcing ##
##-----------------------------------##

# Location of Simon's Script, named "CLEA-nationalization-source", on Local Machine
pathScript <- "clea/clea_presdata_summer2020/nationalization/"

# Location of Main Directory, where All Sub-directories Exist
pathMainDirectory <- "clea/clea_presdata_summer2020/nationalization/"

# Location of Sub-directory (or directories), where all Clean CLEA Files Exist 
pathSubDirectory <- "clean_to_run/"

# Output Path, or where all Final Nationalization Measure Files End Up
pathOutput <- "clea/clea_presdata_summer2020/nationalization/nationalization_output/"

# Sourcing Simon's Script, which Includes Fabricio's Nationalization Function
source(file.path(pathScript,"CLEA-nationalization-source.R"))

##-----------------------##
## Applying the Function ##
##-----------------------##
applyNationalization(mainDirectory = pathMainDirectory,
                     subDirectories = pathSubDirectory,
                     outputPath = pathOutput)

