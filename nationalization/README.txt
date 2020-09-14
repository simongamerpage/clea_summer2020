--------------------------------------------------------------------------
Implementing Nationalization Script/Computations to CLEA Presidential Data
--------------------------------------------------------------------------
Author: Simon Page
--------------------------------------------------------------------------

-----------
Description
-----------
The following script takes Fabricio Vasselai's Nationalization Application function
and applies it to a broad number of data sets that are in CLEA format. This script's
purpose is to simply streamline the process of taking data that is clean to CLEA 
format, then gaining nationalization measures at the party, national, and constituency
levels; moreover, gini values are computed.

------------
Instructions
------------

1. All files indended for reading MUST be in .xls or .xlsx format (while .xlsx is prefered for additional features); 
   - Helpful documentation on writing a file to .xlsx or reading .xlsx into R:
     - `openxlsx` Package: https://www.rdocumentation.org/packages/openxlsx/versions/4.1.5
     - `readxl` Package: https://www.rdocumentation.org/packages/readxl/versions/1.3.1

2. Open the "CLEA-nationalization-to-run.R" script, then fill in the respective pathways indicated and described. Example exists below and in the aforementioned file.
   - NOTE: The sub-directory path can simply be a folder within the main directory that has all of the files which need nationalization computations.
     As is in my example, all of the files in question exist in the "clean_to_run" subdirectory, but if the files are spread with different folders by country,
     you must only have ONE file to read, and MUST specify using the vector operator in R (i.e., `pathSubDirectory <- c("first_sub","second_sub","third_sub")`);

3. The application for nationalization function should not need to be changed, so long as all of the paths and directories are defined above, so once the
   paths are defined, run the entire script. Finally, the entire output of the nationalization script will exist in four files, where all countries included
   will exist in the same files; 
 
4. A few crucial notes are listed below when using the script:
   - If data only exists in the second round (for whatever reason), the user must proxy those values in as first round values (for example, shifting pev2,vot2,etc.
   to pev1, vot1, etc.) in order to acquire an output;
   - As of 8/18/2020, if a data set contains only party/candidate vote SHARES (in cases of poor data quality), the data will run successfully but will not provide
   any nationalization output. The way nationalization measures are computed in the script does not work if crucial totals are missing;
   - Even if a column is not directly applicable (i.e., `seat` is not applicable to Presidential data like it would be for lower chamber elections), it must be
   included to compute the nationalization measures; if it is not applicable, filling with -990s is appropriate;
   - The example below assumes a default working directory. Including the full directory (using combinations of `getwd()` and/or `setwd()`) is acceptable;
   - CRUCIAL: Double clicking the script to open in RStudio will auto-assign that file's location as your working directory! Open the file when in RStudio. 

--------
Example
--------

# Location of Simon's Script, named "CLEA-nationalization-source", on Local Machine
pathScript <- "clea/nationalization_clea/"

# Location of Main Directory, where All Sub-directories Exist
pathMainDirectory <- "clea/nationalization_clea/"

# Location of Sub-directory (or directories), where all Clean CLEA Files Exist 
pathSubDirectory <- "clean_to_run/"

# Output Path, or where all Final Nationalization Measure Files End Up
pathOutput <- "clea/nationalization_clea/nationalization_output/"

# Sourcing Simon's Script, which Includes Fabricio's Nationalization Function
source(file.path(pathScript,"CLEA-nationalization-source.R"))
