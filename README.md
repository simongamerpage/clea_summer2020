# clea_summer2020
The following repository consists of work completed by Simon Page during Summer 2020 Research Assistant work on the Constituency Level Election Achive (CLEA). More specifically, these files are clean to CLEA-format, world-wide presidential election data, as well as an application of said data for nationalization measures (in R). This work will be used for future research on nationalization/effective number of parties around the globe. 


Below is a small, generalized list of the tasks and accomplishments I have contributed to the CLEA Presidential
Data project. This list serves as a pseudo-memo, where more information on specific elections/years cleaned to
CLEA, as well as nationalization measures completed, are included in the spreadsheet with file name
“ clea_presdata_exhaustive_sami_simon.xlsx ”; moreover, all applicable source files, clean-to-CLEA files,
nationalization scripts and outputs, etc… are all included in the file named “ clea_presdata_summer2020 ”.

● Election countries and year ranges (table in aforementioned exhaustive list) have been cleaned-to-CLEA
utilizing R ;

● The vast majority of data cleaned this summer was provided to me by a colleague of Profs Kollman &
Hicken, yet some supplementary data was found (i.e., korean presidential data, uruguayan vote totals);
sources for said data are included in the aforementioned exhaustive sheet;

● Note: data for Round 1 of the Colombian 2010 Presidential election was found and downloaded in scanned
pdf format; this data is available, both in source in the exhaustive list and individual folder, downloaded
(“ clea_presdata_summer2020/unclean_data/colombia2010r1_pdfs ”); this data requires a skilled scraper
or individual with scanned pdf knowledge as it exists in an unfortunate format;

● Also included in that list are data cleaned by Samantha Ilagan (Sami), which was utilized for
nationalization measures computed by me;

● Nationalization script from Fabricio Vasselai was utilized and incorporated into a larger script, which takes
clean-to-CLEA data and pipes it into the nationalization function; moreover, this script created by me
aggregates the nationalization output for easier future implementation;
○ This script/source code is in the “ clea_presdata_summer2020/nationalization ” folder, where a
README.txt exists with instructions and important documentation;

● Finally, a decent amount of time was spent attempting to debug CLEA Release code, but was ultimately
solved by Julia Lippman, PhD;
