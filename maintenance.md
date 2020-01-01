# 2018 profiles overview

## Order of operations

### Metadata

Profiles involve a lot of metadata in order to make headings and add definitions and URLs. Since that's the same for every location, I split it out into a separate script, [`make_profile_meta.R`](prep_scripts/make_profile_meta.R). That also fetches the URLs for the census profile tables (DP02, etc), since those needed to be updated to match the new census site. Location IDs come from a geography lookup table the census puts out.

### CWS data

Last 3 years' profiles had the same 2015 CWS data tacked on for certain locations. For 2018 profiles, I updated with 2018 CWS data, matching same indicators as last time. The data is copied over from the 2019 Index repo, rather than recalculating everything.

Since we need maximum MOE, I copied over all the crosstabs to read out that one line. Unfortunately, we only got that info in the header of the Excel sheets, so I had to unzip the xlsx files, read their underlying XML, and strip out the header elements. That's done in [`get_cws_maxmoe.R`](prep_scripts/get_cws_maxmoe.R).

### ACS

Regular degular ACS data is fetched in a couple scripts in the [`fetch_scripts`](fetch_scripts) folder. The analysis is done in [`build_town_profiles.R`](analysis_scripts/build_town_profiles.R) with few changes from 2017, then tacked together with CWS and metadata to make final profiles, so run this script last.

## TODO

* Some type of build tools/workflow would be nice, since script execution order matters, but I still haven't gotten the hang of drake or GNU make.