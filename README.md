
# glisDataTools

The glisDataTools packages is intended to provide some data
manipulation tools for use with GLIS, the glfishr package, and the
upload templates.  Specifically, it provides mappers to the current
GLIS data upload templates for projects in Creesys 4.1 files, and the
Lake Huron Nearshore Master database.  It also includes tools to
compare the contents of tables in two different databases and reports
any differences if any are found.  For the most part, these tools are
intended to facilate the migration of data from historical data
repositories to GLIS.

NOTE: The functions in glisDataTools currently work with glis upload
templates "Great_Lakes_Sport_Creel_Template_2.accdb" and
"Great_Lakes_Assessment_Template_5.accdb".

## Installation

You can install the development version of glisDataTools like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
# do to....
```

## Creesys 4.1 to GLIS Template

The glisDataTools package includes a function that will connect to any
Creesy 4.1 access database and populate a GLIS assessment template
for each of the provided project code(s).


``` r
library(glisDataTools)
## basic example code

# the path to the copy of the creesys master on your computer. The
# name of the file isn't important, but the access file does have to
# match the schema of a Creesys 4.1 database:

src_db <-  "/CREESYS 4.1.accdb"

# the path to an empty copy of GLIS creel upload template:
template_db <- "~/Great_Lakes_Sport_Creel_Template_2.accdb"


# you can pass in a single project code or a vector of project codes:
# prj_cds <-"LHA_SC10_120" # <- this would work too
prj_cds <- c("LHA_SC10_120", "LHA_SC09_120")

# the creesys_to_template function will report its progress and where
# the populated template can found once it is completed.  By default
# it will be located in a ./build folder in the working directory and
# be named with the project codes that were provided, but this can be
# customized:
creesys_to_template(prj_cds, src_db, template_db, overwrite=TRUE)

```


## Nearshore Master to GLIS Template

The glisDataTools package includes a function that will connect to the
Lake Huron Nearshore master (or more likely a copy of it) and populate
a GLIS assessment template for each of the provided project codes.

``` r
library(glisDataTools)
## basic example code

# the path to the copy of the nearshore master on your computer:
src_db <-  "~/IA_NEARSHORE.accdb"
# the path to an empty copy of GLIS assessment template:
template_db <- "~/Great_Lakes_Assessment_Template_5.accdb"

# you can pass in a single project code or a vector of project codes:
# prj_cds <- "LHA_IS10_140"
prj_cds <- c("LHA_IS10_140", "LHA_IS09_140")

# the nearshore_to_template function will report its progress and where
# the populated template can found once it is completed.  By default
# it will be located in a ./build folder in the working directory and
# be named with the project codes that were passed in but this can be
# customized:
nearshore_to_template(prj_cds, src_db, template_db, overwrite=TRUE)


```


## Compare Database Tables

One of the most basic challenges that we will face as we migrate from
historical dataset to GLIS is comparing what exactly is in the
historical dataset with what is in GLIS.  the glisDataTools package
contains some utlities that help with this task.  The primary function
for is compare_tables, which will accept the path to accdb file
populated firectly form GLIS, a path to a second accdb file created
from the data in the historical source and the name of the table to
compare. If the tables are exactly the same in both sources, the
function will report "no differences".  If there are differences, a
report will pre presented with information about each discrepancy.
The user can then decide if changes need to be made in GLIS, or if the
data in GLIS has already been updated and cleaned.

``` r

library(glisDataTools)

# the complete path a template populated from glis using
# glfishr::populate_template()
glis_db <- "~/LHA_IA22_821_glis.accdb"

# complete path to a template with the data from the same project
# created from the nearshore master (using
# glisDataTools::nearshore_to_template()):
nearshore_db <- "~/LHA_IA22_821_nearshore.accdb"

# we can get a list of tables in each template:
glis_tables <- get_tablenames(glis)
nearshore_tables <- get_tablenames(nearshore)

# By default the list of tables in each database should be exactly the
# same, but will contain extra elements if you have added new tables
# to one or both of the templates.
compare(glis_tables, nearshore_tables)


# the compare_tables() function will compare the structure and
# contents of the provided table in both of the data bases and provide
# a summary of the differences (if any).

compare_tables(glis_db, nearshore_db, "FN011")

compare_tables(glis_db, nearshore_db, "FN121")

compare_tables(glis_db, nearshore_db, "FN127")



```



## Future Functionality:

+ offshore_to_template - populate a template database from the Lake
  Huron Offshore Master for the specified project code(s).  Other data
  sources could be added in the future as needed (eg. - fishnet
  archive, Lake Superior FCIN master ect.)

+ split template - given a template that contains multiple projects,
  split it into separate template databases that can be run through
  process validate and uploaded individuall if desired.  It should
  take an optional prj_cds argument.  If prj_cds is null, all of
  projects in the database will be put into project specific template
  databases, if it is provided, only the specified projects will be
  split out.

+ merge templates - given a list of two or more populated templates,
  merge them into a single template data base that can be run through
  process validate or uploaded to glis.  There are instances where
  scrubbing several related projects is more effecient when they are
  in the same template.

+ prune_prj_cds - delete all of the records from the target database
  for the specified project code(s)

+ recode prj_cd - occasionally, a project code needs to change. The
  referencial integrity built into the template database can make this
  challenging.  This function will automate this task without risk of
  corrupting the database or losing any data.

+ clone_design_tables - often users want to be able to repeat a past
  project and need to get the FN0* tbales in a new template database
  with a new project code.  This could be accomplished by creating a
  function that takes an old project code, and a new proejct code and
  then populates the data from GLIS, or it could take a populated
  template database (with a single project code) and prune all of the
  records from the FN121 down, and recode the project code for the
  remaining table. There would probably be a valid use case for both
  approaches (the first could be emulated by downloading all of the
  data for the source project using populate_template() and then
  pruning that.)


+ migrate() - update a template from version X to Y
