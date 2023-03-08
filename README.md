# Book of Workflow

The following functional families are covered in 'book.of.workflow':

## Environment-processing

The focus is on loading libraries and managing environment objects:

-   do.load_unloaded()
-   do.save_image()
-   do.copy_obj()

## Environment Integrity

This family of functions is focused on maintaining the integrity of object environments, primarily, to mitigate errors due to missing object dependencies:

-   %missing%
-   %must.have%, %+must.have%
-   %check%
-   %+=%
-   %-=%

## External Data Management

This family of functions primarily focuses on connecting to external data (primarily databases):

-   do.make_query()
-   do.get_data()
-   do.export_data()
-   check.db_conn()

## Workflow Management

This family of functions focuses on code execution workflow:

-   read.snippet()
-   make.snippet()
-	  snippets_toc()
-   mgr_upgrade()
-   workflow_manager()
