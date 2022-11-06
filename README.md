# ![](img/book-305126_1280.png){#book style="text-align:middle;" width="39"} Book of Workflow

The following functional families are covered in 'book.of.workflow':

## Environment-processing

The focus is on loading libraries and managing environment objects:

-   do.load_unloaded()
-   do.save_image()
-   do.copy_obj()

## Environment Integrity

This family of functions is focused on maintaining the integrity of object environments, primarily, to mitigate errors due to missing object dependencies:

-   %missing%
-   must.have(), %must.have%, %+must.have%
-   env.check(), %check%
-   %+=%
-   %-=%

## External Data Management

This family of functions focuses on connecting to external data (primarily databases) as well as parallelized computing contexts:

-   get.cluster_meta()
-   do.make_workers()
-   do.make_query()
-   do.get_data()
-   do.export_data()
-   check.db_conn()

## Workflow Management

This family of functions focuses on code execution workflow:

-   read.snippet()
-   make.snippet()
-   workflow_manager()
-   mgr_upgrade()
