Example Haskell Server application
================================== 


This directory contains an example application written in Haskell.  The example has been created by modifying a simple skeleton Snap application.  You are free to base your entry on this application, although you may also prefer to use another framework or language.  

Documentation on the Snap Framework can be found at the URL http://snapframework.com/ 

The sample application is intended to be reasonably minimal.  We have opted to use SQLite, rather than depend on separate database daemon, which could complicate the process of setting everything up.

The application implements a simple interface at the path `/api/position` with no user authentication.  Any client can POST a JSON object representing a position, and likewise fetch a list of positions using GET.  

If you choose to use Snap for your project, you might like to refer to discussion of the Snap Framework at other sites, for example: 

* Luc Perkins at Janrain has written an article which sets out the process of [`building an application`] (http://janrain.com/blog/tutorial-building-a-sample-application-with-haskell-snap-postgresql-and-the-postgresql-simple-snaplet/) 
* Gregory Collins wrote the [`CUFP 2011 Tutorial`] (https://github.com/snapframework/cufp2011). 

Good luck! 






