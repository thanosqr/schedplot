Compiling
=========
Use the Emakefile by calling make:all() from the erlang shell.

Using Schedplot
===============
For a detailed guide refer to Chapter 3 of my thesis (thesis.pdf)

Basic Interface
===============

    start(Fun) -> ok
    start(Fun,Flags) -> ok
    start(Fun,FolderName) -> ok
    start(Fun,FolderName,Flags) -> ok

Types:   
	   Fun = {Module, Function, Args} | function()  
	   FolderName = name()  
	   Flags = [Flag]  

schedplot:start/3 will profile Fun and store the trace in FolderName.  
If no FolderName is given, the default name will be used (schedplot_profile).  
If there is a directory with the same name, the traces will be stored in that directory (older traces will the default names will be overwritten).

Flags:  
no_auto_stop  
	the profiling will not stop when Fun returns but will continue untill schedplot:stop/0 is called. note that the profiling will stop if stop/0 is called even if this flag wasnt used.  

gc  
	profile and display the garbage collection  

trace_all  
	every procedure on the node will be traced  

start()->ok  
starts running the profiler (no function is called)  

stop()->ok  
stops the profiling and ensures that the traces have been delivered.  


analyze()->ok  
analyze(FolderName)->ok  
analyzes the traces stored in FolderName. if no name is given, the default name will be used.  

view()->ok  
view(FolderName)->ok  
displays the analyzed trace. if no name is given, the default name will be used.  


Labels
------
Anywhere in the traced program `schedplot:print(Text)` may be called. This will place a red vertical line, with a label displaying Text, at the graph of the shceduler in which the process was running in when schedplot:print/1 was called.  

Note that the text of labels that are too close may overlap and the vertical line will hide part of the graph.  

Warning: schedplot:print/1 is not supposed to be used a lot of times; it is supposed to be the equivalent of printing messages during debugging. A lot of calls may consume a significant amount of memory, depending on the size of Text as well as slow down the viewing process.  


Graph Keys
----------
Left/right arrow to move left/right (50px)  
Control left/right arrow for 200px, alt left/right arrow for 10px  
(Alt) up/down arrow to move 1 (10) core up/down  
Regular or numpad -/+ (as well as _/=) to zoom out/in  
Home to return to the first state (aprox. fit the screen)  



