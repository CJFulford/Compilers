This program has been written by Cody Fulford for CPSC 411 at the University of Calgary in the Winter 2017 Semester.

To run this code, simply go into the src folder, open a command prompt and type "main <filepath>" if on Windows or "./main <filepath>" if on linux
IF you run the code on windows, i have creaetd some batch files designed to speed the testing process




This program has implemented the tokens we were requested to implement, along with some custom tokens that i believe are appropriate.
The custome tokens are:
    **      representing a power function
    PRINT   representing a print function
    EQUALS  representing equals, this token is represented in the m- file by :=
    ERROR   representing some error handling
    
Since I suspect that i will need to implement the conversion in assignment 2, i have already implemented a string to int fucntion.
That is also how i am able to store my NUM token as an int and not a string