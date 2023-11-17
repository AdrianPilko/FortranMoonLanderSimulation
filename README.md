# FortranMoonLanderSimulation
Simple text based moon lander physics simulation 
================================================

Enter a main engine thrust percentage and then a duration. The code will run a physics simulation at 0.1second intervals until the end of the engine burn. At which point provided you're altitude is greater than zero you will get another prompt to enter burn data.


At end of simulation an altitude vs time plot is done using gnuplot


To install gnuplot on Linux: sudo apt install gnuplot-x11


On Windows will be someother way Google it!

Currently only has a simple vertical (altitude) simulation. Future code may include down range also.
