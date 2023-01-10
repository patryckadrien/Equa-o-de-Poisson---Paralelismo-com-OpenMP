reset
set key outside
set border 4095
set samples 25
set isosamples 20
set ticslevel 0
unset surface
set pm3d at st
set palette
set terminal pngcairo
set output "dif-240-16.png"
splot "saida.out" matrix t''
