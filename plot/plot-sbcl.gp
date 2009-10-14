
# horizontal (time) axis layout
set xlabel "time (seconds)"
set xtics 1.000

# left vertical axis layout
set ylabel "" 2,0
#set yrange [0:40000]
#set ytics 2000
#set mytics 100

# overall layout
set title "Boggle solver (20 x 20 board, Common Lisp, 270k English dict)" 0,-.5
set grid

# Make labels for the totals
words = `perl -e '$s=0;while(<>){$s=0;($m,$a,$b)=split;$s+=$a}; print $s;' time.txt`
score = `perl -e '$s=0;while(<>){$s=0;($m,$a,$b)=split;$s+=$b}; print $s;' time.txt`

# choose output and plot it
set terminal png size 800 600 #fname "Helvetica" fsize 14
set output "plot.png" 
plot "time.txt" using 1:2 title sprintf("# Words", words)\
  with linespoints pointtype 2 linetype 3 linewidth 1,\
  "time.txt" using 1:3 title sprintf("Score", score)\
  with linespoints pointtype 3 linetype 1 linewidth 1
