The solver implementations were used for benchmarking Common Lisp against C code.
They are intentionally not threaded to ensure that they only use one cpu core. The goal is not to measure multi-core performance or algorithmic/data structure optimizations against each implementation but to instead compare the performance of the code(in time and space usage) that the respective compilers generate using as similar as possible algorithms and data structures.

<img src='http://jng.imagine27.com/files/lisp_vs_c_time_2.png' />


See the following blog posts for more details :

http://www.imagine27.com/articles/2009-05-03-195227_i_want_to_believe_in_lisp_performance.html<br />
http://jng.imagine27.com/articles/2009-05-04-185802_iwtb_in_lisp_performance_followup.html<br />
http://jng.imagine27.com/articles/2009-05-06-113423_its_good_to_be_wrong.html<br />