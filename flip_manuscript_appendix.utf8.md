---
appendix: flip_manuscript_appendix.Rmd
output: pdf_document
---






## S1: Experiments included in the linear mixed-effects model {#s1}

The selected experiments consist of an artificial grammar learning task with 12-month-old infants. These experiments are characterized by variability in the number of infants’ prior HPP visits^[At the time of publication of @saffran2003, the first author noted that there appeared to be an association between the number of prior studies completed by the infants and the direction of preference. The analysis was included in the original manuscript submission but was removed from later revisions based on reviewer suggestions.]. They include all studies run in the two senior authors’ labs that included (a) 11- to 13-month-old participants; (b) HPP; (c) artificial grammar learning (linguistic or non-linguistic); (d) 2 to 5 minutes of exposure; (e) an *a priori* hypothesis that infants would show learning; (f) visit numbers recorded at the time of testing. The studies are thus as well matched as is possible given the retrospective nature of this analysis. 


*@saffran2003* demonstrated that 12-month-old infants can compute multiple regularities from a finite-state grammar. Infants were able to first segment words from running speech based on transitional probabilities, then detect permissible orderings of the segmented words. Test items consisted of grammatical and ungrammatical sentences that could only be discriminated based on word-level information (transitional probabilities between syllables were not informative about the "grammaticality" of test items). Infants showed a significant familiarity preference: *F*(1, 38)= 5.37, *p* < .05.

*Saffran, Hauser, Seibel, Kapfhamer, Tsao, & Cushman (2008)* demonstrated that infants could detect simple phrases (i.e., clusters of nonsense words grouped together based on statistical regularities) from artificial grammars. In Exp. 1, infants in the Predictive Language condition were familiarized with a grammar including predictive (statistical) dependencies between words. The test items consisted of familiar sentences vs. novel sentences violating the grammar. Infants showed a significant novelty preference: *t*(11) = 2.52, *p* < .05.


*@santolin2019* is a conceptual replication of @saffran2008 using non-linguistic sounds (e.g., computer alert sounds) to implement the grammars. Infants exposed to the Predictive language showed a significant novelty preference: *t*(26) = 2.45, *p* = .021, *d* = 0.47.
	

We replicated the Predictive Language condition of @santolin2019 at the University Pompeu Fabra, Barcelona (*Santolin, Saffran & Sebastian-Galles, 2019*)[-@santolin2019a], using identical stimuli and procedures. We found significant discrimination of the test stimuli but observed the opposite direction of preference: infants listened longer to familiar than novel strings: *t*(23) = 2.30, *p* = .030, *d* = 0.47. All results are shown in Figure 1 of the main manuscript. 


## S2: Participants information {#s2}

We retrieved data from 102 infants who had participated in a range of 1-6 HPP visits. Three of the experiments were run in Madison, WI (University of Wisconsin-Madison): Saffran & Wilson, 2003 (Exp. 2; *N*=40, mean age: 11.5 months); Saffran et al., 2008 (Exp. 1, Condition P-Language: *N*=12, mean age: 12.8 months); Santolin & Saffran, 2019 (Condition 1; *N*=26, mean age: 12.9 months). One study was run in Barcelona, Spain (Universitat Pompeu Fabra): Santolin, Saffran & Sebastian-Galles, 2019 (*N*=24, mean age: 13 months).


## S3: Linear mixed-effects model - additional information {#s3}

We fit a model predicting looking time ($LT$) including $Item$ (Familiar vs. Novel), number of Head-turn Preference Procedure experiments completed by infants ($HPP$), and their interaction ($Item \times HPP$) as fixed effects. Participant and study [4 levels: Santolin & Saffran (2019), Santolin, Saffran & Sebastian-Galles (2019), Saffran et al. (2008), Saffran & Wilson (2003)] were included as random effects. Following @barr2013, we fit a model with the maximal random effects structure including random intercepts by-participant and by-study, and random slopes of $HPP$ by-participant and by-study. However, due to lack of convergence, we pruned the random effects structure until convergence was achieved [e.g., @brauer2018]. The final model included by-participant and by-study random intercepts only. This model accounts for cross-participant variability in overall looking time (as some infants look longer than others), and for cross-study differences in overall looking time. The model was fit using the `lme4` R package [@bates2015a]. We used the `Anova` function from the `car` R package [@fox2019] to perform *F*-tests on fixed effects using Kenward-Roger’s approximation of the degrees of freedom [e.g., @judd2012]. 


## S4: Results sub-setting data to participants with less than 6, 5, 4, and 3 HPP studies {#s4}

Consistent with the results of the entire dataset, we found a statistically significant interaction of *Test Item* with the number of *HPP* visits when reducing the sample to the infants who participated in less than 6, 5, 4, and 3 HPP experiments. Below, a table reporting the output of the linear mixed-effects model fitted on the original and reduced samples (Table S4.1 \@ref(tab:tabsup)). 



 **Subset**            **Term**            **Coefficient**    ***SEM***       **95\% CI**       **F**    **Den. *df***    ***p*** 
------------  --------------------------  -----------------  -----------  -------------------  -------  ---------------  ---------
  Original           *Intercept*               7,679.0          673.3      [6389.7, 9294.6]     124.6         9.1         < .001  
                     *Test Item*              -1,398.8          411.3      [-2204.9, -589.1]    11.6         100.0         .001   
                        *HPP*                  -539.7           238.7       [-999.9, -74.7]      4.8         133.1         .030   
               *Test Item $\times$ HPP*         667.1           192.6       [247.2, 1028.5]     12.0         100.0         .001   
  HPP 1-5            *Intercept*               7,611.1          719.3      [6188.6, 9275.3]     107.4        10.5         < .001  
                     *Test Item*              -1,491.1          452.1      [-2348.4, -578.5]    10.9         98.0          .001   
                        *HPP*                  -500.8           278.7       [-1070.2, 98.2]      3.0         131.9         .083   
               *Test Item $\times$ HPP*         726.2           224.9        [294.2, 1145]      10.4         98.0          .002   
  HPP 1-4            *Intercept*               7,611.1          719.3      [6188.6, 9275.3]     107.4        10.5         < .001  
                     *Test Item*              -1,491.1          452.1      [-2348.4, -578.5]    10.9         98.0          .001   
                        *HPP*                  -500.8           278.7       [-1070.2, 98.2]      3.0         131.9         .083   
               *Test Item $\times$ HPP*         726.2           224.9        [294.2, 1145]      10.4         98.0          .002   
   HPP1-3            *Intercept*               7,470.0          794.6      [6007.1, 9172.6]     83.8         14.3         < .001  
                     *Test Item*              -1,349.9          532.3      [-2426.9, -267.6]     6.4         92.0          .013   
                        *HPP*                  -395.8           366.6      [-1182.6, 316.1]      1.1         122.3         .299   
               *Test Item $\times$ HPP*         627.9           294.1         [3, 1252.6]        4.6         92.0          .035   
  HPP 1-2            *Intercept*               7,301.9         1009.9      [5253.3, 9362.1]     48.9         23.9         < .001  
                     *Test Item*              -1,783.7          726.7      [-3199.6, -360.7]     6.0         78.0          .016   
                        *HPP*                  -261.3           586.8      [-1449.8, 991.7]      0.2         107.0         .667   
               *Test Item $\times$ HPP*         969.5           481.8       [38.3, 2010.4]       4.0         78.0          .048   


\begin{figure}
\includegraphics[width=\textwidth]{/Users/GonzaloGGC/projects/Flip/Figures/05_anova-merged} \caption{Graphical representation of the coefficients of the Test Item by HPP visits interaction term of the model fitted on the complete data-set (reported in the main manuscript), and of the models fitted on the reduced data-sets. Black dots represent the point estimate of the coefficient, black whiskers represent the standard error of the mean, and grey boxes represent the bootstrapped 95\% confidence interval around the point estimate.}\label{fig:unnamed-chunk-4}
\end{figure}

# Session info {-}

R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.4

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] here_0.1          purrr_0.3.4       kableExtra_1.1.0  knitr_1.28       
[5] ggplot2_3.3.1     tibble_3.0.1      dplyr_1.0.0       papaja_0.1.0.9942

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.4.6      highr_0.8         pillar_1.4.4      compiler_3.6.3   
 [5] tools_3.6.3       digest_0.6.25     viridisLite_0.3.0 evaluate_0.14    
 [9] lifecycle_0.2.0   gtable_0.3.0      pkgconfig_2.0.3   rlang_0.4.6      
[13] rstudioapi_0.11   yaml_2.2.1        xfun_0.14         withr_2.2.0      
[17] stringr_1.4.0     httr_1.4.1        xml2_1.3.1        generics_0.0.2   
[21] vctrs_0.3.0       hms_0.5.3         rprojroot_1.3-2   webshot_0.5.2    
[25] grid_3.6.3        tidyselect_1.1.0  glue_1.4.1        R6_2.4.1         
[29] rmarkdown_2.1     readr_1.3.1       magrittr_1.5      backports_1.1.7  
[33] scales_1.1.1      ellipsis_0.3.1    htmltools_0.4.0   rvest_0.3.5      
[37] colorspace_1.4-1  stringi_1.4.6     munsell_0.5.0     crayon_1.3.4     

## References {-}

\begingroup
\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}

<div id="refs" custom-style="Bibliography"></div>

\endgroup

