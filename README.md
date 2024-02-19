
## Characterising information gains and losses when collecting multiple epidemic model outputs

Katharine Sherratt 1, Ajitesh Srivastava 2, Kylie Ainslie 3, David E. Singh 4, Aymar Cublier 4, Miguel Guzman Merino 4, Maria Cristina Marinescu 5, Jesus Carretero 4, Alberto Cascajo Garcia 4, Nicolas Franco 6, Lander Willem 7, Steven Abrams 8, Christel Faes 8, Philippe Beutels 8, Niel Hens 8, Sebastian Müller 9, Billy Charlton 9, Ricardo Ewert 9, Sydney Paltra 9, Christian Rakow 9, Jakob Rehmann 9, Tim Conrad 10, Christof Schütte 10, Kai Nagel 9, Sam Abbott 1, Rok Grah 11, Rene Niehus 11, Bastian Prasse 11, Frank Sandmann 11, Sebastian Funk 1

_1 London School of Hygiene and Tropical Medicine, London, UK; 2 University of Southern California, Los Angeles, USA; 3 RIVM, Bilthoven, Netherlands; 4 Universidad Carlos III de Madrid, Madrid, Spain; 5 Barcelona Supercomputing Center, Barcelona, Spain; 6 University of Namur (Belgium), Namur, Belgium; 7 University of Antwerp (Belgium), Antwerp, Belgium; 8 University of Hasselt (Belgium), Hasselt, Belgium; 9 TU Berlin, Berlin, Germany; 10 ZIB Berlin, Berlin, Germany; 11 ECDC, Stockholm, Sweden_

### Summary
We compared methods of collecting information from multiple infectious disease models. We found that key epidemic characteristics were less well represented by a quantile-summary method, while collecting modelled trajectories enabled continuous evaluation against newly observed data. The significance of information gain or loss varies with each collaboration’s aims.

- Find out more:
   - Read the [abstract](#abstract)
   - Read the [paper](output/submission-latest/REV1_Characterising-information-loss.pdf) (and [supplement](output/submission-latest/Supplement.pdf))
   - Explore [code](code) and [data](data), and reproduce results in the fully documented [results Rmarkdown](output/results.Rmd)

### Development

#### Publication

- [Available on medRxiv](https://www.medrxiv.org/content/10.1101/2023.07.05.23292245v2)
- We submitted to Epidemics, and following reviews have now submitted a revised manuscript

#### Reproducibility and contributions

- Results and supplement are generated from an [Rmarkdown document](https://github.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe/blob/analysis/analysis/output/results.rmd)
- Your comments, feedback, and questions are very welcome! Please [open an Issue](https://github.com/covid19-forecast-hub-europe/aggregation-info-loss/issues) or contact [Kath Sherratt](https://github.com/kathsherratt)

Note: full commit history for this project prior to 16 April 2023 is available as a [branch of the Scenario hub](https://github.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe/tree/analysis/analysis)

---

#### Abstract

*Background.* Collaborative comparisons and combinations of multiple epidemic models are used as policy-relevant evidence during epidemic outbreaks. In the process of collecting multiple projections of the future, such collaborations may gain or lose relevant information. Typically, each modeller contributes their own probabilistic summary using descriptive statistics at each modelled time step. We compare this method to directly collecting simulated trajectories from each of the models. We aimed to explore information on key epidemic quantities; ensemble uncertainty; and performance against data in order to investigate the potential to continuously gain information from only a single cross-sectional collection of model results.

*Methods.* We compared July 2022 projections from the European COVID-19 Scenario Modelling Hub. Using shared scenario assumptions, five modelling teams each contributed up to 400 simulated trajectories projecting incidence in each of Belgium, the Netherlands, and Spain. We compared epidemic characteristics including incidence, peaks, and cumulative totals. We also created a probabilistic ensemble drawn from all available trajectories at each time step, and compared this to two common ensemble methods of a median across each model’s quantiles, or a linear opinion pool. We then measured the predictive accuracy of each individual trajectory compared to later observations, and used this to create a weighted ensemble combining across all simulations. We repeated this sequentially against increasing weeks of observed data. We then evaluated the performance of these ensembles to reflect their variation in performance with varying amounts of observed data. 

*Results.* By collecting models’ simulated trajectories, we were able to show more policy-relevant epidemic characteristics, and evaluate performance against data, as opposed to collecting models’ quantiles at each time point. Sampled trajectories contained a right-skewed distribution which was poorly captured by an ensemble of models’ quantile intervals but well represented by a linear opinion pool. Ensembles weighted by predictive performance typically retained the range of plausible incidence over time, and in some cases narrowed this range by excluding some epidemic shapes altogether.

*Conclusions.* We observed several information gains from collecting modelled trajectories rather than summarised quantile distributions, highlighting the potential to create continuous new information from a single collection of model output. The value of different information gains and losses may vary with the aims of each collaborative effort, depending on the requirements and flexibility required by projection users. Understanding the differing information potential of methods to collect model projections can support the accuracy, sustainability, and communication of collaborative infectious disease modelling efforts. 
