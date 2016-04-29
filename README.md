# Hagen Ranking
=================
Percentile ranking based on book "Introductory Statistics: Concepts, Models, and Applications by David W. Stockburger".

Ranking formula for Hagen could be found in [this link](http://www.psychstat.missouristate.edu/introbook/sbk14.htm)

PR=Probability Ranking

Fb=Frequency below -> is the frequency below; the number of scores which are less than the score value of the percentile rank

Fw=Frequency within -> is the frequency within; the number of scores which have the same value as the score value of the percentile rank

N=Number of scores

PR = ((Fb + (1/2 * Fw)) / N) * 100

---
Requirements
----
- IBM SPSS Modeler v16 or later
- ‘R Essentials for SPSS Modeler’ plugin: [Download here][2]
-  R 2.15.x, 3.1, or 3.2 (depending on version - [use this link][3] to find the correct version)


---
Installation instructions
----
1. Download the extension: [Download](https://github.com/IBMPredictiveAnalytics/Hagen_Percentile_Ranking/blob/master/src/hagenranking.cfe) 
2. Close IBM SPSS Modeler. Save the .cfe file in the CDB directory, located by default on Windows in "C:\ProgramData\IBM\SPSS\Modeler\16\CDB" or under your IBM SPSS Modeler installation directory.
3. Restart IBM SPSS Modeler, the node will now appear in the Field Ops palette.

---
R Packages Used
---

- [plyr][1]

---
Screenshots
----
1. Node high-level overview
![Image main view](https://raw.githubusercontent.com/blacknred0/hagen-ranking/master/screenshot/main-view.png)

2. Raw data table
![Raw data](https://raw.githubusercontent.com/blacknred0/hagen-ranking/master/screenshot/raw-data.png)

3. Within node
![Node options](https://raw.githubusercontent.com/blacknred0/hagen-ranking/master/screenshot/within-node.png)

4. Results after processing
![Results after ranking](https://raw.githubusercontent.com/blacknred0/hagen-ranking/master/screenshot/results-after-ranking.png)

---
Issues
----
None at the time.

---
How to contribute
----
See [TODO](TODO) for list of enhancements that could be made.

---
License
----
This sample code is licensed under Apache 2.0. Full license text is available in [LICENSE](LICENSE).

---
Open Source @ IBM
----
Find more open source projects on the [IBM Github Page](http://ibm.github.io/)

[1]: https://cran.r-project.org/web/packages/plyr/
[2]: https://github.com/IBMPredictiveAnalytics/R_Essentials_Modeler/releases
[3]:https://developer.ibm.com/predictiveanalytics/downloads/