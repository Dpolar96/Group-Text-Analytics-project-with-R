# Group-Text-Analytics-project-with-R
<div id="top"></div>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Issues][issues-shield]][issues-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/Dpolar96/Group-Text-Analytics-project-with-R">
  </a>

<h3 align="center">Airbnb analysis using NLP and visualization using Tableau</h3>

  <p align="center">
    Analysis of an Airbnb database from Mongo using R for NLP and Tableau to project these insights
    <br />
    <a href="https://github.com/Dpolar96/Group-Text-Analytics-project-with-R"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    ·
    <a href="https://github.com/Dpolar96/Group-Text-Analytics-project-with-R/issues">Report Bug</a>
    ·
    <a href="https://github.com/Dpolar96/Group-Text-Analytics-project-with-R/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

This is a group project, in which we first connected to a cluster of Mongo DB to access a sample of Airbnb data including the listing description, location, property type, review scores, amount of listings, # of bedrooms and bathrooms, and Superhost status. Initially R was used to connect to Mongo DB, running a script for NLP analysis and other precise analysis that couldn't be done on Tableau such as average price overall. At the same time, the initial csv extracted from the database was used to carry a visualization analysis on Tableau to visualize insights not considered on R. Finally compiling both analysis in a report included [here](https://github.com/Dpolar96/Group-Text-Analytics-project-with-R/blob/Group-Project/Report/Combined%20assessment%20report%20(1).pdf).
<p align="right">(<a href="#top">back to top</a>)</p>



### Built With

* [RStudio IDE](https://www.rstudio.com/products/rstudio/download/)
* [Tableau](https://www.tableau.com/trial/download-tableau)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these simple example steps.
1. Line 16 and 17 must be updated to reflect a different topic of interest and connection to a different Mongo Database cluster (optional step)
2. Line 52 and 83 must be updated to reflect local directory.
3. Usage of Tableau is specific to the case scenario and reflects the information obtained from the MongoDB sample based on Airbnb.

### Prerequisites

In order to run the code code from this project the following libraries are needed.
* jsonlite
* mongolite
* tidytext
* tidyverse
* tidyr
* tidytuesdayR
* stringr
* textreadr
* pdftools
* textshape
* twitteR
* tm
* ggplot2
* scales
* magrittr
* dplyr
* gutenbergr
* Matrix
* textdata
* igraph
* ggraph
* widyr
* topicmodels
* gutenbergr
* quanteda
* quanteda.textmodels
* RColorBrewer
* tibble
* stringr

### Installation

In case these packages are not installed, please run the following lines of code.
  ```sh
  install.packages("jsonlite")
  install.packages("mongolite")
  install.packages("tidytext"
  install.packages("tidyverse")
  install.packages("tidyr")
  install.packages("tidytuesdayR")
  install.packages("stringr")
  install.packages("textreadr")
  install.packages("pdftools")
  install.packages("textshape")
  install.packages("twitteR")
  install.packages("tm")
  install.packages("ggplot2")
  install.packages("scales")
  install.packages("magrittr")
  install.packages("dplyr")
  install.packages("gutenbergr")
  install.packages("Matrix")
  install.packages("textdata")
  install.packages("igraph")
  install.packages("ggraph")
  install.packages("widyr")
  install.packages("topicmodels")
  install.packages("gutenbergr")
  install.packages("quanteda")
  install.packages("quanteda.textmodels")
  install.packages("RColorBrewer")
  install.packages("tibble")
  install.packages("stringr")
  ```

<!-- CONTACT -->
## Contact

Diego Polar - dpolar76@gmail.com

Project Link: [https://github.com/Dpolar96/Group-Text-Analytics-project-with-R](https://github.com/Dpolar96/Group-Text-Analytics-project-with-R)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments
I thank and appreciate the help of my group members during the ellaboration of this project:
* [Diana Aycachi](https://www.linkedin.com/in/diaycm/)
* [Allesandro Casella](https://www.linkedin.com/in/alessandro-casella-97953b197/)
* [Kevin Farjallah](https://www.linkedin.com/in/kevin-farjallah/)
* [Vivian Soo](https://www.linkedin.com/in/vivian-soo-8446641b7/)
* [Madhuri Thackeray](https://www.linkedin.com/in/madhuri-thackeray-05168b14a/)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[issues-shield]: https://img.shields.io/github/issues/Dpolar96/Group-Text-Analytics-project-with-R.svg?style=for-the-badge
[issues-url]: https://github.com/Dpolar96/Group-Text-Analytics-project-with-R/issues
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/diego-polar-velasquez-3bbbb9154/
[product-screenshot]: images/screenshot.png
