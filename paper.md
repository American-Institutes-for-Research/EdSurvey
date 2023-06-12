---
title: 'EdSurvey: an R package to analyze Large-scale educational assessments data'
tags:
  - R
  - Large-scale assessment
  - survey
  - education
  - NCES
authors:
  - name: Paul Bailey
    orcid: 0000-0003-0989-8729
    corresponding: true # (This is how to denote the corresponding author)
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Charles Blankenship
    orcid: 0009-0004-5279-9376
    affiliation: 1
  - name: Eric Buehler
    orcid: 0009-0004-6354-2015
    affiliation: 2
  - name: Ahmad Emad
    affiliation: 3
  - name: Tom Fink
    orcid: 0009-0003-9308-2833
    affiliation: 1
  - name: Huade Huo
    orcid: 0009-0004-5014-646X
    affiliation: 1
  - name: Claire Kelley
    orcid: 0000-0000-0000-0000
    affiliation: 4
  - name: Michael Lee
    affiliation: 5
    orcid: 0009-0006-0959-787X
  - name: Sun-joo Lee
    affiliation: 6
  - name: Yuqi Liao
    orcid: 0000-0001-9359-6015
    affiliation: 1
  - name: Alex Lishinski
    orcid: 0000-0003-4506-1600
    affiliation: 7
  - name: Trang Nguyen
    orcid: 0009-0001-0167-8775
    affiliation: 8
  - name: Emmanuel Sikali
    affiliation: 9
  - name: Blue Webb
    orcid: 0009-0004-4080-9864
    affiliation: 1
  - name: Qingshu Xie
    affiliation: 1
  - name: Sinan Yavuz
    orcid: 0000-0003-3131-7820
    affiliation: 1
  - name: Jiao Yu
    affiliation: 10
  - name: Ting Zhang
    orcid: 0009-0001-1724-6141
    affiliation: 1

affiliations:
 - name: American Institutes for Research
   index: 1
 - name: Memorial Sloan Kettering Cancer Center
   index: 2
 - name: Google LLC
   index: 3
 - name: Child Trends
   index: 4
 - name: Independent researcher
   index: 5
 - name: United Nations Development Programme
   index: 6
 - name: University of Tennessee Knoxville
   index: 7
 - name: University of Massachusetts Amherst
   index: 8
 - name: National Center for Education Statistics, USA
   index: 9
 - name: JPMorgan Chase & Co.
   index: 10


date: 1 May 2023
bibliography: paper.bib

---

# Summary

Data from large-scale educational assessment programs, such as the National Assessment of Educational Progress (NAEP) and Progress in International Reading Literacy Study (PIRLS), require special methods to conduct statistical analyses because of their scope and complexity. The `EdSurvey` package gives users functions to perform analyses that account for both the complex sample survey design and the use of plausible values. The `EdSurvey` package also seamlessly takes advantage of the `LaF` [@LaF] package to read in data only when required for an analysis. Users with computers that have insufficient memory to read in entire datasets can still do analyses without having to write special code to read in just the appropriate variables. This is all addressed directly in the `EdSurvey` package — behind the scenes and without any special tuning by the user.

# Statement of need

There is a significant demand for accessible and user-friendly software to process and analyze large-scale educational assessment data. These assessments provide valuable insights for policy-makers, education practitioners, and researchers to understand and explore various education-related issues throughout childhood to adulthood, in schools, homes, and neighborhoods.

To access these data with ease, perform data analysis, and test their hypotheses, various free tools have been supplied by assessment institutes, or government agencies, including the International Database IDB Analyzer, NCES Electronic Code Books, NAEP Data Explorer, International Data Explorer, WesVar, and the AM software. 
Most of these free tools are menu-driven software that do not create a shareable code. They were created with the average statistical user in mind and are limited in the types of the statistical analyses that they can support. There are also commercially available software packages (e.g., SPSS, SAS, or STATA) for data merging, cleaning, and manipulation but many analysts use them for data manipulation only and then choose to import the data into tools such as WesVar or AM to account for complex sample designs and use the plausible values.

The `EdSurvey` package was conceived and developed to group in one place all the methodologies and techniques that a researcher needs to access, process, manipulate, and analyze these educational databases. `EdSurvey` can handle most complex samples, plausible value estimations, and longitudinal data methodologies. `EdSurvey` runs in R, a programming language licensed under the GNU General Public License, and is widely used by academia and the research communities. The package is tailored to the processing and analysis of large-scale assessment data with appropriate procedures. It is built as a one-stop shop for downloading, processing, manipulation and analysis of survey data. Other packages in R will analyze large-scale assessment data, including survey [@Lumley], lavaan.survey [@Oberski], svyPVpack [@svyPVpack], BIFIE [@BIFIE], intsvy [@intsvy] and RALSA [@RALSA]. Among these packages, some have limited data coverage (e.g., tailored to international large-scale assessments only), and some require user input about the survey design and plausible values before they can be used. `EdSurvey` was developed to analyze all the large-scale assessments that the United States participates in under NCES, and it incorporates a complex sampling design and plausible value methodology seamlessly. The advantages of `EdSurvey` include:

- allowing for data manipulation inside and outside the package;
- minimizing memory footprint by only reading in required data;
- enabling users to search the names and labels of variables, view frequencies and percentages of response categories of variables, and visualize the data;
- performing complex sample analysis operations;
- computing analyses with plausible values;
- performing multilevel analyses and modeling; 
- expanding functions and data supports to meet the needs of analysts with varying levels of expertise; and
- incorporating sampling weights and performing recommended standard error estimations via replicate weighting or the Taylor Series method.

# Workflow
Recognizing that researchers using R statistical software come with varying levels of experience, the `EdSurvey` package has provided multiple workflows to aid in this process of conducting survey analysis. The following graphic details the recommended workflows with corresponding `EdSurvey` functions.

![EdSurvey workflow](./edsurveyWorkflow.png){ width=70% }

The workflow has three components:

1.	Understanding the data
2.	Preparing the data for analysis
3.	Running the analysis

Note that when preparing data for analysis, the `EdSurvey` package provides functions for users to manipulate their data. Experienced R programmers might prefer to extract and manipulate their data using other R methods or supplementary packages. Each method is supported.


# Conclusion
The `EdSurvey` package provides accessible and user-friendly functions to analyze large-scale educational assessment data, accounting for the complex sample survey design and the use of plausible values. For detailed function walkthroughs and the education studies supported by `EdSurvey`, consult the `EdSurvey` User's Guide [@Lee].


# Acknowledgements

This project has been funded at least in part with Federal funds from the U.S. Department of Education under contract numbers ED-IES-12-D-0002/0004 and 91990022C0053. The content of this publication does not necessarily reflect the views or policies of the U.S. Department of Education nor does mention of trade names, commercial products, or organizations imply endorsement by the U.S. Government.

# References