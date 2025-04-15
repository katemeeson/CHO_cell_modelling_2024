# **CHO_cell_modelling_2024** (repository developed October 2024)
### Link to publication in Biotechnology and Bioengineering: https://doi.org/10.1002/bit.28982
![image](https://github.com/user-attachments/assets/e7501eb9-ebda-43a7-8748-84dc5cfa900d)
#### **Main Authors** of this repository: Dr Kate Meeson and Dr Joanne Watson
#### **Corresponding Author:** Kate Meeson: kate.meeson@manchester.ac.uk
### **Aim of project:** To explore the metabolism of CHO cells and optimise their IgG production.
### **Flux analysis method:** Flux sampling workflow using the Coordinate hit-and-run with rounding algorithm (PMID: 28158334; Haraldsdóttir et al, 2017).
### **Statistical approach:** This flux sampling workflow uses Mann-Whitney U test to identify the reactions significantly associated with the 95th percentile of high IgG-producing sampling solutions.
### **GEM, integration algorithm and collaboration:** This flux sampling algorithm has been applied to an adapted version of the iCHO2441 Chinese Hamster Ovary (CHO) genome-scale model (PMID: 36866411; Strain et al, 2023), which has been constrained using transcriptomics data generated by the Edinburgh Genomics department, as part of a Prosperity Partnership Project, funded by EPSRC and BBSRC, in collaboration with FUJIFILM Diosynth Biotechnologies, Billingham, UK. The algorithm used to constrain this model was developed by Timouma et al, 2024 (PMID: 38819150; Timouma et al, 2024).
# File descriptions:
#### 1. iCHOv1_221-107_producing.ipynb describes how the original iCHO2441 model was adapted for this project, to match the specific product profile of the FDBKA (FUJIFILM) CHO cell line.
#### 2. DeSeq2_analysis.R contains code for PCA DeSeq2 analysis and functional enrichment analysis in Figure 2.
#### 3. transcriptomics_figs.ipynb is the file with PCA, heatmap and visualisation of functional enrichment results for Figure 2.
#### 4. function_mapping_transcriptome_data_JW.py is the integration algorithm designed by Timouma et al, 2024, which enables the integration of transcriptomics (or proteomics) data as constraints of a GEM. 
#### 5. 2_runFluxSampling_CSF.py is the file containing the code to run flux sampling (on the University CSF)
#### 6. FluxSampling_analysis_and_visualisation.ipynb contains workflow for the integration of transcriptomics data into the iCHO2441 model. Followed by the import of flux sampling solutions (generated in file number 6 '2_runFluxSampling_CSF.py') and FBA, subsystem overenrichment analysis and PCA visualisation of high IgG-producing sampling solutions. Some statistical analysis of results.
#### 7. FluxSampling_highIgG_reactions_subsystemsVis.R contains code for subsystem enrichment analysis presented in Figure 5.
