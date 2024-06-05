## NoButter

The CosMX Spatial Molecular Imager (SMI) offers a high-plex in-situ based solution to spatial multiomics analysis, allowing for the quantification and visualisation of RNA and protein analytes at cellular and subcellular resolution. By leveraging CosMx SMI, individual cells can be identified in their natural environment in both formalin-fixed paraffin-embedded (FFPE) and fresh frozen (FF) tissue samples.

The CosMx experimental workflow involves first sectioning tissue into thin slices, typically 5 µm in size. Probes are then applied to these sections to hybridize with target RNA or protein analytes. Following this, the tissue sample is washed, and incubated with oligolabeled antibodies required for cell segmentation staining. 

The flow cell is then assembled and loaded onto the CosMx SMI instrument for imaging. Here, Fields of View (FOV) are selected, which represent areas within the tissue (up to 300mm2) that will be imaged. The instrument automates rounds of reporter binding and fluorescent imaging to read out the barcodes on each imaged RNA probe or protein antibody.

During the imaging process, images are captured at multiple focal planes along the Z-axix called Z stacks. Z stacks are used in the morphology and cell segmentation process. Each Z stack represents a ~0.8um tissue slice, used to reconstruct a 3D image of the tissue. Z stacks are subsequently collapsed into 2D representations and transferred to the AtoMx Spatial Informatics Platform (SIP), alongside the CosMx data readout.

One of the critical challenges with higher Z stack levels is the non-specificity of transcripts. At these levels, transcripts often appear outside the expected tissue boundaries, suggesting a degree of uncertainty regarding the exact cellular origin of these transcripts. This non-specificity can obscure true biological signals and complicate downstream analysis. Noise from non-specific transcripts can lead to inaccurate quantification of gene expression levels, making it challenging to accurately identify and categorisee cells based on their transcriptomic profiles. 

To address this challenge, we developed NoButter, an R package equipped with tools to analyse the distribution of transcripts across different Z stacks, identify patterns of uneven distribution and compare these to actual tissue structure. Additionally, NoButter provides methods to remove problematic Z stacks and reconstruct the flat files required for downstream analysis.

## Installation

To install the package from GitHub, run the following commands in R:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Load devtools
library(devtools)

# Install the package
devtools::install_github("BeibhinnOH/NoButter")
