## Quadrat Analysis

### Description

Analysis code for estimating plant coverage from photos of quadrats located around Black Tusk and Taylor Meadows in Garibaldi Provincial Park (see parent folder README for more details). Due to file size constraints, these photos are not hosted on this GitHub page.

Contributors: 
Carly Hilbert (chilbe02@student.ubc.ca) - preliminary photo cropping and collation
Allen Zhao (allen10to11@gmail.com) - Image analysis code.

Last edited: 18 June 2023

Methods: Using a script, we boosted the saturation of our image to easily identify and isolate pixels that are predominantly green. This was found to be a reliable indicator of green vegetation in our images. We then find use the percentage of the image area occupied by the images as our coverage value.

### Data files

### raw_data

For housing image data files prior to processing.

#### output_data

final_coverage_data.csv: Calculated plant percent cover values for each quadrat (Allen Zhao, allen10to11@gmail.com); created on 27 March 2023; final version; analyses in Quadrat_Analysis folder

#### Scripts
All scripts are written in Python 3.10.

dirtools.py: Basic file and directory handling functions

get_cover.py: Image data reader and percent cover-calculating functions.

coverage_getter.py: Runs percent cover code sequentially over all images in /raw_data/

Dependencies:

* Numpy
* Matplotlib (for optional coverage visualizer)
* exifread
