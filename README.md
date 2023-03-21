# rave-pipelines

RAVE 2.0 interactive modules and data pipelines for reproducible analysis and visualization of intracranial electroencephalography. This repository consists of built-in RAVE (https://rave.wiki/) modules and pipelines; currently contain the entire preprocessing pipelines. All presented as interactive modules. 

## Selected Screenshots

| Notch Filter | Wavelet | Epoch Generator |
|:------------:|:-------:|:---------------:|
|<img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225415783-5a16f3d8-44a1-4add-a0ab-05805be71893.png">  |  <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225415828-8f3cacec-1080-4ea1-b106-fe0d0933c904.png">  | <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225416445-cac2d4dd-53ae-4603-bea7-43be155050ca.png"> |



| Channel Reference | CT & MRI Co-registration | Electrode Localization |
|:-----------------:|:------------------------:|:----------------------:|
| <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225416639-c140d3c1-e1ac-490b-93a0-4e876af66340.png"> | <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225417282-936787a1-9030-4152-bb83-b98401ac5688.png">|<img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225418490-dc6caa68-c477-4c5d-a90c-159a35ad815d.png">|

## How to use

Check the [installation guide](https://openwetware.org/wiki/RAVE:Install) to install `RAVE`. The pipeline will be downloaded automatically during installation/upgrade.

# Make contribution

This repository forbids directly push. All changes must be made via pull-requests. Please fork this project and edit on your own before creating pull-requests. "At" our team members or join our [slack channel](mailto:slack@rave.wiki) to request for supports.

## Why RAVE 2.0 instead of 1.0

RAVE 1.0 has the following limitations:

* Type of data to load is limited to power, phase, voltage. Custom data loading is complicated.
* The analysis code is often entangled with UI code, making it hard to just run the pipeline scripts offline (without GUI)
* The default input types are limited, the inputs UI can become very complicated when non-default arrangement is needed (which is often the case)
* The modules are designed to run simple analysis code. Users often need to run the whole pipeline in order to get results, even though partial pipeline code is needed.

The motivation of RAVE 2.0 is to overcome these limitations. Features include:

* Separate pipeline scripts and interactive UI modules. Users can run reproducible pipeline code without UI. This can speed up and scale up the pipeline when no UI is needed
* Module authors have 100% control over the UI code. They can choose to load any type of data, even for data not defined in RAVE
* The pipeline can run partially. 
* All the intermediate key variables will be saved and read easily
* Supports `Python`

## Anatomy of RAVE 2.0 pipeline module

A RAVE 2.0 pipeline module consists of two parts: 

1. Data pipeline: well structured scripts that runs pipeline without graphical user interface;
2. UI module: `R-Shiny` application providing user interface. Users set the inputs from the UI, the module drives data pipeline, and visualize the pipeline results back to users.

### Write RAVE data pipeline

Writing a RAVE data pipeline is just like writing a script (or jupyter notebook)
Think of writing a script


