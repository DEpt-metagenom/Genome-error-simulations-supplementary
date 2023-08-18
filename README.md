# An overlooked phenomenon: complex interactions of potential error sources on the quality of bacterial *de novo* genome assemblies
Online supplementary material

## Static supplementary material
The `basic_supplementary_material` directory contains the results of the statistics of simulated genome assemblies with different types and ratios of sequencing errors (error rate, sequencing depth, optical duplicate ratio, and PCR duplicate ratio). The `.Rmd` file was knitted and the output is uploaded as gziped html. After cloning the repository by running `git clone https:...git` and unzipping `basic_supplementary_material/suppl_mat_for_review_V2.html.gz`, it can be opened in any browser. To recompile this output you need the file `load_vars.Rdata` which is located in the directory `interactive_supplementary_material`.

## Interactive Supplementary Material
This is the ShinyApp that is based on the data presented in our paper (Rádai et al. An overlooked phenomenon: complex interactions of potential error sources on the quality of bacterial *de novo* genome assemblies, under review)

This online supplementary material is used to visualize the results of multiplicative models in which the effects of sequencing errors on genome assembly quality metrics are assessed.

## Model predictions

3D scatterplot visualization of model predictions of quality metric values over a defined range of sequencing depth and error rate values and at defined optical and PCR duplicate ratio values. Predicted quality metric values can be plotted separately for each bacterial species used in the analyses.

The "best" observed value highlighted in the title of the plot refers to those values that represented the highest quality in the given set of assemblies for the given metric, in the given bacterium.

## Quality metric comparisons

Pairwise comparisons of quality metric values over the defined range of sequencing depth and error rate and at defined values of optical and PCR duplicate ratios. (Rendering these figures when comparing 3 or more metrics may take some time, because for each quality metric the prediction values have to be calculated separately.)

** IMPORTANT:** visualized results serve the purpose of helping interpretation of complex effects arising from interactions of the assessed sample parameters ( error sources), model parameter estimates and model predictions should be interpreted with caution, and be taken as broad guidelines towards understanding the modeled associations, rather than as accurate estimations (see article Discussion)!

## Running the app
There are multiple ways to run the ShinyApp.
The first way is to clone this repository by running `git clone https://github.com/DEpt-metagenom/Genome-error-simulations-supplementary.git` in a terminal and then opening `interactive_supplementary_material/overlooked_phenomenon_shinyapp.R` in Rstudio. Clicking the "Run App" button, the application is created and opens in a new window. There is a possibility that you will need to open this page in your browser for full compatibility (Firefox and Chrome/Chromium all work fine).

The second option is to build a container using Apptainer (formerly Singularity). After cloning the repository, navigate to the `interactive_supplementary_material` directory. Running the command `apptainer build overlooked_phenomenon_shinyapp.sif apptainer.def` (you need sudo permissions) will create an Apptainer image containing `load_vars.Rdata`, `overlooked_phenomenon_shinyapp.R` and all dependencies needed to run the app. Then, when you run `apptainer run overlooked_phenomenon_shinyapp.sif`, you will get the message 'You can access the ShinyApp by opening the following link in your browser: http://127.0.0.1:6186/'. The ShinyApp will launch in a few seconds, and when you open this link in your browser, the app will be ready to use.

The third option is to use Docker. We built a container with all the dependencies and data needed to run the app, and hosted the image at DockerHub. By running `docker pull deptmetagenom/overlooked-phenomenon-shinyapp` you can retrieve the image and running `docker run -p 127.0.0.1:6186:6186 overlooked-phenomenon-shinyapp` the container will be initiated. If everything worked as expected, you should see the message 'You can access the ShinyApp by opening the following link in your browser: http://127.0.0.1:6186/'. When you open the link, the ShinyApp should be available in a few seconds. The file `interactive_supplementary_material/overlooked_phenomenon_shinyapp_docker.R` contains all the modifications (linking ports to make the app available outside the container) needed to run the Docker container after it is created using `interactive_supplementary_material/Dockerfile`. Do not use this version unless you want to rebuild the Docker image.

The app is also hosted on https://shinyapps.io with limited resources and can be accessed at the following link: https://laczkol.shinyapps.io/overlooked_phenomenon_supplementary/
