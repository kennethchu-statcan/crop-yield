#!/bin/bash

currentDIR=`pwd`
   codeDIR=${currentDIR}/code
 outputDIR=${currentDIR}/output

if [ ! -d ${outputDIR} ]; then
    mkdir -p ${outputDIR}
fi

cp -r ${codeDIR} ${outputDIR}
cp    $0         ${outputDIR}/code

########################################################
##### Assemble contents of R package
packageName=stcCropYield
myRscript=${codeDIR}/main-build-package.R
stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`
stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`
Rscript ${codeDIR}/main-build-package.R ${codeDIR} ${outputDIR} ${packageName} > ${stdoutFile} 2> ${stderrFile}

##### Build the R package (create *.tar.gz file)
cd ${outputDIR}
R CMD build --md5 --no-build-vignettes ${outputDIR}/${packageName} > ${outputDIR}/stdout.R.build 2> ${outputDIR}/stderr.R.build

##### Check the newly built R package
# remember: define the environment variable R_LIBS_USER
# so this build script knows where the dependencies are,
# so that the following command can work properly.
R CMD check --as-cran ${packageName}_*.tar.gz > ${outputDIR}/stdout.R.check 2> ${outputDIR}/stderr.R.check

