#!/bin/bash 
set -x
echo Refreshing sbt template...
TGT_DIR=$(pwd)/..
cd ../../s_mach.sbt_templates/single_project
cp .gitignore .travis.yml LICENSE $TGT_DIR
mkdir -p $TGT_DIR/project
cd project
cp plugins.sbt *.scala refresh.sh publish_checklist.asciidoc $TGT_DIR/project
