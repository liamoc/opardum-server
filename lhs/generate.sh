#!/bin/bash
mkdir -p tex
mkdir -p pdf
cd tex
rm -R *
cd ../pdf
rm -R *
cd ..
for file in ../Haskellpad/*.lhs; do lhs2TeX $file --tt | sed "s/char'06/char'30/g" > $file.tex; done
mv ../Haskellpad/*.tex ./tex
for file in tex/*.tex; do pdflatex $file; done
mv *.pdf pdf
rm *.aux
rm *.log