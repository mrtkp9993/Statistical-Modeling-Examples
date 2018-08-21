#!/usr/bin/env bash
cd ../Statistical-Modelling-Examples/pystan/scripts
for f in *.py; do python "$f"; done
cd ../../statsmodels
for f in *.py; do python "$f"; done
