#!/bin/bash

scan1=$(swipl -q -l test.scanner.pro -t "main()")
exp1='state{acc:[var,id{val:x},equal,number{val:5},plus,number{val:10},semicolon],err:[],line:1}'

[[ "$scan1" == "$exp1" ]] || echo -e "Fail on: \n $scan1 \n $exp1"
