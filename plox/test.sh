#!/bin/bash

scan1=$(swipl -q -l test.scanner.pro -t "main(\"var x = 5 + 10;\")")
exp1='state{acc:[var,id{val:x},equal,number{val:5},plus,number{val:10},semicolon],err:[],line:1}'

scan2=$(swipl -q -l test.scanner.pro -t "main(\"// var x = 5 + 10;\")")
exp2='state{acc:[comment],err:[],line:1}'

scan3=$(swipl -q -l test.scanner.pro -t "main(\"& // var x = 5 + 10;\")")
exp3='state{acc:[comment],err:[Unrecognized & on line: 1],line:1}'

[[ "$scan1" == "$exp1" ]] || echo -e "Fail on: \n $scan1 \n $exp1"
[[ "$scan2" == "$exp2" ]] || echo -e "Fail on: \n $scan2 \n $exp2"
[[ "$scan3" == "$exp3" ]] || echo -e "Fail on: \n $scan3 \n $exp3"
