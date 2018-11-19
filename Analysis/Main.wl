(* ::Package:: *)

Needs @ "GroupAnalyzer`";


m11 = {{0, 3}, {2, 4}};
m12 = {{0, 1}, {6, 0}};
m13 = {{3, 0}, {0, 5}};
m14 = {{1, 1}, {0, 1}};
group1 = {m11, m12, m13, m14};
mod = 7;


m21 = m11;
m22 = m12 . MatrixPower[m21, 2] . m12;
group2 = {m21, m22};


currentGroup = group2;


g1 = generateGroup[currentGroup, mod]
g2 = removeEqualElements[g1, mod]



