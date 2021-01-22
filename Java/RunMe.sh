#!/bin/zsh -x
here=$(dirname $0:A)
cd "$(dirname $0:A)/sudoku/target/classes"
export CLASSPATH=$CLASSPATH:/Users/carstenlawrenz/.m2/repository/org/apache/logging/log4j/log4j-api/2.1/log4j-api-2.1.jar:/Users/carstenlawrenz/.m2/repository/org/apache/logging/log4j/log4j-core/2.1/log4j-core-2.1.jar:/Users/carstenlawrenz/.m2/repository/commons-io/commons-io/2.8.0/commons-io-2.8.0.jar
time /Library/Java/JavaVirtualMachines/adoptopenjdk-15.jdk/Contents/Home/bin/java org.cars10.sudoku.Sudoku ../../../../Matrices/*.matrix  | tee "$here/run.txt"
