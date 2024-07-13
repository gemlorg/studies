#!/bin/zsh
mvn -f /Users/gemlorg/Downloads/BTLopatin/pom.xml clean install
mvn -f /Users/gemlorg/Downloads/BTLopatin/pom.xml dependency:copy-dependencies