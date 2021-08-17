#!/usr/bin/bash


antlr_path=/usr/local/lib/antlr-4.9.2-complete.jar

java -Xmx500M -cp "$antlr_path:$CLASSPATH" org.antlr.v4.Tool Bil.g4
javac Bil*.java
