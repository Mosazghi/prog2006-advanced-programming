#!/bin/bash

# Check if project name is supplied
if [ -z "$1" ]; then
	echo "No project name supplied"
	exit 1
fi

# Create a new cargo project with the supplied name
cargo new $1

# Change directory to the new project
cd $1

# Create a .gitignore file and append "target/" to it
echo "target/" >>.gitignore
