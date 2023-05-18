#!/usr/bin/Rscript
args <- commandArgs(trailingOnly=TRUE);

if (length(args) < 1) {
    stop("Usage: args[0] research_question.yml")
} else {
    yml <- args[1];
}

yaml::read_yaml(yml)

