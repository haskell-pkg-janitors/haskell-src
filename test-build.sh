#!/bin/bash

stack --stack-yaml stack-ghc-8.4.yaml build || exit 1
stack --stack-yaml stack-ghc-8.6.yaml build || exit 1
stack --stack-yaml stack-ghc-8.8.yaml build || exit 1
