#!/usr/bin/env bash

set -euo pipefail

sbt assembly

git add main.jar