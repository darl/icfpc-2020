#!/usr/bin/env bash

set -euo pipefail

java -jar /solution/main.jar "$@" || echo "run error code: $?"
