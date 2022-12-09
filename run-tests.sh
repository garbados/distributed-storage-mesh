#!/usr/bin/env sh
GUILE_WARN_DEPRECATED="no" guix shell -m manifest.scm -- guile -l dsm.scm tests.scm
