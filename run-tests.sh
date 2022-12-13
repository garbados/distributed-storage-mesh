#!/usr/bin/env sh
GUILE_WARN_DEPRECATED="no" \
                     guix shell \
                     -m manifest.scm \
                     -- \
                     guile \
                     -l utils.scm \
                     -l dsm.scm \
                     tests.scm
