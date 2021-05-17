#!/usr/bin/env bash

dart pub get
dart compile exe bin/dartdiff.dart -o dartdiff
