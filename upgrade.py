#!/usr/bin/python3
# -*- coding: utf-8 -*-

import pip
from subprocess import call

for dist in pip.get_installed_distributions():
    call("python3 -m pip install --upgrade " + dist.project_name, shell=True)
