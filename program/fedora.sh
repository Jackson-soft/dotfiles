#!/bin/bash

dnf upgrade --refresh

dnf install -y dnf dnf-plugins

# enable the RPM Fusion software repositories
dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
dnf config-manager setopt fastestmirror=1

# install vscode, edge
dnf config-manager addrepo --set=baseurl=https://packages.microsoft.com/yumrepos/vscode/
dnf config-manager addrepo --set=baseurl=https://packages.microsoft.com/yumrepos/edge/

dnf install -y microsoft-edge-stable code

dnf autoremove -y
