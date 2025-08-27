#!/bin/bash

dnf upgrade -y --refresh

# enable the RPM Fusion software repositories
dnf install -y "https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm" "https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"

# install vscode, edge
dnf config-manager setopt fastestmirror=1
dnf config-manager addrepo --save-filename=vscode --set=baseurl=https://packages.microsoft.com/yumrepos/vscode/
dnf config-manager addrepo --save-filename=microsoft-edge --set=baseurl=https://packages.microsoft.com/yumrepos/edge/

dnf install -y microsoft-edge-stable code

dnf autoremove -y
