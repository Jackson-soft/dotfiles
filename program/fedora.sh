#!/bin/bash

dnf upgrade --refresh

if ! command -v dnf5 &> /dev/null; then
    dnf install -y dnf5
fi

# enable the RPM Fusion software repositories
dnf5 install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
dnf5 config-manager setopt fastestmirror=1

# install vscode, edge
dnf5 config-manager addrepo --set=baseurl=https://packages.microsoft.com/yumrepos/vscode/
dnf5 config-manager addrepo --set=baseurl=https://packages.microsoft.com/yumrepos/edge/

dnf5 install -y microsoft-edge-stable code

dnf5 autoremove -y
dnf5 remove -y dnf
