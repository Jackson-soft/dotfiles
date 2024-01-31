#!/bin/bash

sudo dnf upgrade --refresh

# enable the RPM Fusion software repositories
sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
sudo dnf config-manager --enable fedora-cisco-openh264

# install vscode, edge
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
sudo dnf config-manager --add-repo https://packages.microsoft.com/yumrepos/edge
sudo dnf config-manager --add-repo https://packages.microsoft.com/yumrepos/vscode

sudo dnf install -y microsoft-edge-stable vscode

sudo dnf autoremove -y
sudo dnf remove --oldinstallonly -y
