#!/bin/bash
# Fedora post-install setup: RPM Fusion, Microsoft repos, GUI apps.
# Usage: sudo ./fedora.sh
set -euo pipefail

if [[ $EUID -ne 0 ]]; then
    echo "Error: This script must be run as root (sudo)." >&2
    exit 1
fi

info() { printf '\033[1;34m[INFO]\033[0m  %s\n' "$*"; }

# ── System upgrade ──────────────────────────────────────────────────────────
info "Upgrading system packages …"
dnf upgrade -y --refresh

# ── RPM Fusion ──────────────────────────────────────────────────────────────
fedora_ver=$(rpm -E %fedora)
rpmfusion_free="https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-${fedora_ver}.noarch.rpm"
rpmfusion_nonfree="https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-${fedora_ver}.noarch.rpm"

if ! rpm -q rpmfusion-free-release &>/dev/null; then
    info "Enabling RPM Fusion repositories …"
    dnf install -y "$rpmfusion_free" "$rpmfusion_nonfree"
else
    info "RPM Fusion already enabled, skipping."
fi

# ── Microsoft repos (VS Code, Edge) ────────────────────────────────────────
info "Configuring Microsoft repositories …"
rpm --import https://packages.microsoft.com/keys/microsoft.asc

dnf config-manager setopt fastestmirror=1

if [[ ! -f /etc/yum.repos.d/vscode.repo ]]; then
    cat >/etc/yum.repos.d/vscode.repo <<'EOF'
[code]
name=Visual Studio Code
baseurl=https://packages.microsoft.com/yumrepos/vscode
enabled=1
gpgcheck=1
gpgkey=https://packages.microsoft.com/keys/microsoft.asc
EOF
fi
if [[ ! -f /etc/yum.repos.d/microsoft-edge.repo ]]; then
    cat >/etc/yum.repos.d/microsoft-edge.repo <<'EOF'
[microsoft-edge]
name=Microsoft Edge
baseurl=https://packages.microsoft.com/yumrepos/edge
enabled=1
gpgcheck=1
gpgkey=https://packages.microsoft.com/keys/microsoft.asc
EOF
fi

info "Installing VS Code and Edge …"
dnf install -y microsoft-edge-stable code

# ── Cleanup ─────────────────────────────────────────────────────────────────
info "Removing unused packages …"
dnf autoremove -y

info "Done."
