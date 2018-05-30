sudo dnf install -y --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

sudo dnf remove -y amarok kget knode kmail korganizer konqueror kaddressbook qupzilla qt5-qdbusviewer kwrite ktorrent dragon akregator calligra-libs

sudo dnf install -y zsh git kate cairo-dock emacs tmux fcitx fcitx-table-chinese kcm-fcitx clang cmake python3-devel qt-creator adobe-source-code-pro-fonts cppcheck zeal npm redhat-rpm-config qt5-qtdeclarative-devel neovim qt5-qtquickcontrols2-devel

sudo dnf install -y emacs-auctex texlive-collection-mathextra texlive-hyphenat texlive-overpic texlive-fncychap texlive-titlesec texlive-xecjk texlive-capt-of texlive-ulem texlive-wrapfig texlive-ctex

git clone https://github.com/Valloric/ycmd.git ~/library/ycmd



