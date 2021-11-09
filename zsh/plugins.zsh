if [[ ! -d ${HOME}/.zim ]]; then
    curl -fsSL https://raw.githubusercontent.com/zimfw/install/master/install.zsh | zsh
    rm ${HOME}/.zimrc
    ln -sf ${zshDot}/zimrc.zsh $HOME/.zimrc
fi
