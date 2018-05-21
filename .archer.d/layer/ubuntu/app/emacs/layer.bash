layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        base
    )
}

layer_is_installed() {
    command -v emacs25
}

layer_install() {
    # sudo apt-get install -y emacs
    sudo add-apt-repository -y ppa:kelleyk/emacs
    sudo apt-get update
    sudo apt-get install -y emacs25
    sudo ln "$(which emacs25)" /usr/local/bin/emacs -s
}

