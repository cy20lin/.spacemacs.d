layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v emacs25
}

layer_install() {
    # sudo apt-get install -y emacs
    yes | sudo add-apt-repository ppa:kelleyk/emacs
    sudo apt-get update
    sudo apt-get install emacs25
    sudo ln "$(which emacs25)" /usr/local/bin/emacs -s
}

