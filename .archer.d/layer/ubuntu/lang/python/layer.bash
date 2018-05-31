layer_help() {
    echo "lang/python"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v python3 && command -v pip3 # && command -v pyls
}

layer_install() {
    sudo apt-get install -y python3
    sudo apt-get install -y python3-pip
    sudo apt-get install -y python
    sudo apt-get install -y python-pip
    sudo -H pip3 install python-language-server
    sudo -H pip3 install pyls-isort
    sudo -H pip3 install pycodestyle
    sudo -H pip3 install flake8
    sudo -H pip3 install pylint
    sudo -H pip3 install importmagic
    sudo -H pip3 install epc
}
