layer_help() {
    echo "lang/javascript"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v node &&
    command -v npm &&
    command -v tern &&
    test -f ~/.eslintrc.json &&
    test -f ~/.tern-config
}

layer_install() {
    git clone https://github.com/nodesource/distributions /tmp/archer/source/nodejs/distributions
    pushd /tmp/archer/source/nodejs/distributions
    git checkout 1fb7fd56201760a62581b61ac2e51013c0c3766c
    popd
    sudo bash /tmp/archer/source/nodejs/distributions/deb/setup_10.x
    sudo apt-get update
    sudo apt-get install -y nodejs
    sudo apt-get install -y nodejs-legacy
    sudo apt-get install -y npm
    sudo apt-get install -y build-essential
    npm config set prefix /usr/local
    sudo npm install -g node-gyp
    sudo npm install -g tern
    sudo npm install -g js-beautify
    sudo npm install -g eslint
    echo '{'                        >~/.tern-config
    echo '    "plugins": {'         >>~/.tern-config
    echo '        "node": {},'      >>~/.tern-config
    echo '        "es_modules": {}' >>~/.tern-config
    echo '    },'                   >>~/.tern-config
    echo '    "libs": ['            >>~/.tern-config
    # echo '        "ecma5",'         >>~/.tern-config
    # echo '        "ecma6"'          >>~/.tern-config
    echo '    ],'                   >>~/.tern-config
    echo '    "ecmaVersion": 6'     >>~/.tern-config
    echo '}'                        >>~/.tern-config

    echo '{'                                    >~/.eslintrc.json
    echo '    "root": true,'                   >>~/.eslintrc.json
    echo '    "parserOptions": {'              >>~/.eslintrc.json
    echo '        "ecmaVersion": 6,'           >>~/.eslintrc.json
    echo '        "sourceType": "module",'     >>~/.eslintrc.json
    echo '        "ecmaFeatures": {'           >>~/.eslintrc.json
    echo '            "jsx": true'             >>~/.eslintrc.json
    echo '        }'                           >>~/.eslintrc.json
    echo '    },'                              >>~/.eslintrc.json
    echo '    "rules": {'                      >>~/.eslintrc.json
    echo '        "semi": ["warn"],'           >>~/.eslintrc.json
    echo '        "indent": ["warn", 2],'      >>~/.eslintrc.json
    echo '        "strict": ["warn", "safe"],' >>~/.eslintrc.json
    echo '        "semi-spacing":["warn"],'    >>~/.eslintrc.json
    echo '        "no-undef":["warn"]'         >>~/.eslintrc.json
    echo '    },'                              >>~/.eslintrc.json
    echo '    "env": {'                        >>~/.eslintrc.json
    echo '        "node": true'                >>~/.eslintrc.json
    echo '    }'                               >>~/.eslintrc.json
    echo '}'                                   >>~/.eslintrc.json
    echo ''                                    >>~/.eslintrc.json
}
