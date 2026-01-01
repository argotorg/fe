#!/bin/bash

# Rewrite commit messages without replaying diffs
git filter-branch -f --msg-filter '
case "$GIT_COMMIT" in
    f585a78c1d241da606eb7cccb7dbeeb72d2aed31)
        echo "Refactor diagnostics to use semantic analysis"
        ;;
    bce6042e7c90c94eb050793c2b4c7b7df2006048)
        echo "Move definition analysis logic to semantic module"
        ;;
    a327466c76e5e3333a07d5400e82f0926efa040e)
        echo "Expand semantic path resolution API"
        ;;
    988515067ac4cee226358fbbae2e1e1ada1cfa04)
        echo "Remove unused code from path resolver and def analysis"
        ;;
    7afaae53888bb0dfad48432e1d2b07778cf03d53)
        echo "Continue semantic module expansion"
        ;;
    5cac36c76b1af21526b6f0c03b568fd2e49b1372)
        echo "Add error handling to semantic path resolution"
        ;;
    ac7b39c7fd10347ea28cb52e9fea5e111bc15362)
        echo "Fix error handling in semantic module"
        ;;
    51cb20c4b6c7211cf10fa3fd1d316c87dbbf4ff9)
        echo "Add semantic type error reporting"
        ;;
    52b3b86e140ffb5b13b4c3bc79659db990a2945c)
        echo "Expand semantic definition analysis"
        ;;
    fe945c7219de7db2c1535be33a12670018595e91)
        echo "Add impl associated type view scaffolding"
        ;;
    74ea2bcf129bc7d2b6e2b233b69f74bbf29f626d)
        echo "Expand trait resolution with semantic bounds API"
        ;;
    dcb44b7604aa31db6f148221af7734616f604ca8)
        echo "Sketch generic parameters semantic API"
        ;;
    9db7dccd9a121820e379ab911e5e3dbc7a776ff2)
        echo "Refactor ADT analysis to use semantic traversal"
        ;;
    1a53a4857c65da9f8e485d73198c1a180fcb06a8)
        echo "Begin semantic associated type traversal API"
        ;;
    *)
        cat
        ;;
esac
' master..HEAD

echo "Done! Commit messages have been rewritten."
echo "Note: This creates new commit hashes. If you've already pushed, you'll need to force push."
