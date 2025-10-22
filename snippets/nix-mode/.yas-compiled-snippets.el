;;; "Compiled" snippets and support files for `nix-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'nix-mode
                     '(("dev-nix-shell"
                        "{ pkgs ? import <nixpkgs> { } }:\nwith pkgs;\nmkShell {\n  buildInputs = [ ${1:package} ];\n  shellHook = ''\n    export PROJECT_ROOT=\"\\$PWD\"\n    \n    # Define aliases: name -> command\n    declare -A aliases=(\n      [\"root\"]=\"cd \\\"\\$PROJECT_ROOT\\\"\"\n      [\"${2:alias1}\"]=\"${3:command1}\"\n      [\"${4:alias2}\"]=\"${5:command2}\"\n    )\n    \n    # Register all aliases\n    for alias_name in \"''$\\{!aliases[@]}\"; do\n      alias \"\\$alias_name=''$\\{aliases[\\$alias_name]}\"\n    done\n    \n    echo -e \"\\\\n\\\\033[33m<><><><><><> ${6:DEV} ENVIRONMENT READY <><><><><><>\\\\033[0m\\\\n\"\n    \n    # Print all aliases with descriptions\n    for alias_name in \"''$\\{!aliases[@]}\"; do\n      echo -e \"\\\\033[34m\\$alias_name\\\\033[0m\\\\t''$\\{aliases[\\$alias_name]}\"\n    done\n    echo \"\"\n  '';\n}\n$00"
                        "dev-nix-shell" t nil nil
                        "/home/gabriel/.config/doom/snippets/nix-mode/dev-nix-shell.yas"
                        nil nil)))


;;; Do not edit! File generated at Sat Oct 18 18:13:36 2025
