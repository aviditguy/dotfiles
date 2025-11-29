#!/usr/bin/env bash

set -e

DOTFILES_DIR="$HOME/Workspace/dotfiles"

backup_file() {
    local target="$1"
    if [ -e "$target" ] && [ ! -L "$target" ]; then
        local backup="${target}.backup.$(date +%Y%m%d%H%M%S)"
        echo "Backing up $target -> $backup"
        mv "$target" "$backup"
    fi
}

link_dir() {
    local src="$1"
    local dest="$2"

    echo "Linking $src -> $dest"

    backup_file "$dest"

    mkdir -p "$(dirname "$dest")"
    rm -rf "$dest"
    ln -s "$src" "$dest"
}

echo "Using DOTFILES_DIR = $DOTFILES_DIR"

### Emacs
# dotfiles/emacs -> ~/.config/emacs
if [ -d "$DOTFILES_DIR/emacs.d" ]; then
    link_dir "$DOTFILES_DIR/emacs.d" "$HOME/.emacs.d"
fi

### Neovim
# dotfiles/nvim -> ~/.config/nvim
if [ -d "$DOTFILES_DIR/nvim" ]; then
    link_dir "$DOTFILES_DIR/nvim" "$HOME/.config/nvim"
fi

### i3
# dotfiles/i3 -> ~/.config/i3
if [ -d "$DOTFILES_DIR/i3" ]; then
    link_dir "$DOTFILES_DIR/i3" "$HOME/.config/i3"
fi

### mpv
# dotfiles/mpv -> ~/.config/mpv
if [ -d "$DOTFILES_DIR/mpv" ]; then
    link_dir "$DOTFILES_DIR/mpv" "$HOME/.config/mpv/"
fi

echo "All done!"

