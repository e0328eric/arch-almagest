#! /usr/bin/sh

# Arch Linux Almaegst Setting Installer
#
# Lock Screen : sddm
# Window Manager : Xmonad
#
# This installer must be used after install arch linux
# at current computer
# If you want to intalling arch linux itself, then run another sh file
#
# Also, you must run this installer at user not root!
if [[ $USER == "root" ]]
then
    echo "You must run this installer not in root"
    exit 1
fi

# ================================================================================================
#
# Chapter 1 : Install and Upgrade Packages
#
# ================================================================================================

echo "Installing packages"

# Install wget
sleep 1
echo "Installing wget"
sudo pacman -S wget

# Install auryo
sleep 1
echo "Installing auryo"
wget https://github.com/Superjo149/auryo/releases/download/v2.4.0/auryo-2.4.0.pacman
sudo pacman -U ./auryo-2.4.0.pacman

# Install yay
sleep 1
echo "Installing yay"

git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

# Install all pacman packages
# Package list are in ./pacman/pacman-installed.txt
sleep 1
echo "Installing Main Packages"

yay -Syyu
yay -S $(cat ./pacman/pacman-installed.txt)

# Installing zsh packages
sleep 1
echo "Installing zsh Plugins"
sudo pip install thefuck
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting

# Installing Vundle
sleep 1
echo "Installing Vundle"
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# This now install all of the packages as I used before

# ================================================================================================
#
# Chapter 2 : Pasting Configure Files
#
# ================================================================================================

# Pasting Files
# X
sleep 1
echo "Pasting Configure Files"
echo "X relevant files"
cp ./X/.X* ~/
cp ./X/.x* ~/

# zshrc
sleep 1
echo "Zsh files"
cp ./zsh/* ~/*

# Vim
sleep 1
echo "Vim files"
echo "Also install all vim plugins"
cp ./vim/.vimrc ~/
cp -r ./vim/localSnips ~/.vim/


# xmonad
sleep 1
echo "Xmonad files"
if [[ ! -d ~/.xmonad ]]; then
    mkdir ~/.xmonad
fi
cp ./xmonad/xmonad.hs

# dmenu_extended
if [[ ! -d ~/.config/dmenu-extended ]]; then
    mkdir ~/.config/dnemu-extended
fi
cp "./dmemu-extended/*" "~/.config/dmenu-extended/config"

function pasteFiles {
if [[ ! -d ~/.config/$1 ]]; then
    mkdir ~/.config/polybar
fi
cp "./$1/*" "~/.config/$1/"
}


# polybar
sleep 1
echo "Polybar files"
pasteFiles "polybar"

# zathura
sleep 1
echo "Zathura files"
pasteFiles "zathura"

# ranger
sleep 1
echo "Ranger files"
pasteFiles "ranger"

# qutebrowser
sleep 1
echo "QuteBrowser files"
pasteFiles "qutebrower"

# wallpapers
sleep 1
echo "Pasting wallpaper folder into home"
cp -r ./wallpapers ~/

# keysetting script
sleep 1
echo "Copy Keyscript"
mkdir ~/.scrpit
cp ./keysetting_xmodmap.sh ~/.script
