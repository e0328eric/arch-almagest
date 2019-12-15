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

# Install auryo
sleep 1
echo "Installing auryo"
wget https://github.com/Superjo149/auryo/releases/download/v2.4.0/auryo-2.4.0.pacman
sudo pacman -U ./auryo-2.4.0.pacman

# This now install all of the packages as I used before

# ================================================================================================
#
# Chapter 2 : Install Fonts
#
# ================================================================================================

# Install nerd fonts
git clone https://github.com/ryanoasis/nerd-fonts.git
cd nerd-font
./install.sh

# ================================================================================================
#
# Chapter 3 : Pasting Configure Files
#
# ================================================================================================

# Pasting Files
# X
sleep 1
echo "Pasting Configure Files"
echo "X relevant files"
cp $PWD/X/.xprofile ~/
cp $PWD/X/.xinitrc ~/
cp $PWD/X/.Xauthority ~/
cp $PWD/X/.Xmodmap ~/
cp $PWD/X/.Xmodmap.default ~/
cp $PWD/X/.Xmodmap.bak ~/
cp $PWD/X/.xsessionrc ~/
cp $PWD/X/.Xclients ~/

# zshrc
sleep 1
echo "Zsh files"
cp $PWD/zsh/.zshrc ~/
cp $PWD/zsh/.p10k.zsh ~/

# Installing zsh packages
sleep 1
echo "Installing zsh Plugins"
sudo pip install thefuck
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting

# Vim
sleep 1
echo "Vim files"
echo "Also install all vim plugins"
cp $PWD/vim/.vimrc ~/
cp -r $PWD/vim/localSnips ~/.vim/

# Installing Vundle
sleep 1
echo "Installing Vundle"
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# xmonad
sleep 1
echo "Xmonad files"
if [[ ! -d ~/.xmonad ]]; then
    mkdir ~/.xmonad
fi
cp $PWD/xmonad/xmonad.hs ~/.xmonad
sudo cp $PWD/xmonad/lock-alma /usr/bin/
sudo cp $PWD/xmonad/lockscreen.service /etc/systemd/ststem/

# dmenu_extended
if [[ ! -d ~/.config/dmenu-extended ]]; then
    mkdir ~/.config/dnemu-extended
fi
cp $PWD/dmemu-extended/* ~/.config/dmenu-extended/config

# polybar
sleep 1
echo "Polybar files"
cp -r $PWD/polybar ~/.config/

# zathura
sleep 1
echo "Zathura files"
cp -r $PWD/zathura ~/.config/

# ranger
sleep 1
echo "Ranger files"
cp -r $PWD/ranger ~/.config/

# qutebrowser
sleep 1
echo "QuteBrowser files"
cp -r $PWD/qutebrowser ~/.config/

# wallpapers
sleep 1
echo "Pasting wallpaper folder into home"
cp -r $PWD/wallpapers ~/

# keysetting script
sleep 1
echo "Copy Keyscript"
mkdir ~/.scrpit
cp $PWD/keysetting_xmodmap.sh ~/.script

# ================================================================================================
#
# Chapter 4 : Enable Systemd
#
# ================================================================================================

sudo systemctl enable sddm.service
sudo systemctl enable bluetooth.service
sudo systemctl enable lockscreen.service
