#!/bin/bash
echo " ##################################################################"
echo " ########### Clone my Hotfiles From Github ########################"
echo " ##################################################################"
## Create .config File && aleardy existe after enter Fish Sheell
mkdir -p ~/.config

# Clone My Hotfiles
git clone https://github.com/frhxm/Hotfiles

echo " ################################################################## "
echo " ######## Move Every File Hotfiles To .Config ##################### "
echo " ################################################################## "
# In Hotfiles
cd Hotfiles/

echo "########################"
echo "### Move X11 File ######"
echo "########################"
mv .xinitrc ~/
mv .Xresources ~/

echo "########################"
echo "### Move .Config File ###"
echo "########################"
mv .stalonetrayrc ~/

cd .config
mv xmonad ~/.config/
mv xmobar ~/.config/
mv dunst/ ~/.config/
mv rofi/ ~/.config/
mv sxiv/ ~/.config/
mv kitty/ ~/.config/
mv picom/ ~/.config/
mv nvim/ ~/.config/
mv qutebrowser/ ~/.config/
mv neofetch/ ~/.config/
xdg-user-dirs-update
cp -f user-dirs.dirs ~/.config/
cp -f fish/config.fish ~/.config/fish/
mv fish/functions/* ~/.config/fish/functions
mkdir -p ~/.local/share/fonts
mv ../.local/share/fonts/* ~/.local/share/fonts
mv ../pix/ ~/

echo " ########################### "
echo " ### Directory In Home ##### "
echo " ########################### "
mkdir ~/desk
mkdir ~/dl
mkdir ~/dox
mkdir ~/music
mkdir -p ~/prjcts/{siteWeb/task,app,scripts,farmeWork}
mkdir -p ~/vids/{selfCare,dev201}
mkdir ~/pub
# Deleted Old Folder
# trash-put Desktop/ Documents/ Downloads/ Music/ Pictures/ Public/ Templates/ Videos/

echo "##########################"
echo "#### NewTab Web Page ####"
echo "##########################"
cd ~/prjcts/siteWeb/
git clone https://github.com/frhxm/newtab

echo " ############################### "
echo " ##### Permision Files ######### "
echo " ############################### "
# .xinitrc
chmod +x ~/.xinitrc
# Sxiv (image viewr Scripts)
chmod +x ~/.config/sxiv/exec/key-handler
# Better Notify Voume
chmod +x ~/.config/xmobar/scripts/volume.sh
# Pacman Update in XMobar
chmod +x ~/.config/xmobar/scripts/pacupdate.sh
# Battery Low Notify alert
chmod +x ~/.config/xmobar/scripts/battnotify.sh

echo "##############################"
echo "###### Pacman Things #########"
echo "##############################"
## Remove vim After Install NeoVim
sudo pacman -Rsn vim

## enable bluetooth
#sudo systemctl enable bluetooth

echo "##############################"
echo "###### Pacman Hooks #########"
echo "##############################"
###### Create Folder Hooks (auto Commande After Action)
sudo mkdir -p /etc/pacman.d/
cd /etc/pacman.d/

###### Download my Action from my Repo
sudo git clone https://github.com/FrhXM/hooks

echo "##############################"
echo "###### another Things ########"
echo "##############################"
# Open Terminal here in Nemo
gsettings set org.cinnamon.desktop.default-applications.terminal exec kitty

echo " ########################################################### "
echo " ################ EveryThings Is Ready ##################### "
echo " ########################################################### "
sleep 5
exit
