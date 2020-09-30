@echo off

echo/
echo -----------------------------
echo Installing core CLI utilities...
echo/
winget install -ei --id=Git.Git
winget install -ei --id=vim.vim
rem The following utilities are not yet available in the winget repositories
rem set cli-utils=curl bat neovim ranger ripgrep thefuck
rem for %%a in (%cli-utils%) do winget install -e %%a

echo/
echo -----------------------------
echo Installing IDE's...
echo/
set IDEs=GNU.Emacs Microsoft.VisualStudioCode
for %%a in (%IDEs%) do winget install -e --id=%%a

echo/
echo -----------------------------
echo Installing media packages...
echo/
set MediaPackages=Discord.Discord Spotify.Spotify OBSProject.OBSStudio VideoLAN.VLC
for %%a in (%MediaPackages%) do winget install -e --id=%%a

echo/
echo -----------------------------
echo Installing Browsers...
echo/
winget install -e --id=Mozilla.Firefox

echo/
echo -----------------------------
echo Installing Pro Gamer Moves...
echo/
set GamerPackages=Valve.Steam
for %%a in (%GamerPackages%) do winget install -e --id=%%a
