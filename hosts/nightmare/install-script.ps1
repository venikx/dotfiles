function Install-My-Apps {
    param (
        $apps
    )

    Foreach ($app in $apps) {
        $listApp = winget list --exact -q $app.name
        if (![String]::Join("", $listApp).Contains($app.name)) {
            Write-host "Installing:" $app.name
            if ($app.source -ne $null) {
                winget install --exact --silent $app.name --source $app.source
            } else {
                winget install --exact --silent $app.name 
            }
        } else {
            Write-host "Skipping Install of " $app.name
        }
    }
}

$dev = @(
    @{name = "Git.Git" }, 
    @{name = "Microsoft.VisualStudioCode" }
    
);

$gaming = @(
    @{name = "Valve.Steam" }, 
    @{name = "RiotGames.LeagueOfLegends.EUW" },
    @{name = "EpicGames.EpicGamesLauncher" } 
);

$communication = @(
    @{name = "Discord.Discord" }
);

$browsers = @(
    @{name = "BraveSoftware.BraveBrowser" },
    @{name = "Mozilla.Firefox.DeveloperEdition" }
);

$security = @(
    @{name = "AgileBits.1Password" }
);

Write-Output "Installing development packages..."
Install-My-Apps $dev

Write-Output "Installing gaming packages..."
Install-My-Apps $gaming

Write-Output "Installing communication packages..."
Install-My-Apps $communication

Write-Output "Installing browsers..."
Install-My-Apps $browsers

Write-Output "Installing security packages..."
Install-My-Apps $security

# Also do the following to optimize for gaming
# https://www.reddit.com/r/OptimizedGaming/comments/su6cq7/windows_1011_optimization_guide/

#Remove Apps
#Write-Output "Removing Apps"

#$apps = "*3DPrint*", "Microsoft.MixedReality.Portal"
#Foreach ($app in $apps)
#{
#  Write-host "Uninstalling:" $app
#  Get-AppxPackage -allusers $app | Remove-AppxPackage
#}