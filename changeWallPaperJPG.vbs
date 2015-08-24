dim wshShell
dim sUserName

Set wshShell = WScript.CreateObject("WScript.Shell")
sUserName = wshShell.ExpandEnvironmentStrings("%USERNAME%")

Set oFSO = CreateObject("Scripting.FileSystemObject")

sWinDir = oFSO.GetSpecialFolder(0)
sWallPaper = "C:\PATH\TO\LOCATION\OF\WALLPAPER\current.jpg"

wshShell.RegWrite "HKCU\Control Panel\Desktop\Wallpaper",sWallPaper

'wshShell.Run "%sWinDir%\System32\RUNDLL32.EXE user32.dll,UpdatePerUserSystemParameters",1,True
wshShell.Run "C:\Windows\System32\RUNDLL32.EXE user32.dll,UpdatePerUserSystemParameters",1, True
