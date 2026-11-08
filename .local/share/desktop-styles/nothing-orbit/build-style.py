from pathlib import Path
import re
base=Path.home()/'.local/state/desktop-styles/original/config'
if not base.is_dir():
 base=Path.home()/'.local/share/desktop-styles/original/config'
out=Path.home()/'.local/share/desktop-styles/nothing-orbit/config'

def write(relative,s):
 p=out/relative; p.parent.mkdir(parents=True,exist_ok=True); p.write_text(s)

s=(base/'quickshell/shell.qml').read_text()
s=s.replace('readonly property string fontFamily: "GeistMono Nerd Font Mono"','readonly property string fontFamily: "SpaceMono Nerd Font Mono"')
palette={'bgColor':'#ec111214','textColor':'#ebe9e2','subtext':'#969998','accent':'#e6393f','clockColor':'#e9e7e0','volumeColor':'#d8d8d0','mutedColor':'#ed5d62','gainColor':'#d9dfd4','surface':'#242628','inkColor':'#101112','outline':'#343638'}
for k,v in palette.items():
 s,n=re.subn(r'(readonly property color '+k+r': )"[^"]+"[^\n]*',r'\g<1>"'+v+'"',s); assert n==1,(k,n)
s=s.replace('// -- colors (neon current) --','// -- Nothing / Orbit palette --')
start=s.index('    component NeonGradient:'); end=s.index('    // -- shared state --',start)
block=s[start:end]
for color in ['clockColor','accent','gainColor']:
 block=block.replace(f'Qt.rgba(root.{color}.r, root.{color}.g, root.{color}.b, neon.tintOpacity)','Qt.rgba(root.textColor.r, root.textColor.g, root.textColor.b, neon.tintOpacity)')
s=s[:start]+'''    FontLoader {
        id: styleDotFont
        source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/Doto.ttf"
    }
    OrbitalDesktop { shellState: root }

'''+block+s[end:]
s=s.replace('color: parent.isFocused ? root.surface : "transparent"','color: parent.isFocused ? root.accent : "transparent"')
s=s.replace('                                        gradient: workspaceButton.parent.isFocused ? workspaceGradient : null\n                                        NeonGradient { id: workspaceGradient }\n','')
s=s.replace('color: parent.parent.isFocused ? root.inkColor : root.subtext','color: parent.parent.isFocused ? root.textColor : root.subtext')
start=s.index('                // -- clock face --'); end=s.index('                    Behavior on opacity',start)
block=s[start:end].replace('font.family: root.fontFamily','font.family: styleDotFont.name').replace('font.pixelSize: 12','font.pixelSize: 15\n                    font.weight: Font.Bold\n                    font.variableAxes: ({"ROND": 100})')
s=s[:start]+block+s[end:]
s=s.replace('return JSON.stringify({launcher: root.launcherOpen','return JSON.stringify({desktopStyle: "nothing-orbit", launcher: root.launcherOpen')
write('quickshell/shell.qml',s)

s=(base/'hypr/hyprland.lua').read_text()
s=s.replace('border_size = 2,','border_size = 1,').replace('rounding = 4,','rounding = 12,')
s=s.replace('{ colors = {"rgba(9cdedb99)", "rgba(b5c8ed66)"}, angle = 45 }','"rgba(e9e7e088)"')
s=s.replace('inactive_border = "rgba(78909c33)"','inactive_border = "rgba(4b4f4c44)"').replace('color = "rgba(050c1840)"','color = "rgba(00000055)"')
s+='\n-- Restore the saved glass / forest desktop.\nhl.bind("SUPER + ALT + BACKSPACE", hl.dsp.exec_cmd("/home/prayuj/.local/bin/desktop-style original"))\n'
write('hypr/hyprland.lua',s)

s=(base/'alacritty/alacritty.toml').read_text().replace('opacity = 0.80','opacity = 0.86')
colors={'#101a2b':'#101112','#e7f2ff':'#e9e7e0','#202e43':'#242628','#f28b68':'#e36a70','#b0e86f':'#b4c9b1','#f3c969':'#d8c895','#7b9eff':'#adbccb','#aa9ce8':'#c1b2c5','#4ee6e8':'#b8c9c4','#c9dbee':'#c9c9c2','#7b91aa':'#777d7c','#ffae89':'#f09a9e','#d1fa98':'#cfddc8','#ffe19b':'#e8dbb3','#a6c1ff':'#c7d1db','#c9bffc':'#ddd1df','#96f9fa':'#d1e0db','#f3faff':'#faf8ef'}
for a,b in colors.items(): s=s.replace(a,b)
write('alacritty/alacritty.toml',s)
write('hypr/hyprpaper.conf',(base/'hypr/hyprpaper.conf').read_text())
print('Nothing / Orbit profile built from the saved desktop.')
