#!/usr/bin/env python3

import re
import subprocess
import sys

def parse_workspaces(output):
    windows = re.findall(r'Window (\S+) -> (.+?):\n([\s\S]*?)(?=\n\n|$)', output)
    result = {}

    for window_id, title, window_info in windows:
        workspace_match = re.search(r'workspace: (\d+)', window_info)
        class_match = re.search(r'class: (.+)', window_info)

        if workspace_match and class_match:
            workspace = int(workspace_match.group(1))
            window_class = class_match.group(1)

            if workspace not in result:
                result[workspace] = []

            result[workspace].append(window_class)

    return result

def get_active_workspace(active_workspace_output):
    pattern = r'workspace ID (\d+)'

    match = re.search(pattern, active_workspace_output)

    if match:
        workspace_id = match.group(1)
        return int(workspace_id)
    else:
        return None

if len(sys.argv) < 2:
    print('Usage: hyprland.py <monitor>')
    sys.exit(1)

# Monitor 1: workspaces 1-10
# Monitor 2: workspaces 11-20
monitor = int(sys.argv[1])

workspaces = subprocess.check_output(['hyprctl', 'clients']).decode('utf-8')
active_workspace = subprocess.check_output(['hyprctl', 'activeworkspace']).decode('utf-8')

workspace_classes = parse_workspaces(workspaces)
active_workspace = get_active_workspace(active_workspace)

workspace_classes = {
    workspace: classes for workspace, classes in workspace_classes.items()
    if (workspace <= 10 * monitor) and (workspace > 10 * (monitor - 1))}

text = ''

for idx in range(10 * (monitor - 1) + 1, 10 * monitor + 1):

    if len(workspace_classes) == 0:
        if active_workspace <= 10 * monitor and active_workspace > 10 * (monitor - 1) and active_workspace >= idx:
            while idx < active_workspace:
                text += '<span color=\"#7c818c\">   </span>'
                idx += 1
            text += '<span color=\"#f5f2f2\">   </span>'
        break

    color = '#7c818c' if idx != active_workspace else '#f5f2f2'

    if idx in workspace_classes:
        space = '' if len(workspace_classes) == 1 else '   '
        if 'discord' in workspace_classes[idx]:
            text += '<span color=\"{0}\">{1}</span>'.format(color, space)
        elif 'Emacs' in workspace_classes[idx]:
            text += '<span color=\"{0}\">{1}</span>'.format(color, space)
        elif 'google-chrome' in workspace_classes[idx] or 'Firefox' in workspace_classes[idx]:
            text += '<span color=\"{0}\">{1}</span>'.format(color, space)
        elif 'Alacritty' in workspace_classes[idx]:
            text += '<span color=\"{0}\">{1}</span>'.format(color, space)
        else:
            text += '<span color=\"{0}\">{1}</span>'.format(color, space)

    else:
        text += '<span color=\"{0}\">{1}</span>'.format(color, space)

    workspace_classes = {workspace: classes for workspace, classes in workspace_classes.items() if (workspace > idx)}

print(text)
