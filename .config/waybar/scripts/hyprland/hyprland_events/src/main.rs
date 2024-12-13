use hyprland::data::{Clients, Workspace};
use hyprland::event_listener::EventListenerMutable as EventListener;
use hyprland::prelude::*;
use std::collections::HashMap;

fn main() -> hyprland::Result<()> {
    compute_windows();
    let mut event_listener = EventListener::new();

    event_listener.add_active_window_change_handler(|_, _| {
        compute_windows();
    });

    event_listener.add_workspace_change_handler(|_, _| {
        compute_windows();
    });

    event_listener.add_active_monitor_change_handler(|_, _| {
        compute_windows();
    });

    event_listener.add_window_open_handler(|_, _| {
        compute_windows();
    });

    event_listener.add_window_close_handler(|_, _| {
        compute_windows();
    });

    event_listener.add_window_moved_handler(|_, _| {
        compute_windows();
    });

    event_listener.start_listener()
}

fn compute_windows() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: hyprland <monitor>");
        std::process::exit(1);
    }

    let monitor: i32 = args[1]
        .parse()
        .expect("Failed to parse monitor as an integer");

    let workspace_raw = Workspace::get_active().unwrap();
    let active_workspace = workspace_raw.id;

    let clients_raw = Clients::get().unwrap();
    let mut workspace_classes = HashMap::new();

    let monitor_range: Vec<i32> = (10 * (monitor - 1) + 1..=10 * monitor).collect();

    clients_raw.iter().for_each(|client| {
        let workspace = client.workspace.id;
        let mut name = client.initial_class.clone();
        if workspace != -1
            && client.mapped == true
            && name != ""
            && monitor_range.contains(&workspace)
            && client.pid != -1
        {
            if name == "tick" || name == "tickrs" {
                name = client.title.clone();
            }
            workspace_classes
                .entry(workspace)
                .or_insert(vec![])
                .push(name);
        }
    });

    let mut text = String::new();

    for mut idx in monitor_range.clone() {
        if workspace_classes.is_empty() {
            if monitor_range.contains(&active_workspace) && active_workspace >= idx {
                while idx < active_workspace {
                    let spaces = if text.len() == 0 { "" } else { " " };
                    text += &format!("<span color=\"#7c818c\">{spaces}</span>");
                    idx += 1;
                }
                let spaces = if text.len() == 0 { "" } else { " " };
                text += &format!("<span color=\"#f5f2f2\">{spaces}</span>");
            }
            break;
        }

        let color = if idx == active_workspace {
            "#f5f2f2"
        } else {
            "#7c818c"
        };

        if let Some(classes) = workspace_classes.get(&idx) {
            let space = if workspace_classes.len() == 1 {
                ""
            } else {
                " "
            };
            if classes.contains(&"discord".to_string()) {
                text += &format!(
                    "<span color=\"{0}\" size=\"small\">{1}</span>",
                    color, space
                );
            } else if classes.contains(&"Mailspring".to_string()) {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            } else if classes.contains(&"zoom".to_string()) {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            } else if classes.contains(&"Emacs".to_string())
                || classes.contains(&"emacs".to_string()) {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            } else if classes.contains(&"Postman".to_string()) {
                text += &format!("<span color=\"{0}\">󰢩{1}</span>", color, space);
            } else if classes.contains(&"google-chrome".to_string())
                || classes.contains(&"Firefox".to_string())
                || classes.contains(&"firefox-developer-edition".to_string())
            {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            } else if classes.contains(&"feishin".to_string())
                || classes.contains(&"sonixd".to_string())
                || classes.contains(&"Sonixd".to_string())
            {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            } else if classes.contains(&"Alacritty".to_string()) {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            } else {
                text += &format!("<span color=\"{0}\">{1}</span>", color, space);
            }
        } else {
            text += &format!("<span color=\"{0}\"> </span>", color);
        }

        workspace_classes.retain(|&workspace, _| workspace > idx);
    }

    if text.len() == 0 {
        text += "<span color=\"#7c818c\"></span>";
    }
    println!("{}", text);
}
