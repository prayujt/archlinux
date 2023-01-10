#include <iostream>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <thread>
#include <chrono>

#include "OpenRGB/Client.hpp"

#include <X11/Xlib.h>
#include <X11/keysym.h>

using namespace std;

using orgb::Client;
using orgb::ConnectStatus;
using orgb::RequestStatus;
using orgb::DeviceType;
using orgb::DeviceListResult;
using orgb::Device;
using orgb::LED;

unordered_set<string> super = {"D", "F", "H", "J", "K", "L", "Q", "R", "W", "Left Shift"};
unordered_set<string> supershift = {"F", "H", "J", "K", "L"};

bool key_is_pressed(KeySym ks) {
    Display *dpy = XOpenDisplay(":0");
    char keys_return[32];
    XQueryKeymap(dpy, keys_return);
    KeyCode kc2 = XKeysymToKeycode(dpy, ks);
    bool isPressed = !!(keys_return[kc2 >> 3] & (1 << (kc2 & 7)));
    XCloseDisplay(dpy);
    return isPressed;
}

int main(int argc, char *argv[]) {
    Client client;

    ConnectStatus status = client.connect("127.0.0.1", 6742);
    if (status != ConnectStatus(0)) {
        fprintf( stderr, "failed to connect: %s (error code: %d)\n", orgb::enumString( status ), int( client.getLastSystemError() ) );
        return 1;
    }

    DeviceListResult result = client.requestDeviceList();
    if (result.status != RequestStatus(0)) {
        fprintf( stderr, "failed to get device list: %s (error code: %d)\n", orgb::enumString( result.status ), int( client.getLastSystemError() ) );
        return 1;
    }

    const Device* keyboard = result.devices.find(DeviceType::Keyboard);
    if (!keyboard) {
        fprintf( stderr, "device keyboard not found.\n" );
        return 1;
    }

    client.switchToCustomMode(*keyboard);
    unordered_set<string> last_keys;
    while (1) {
        cout << "pressed" << endl;
        if (key_is_pressed(XK_Super_L) && !key_is_pressed(XK_Shift_L) &&
            (last_keys.find("super") == last_keys.end() || last_keys.find("shift") != last_keys.end())) {
            client.disconnect();
            ConnectStatus status = client.connect("127.0.0.1", 6742);
            if (status != ConnectStatus(0)) {
                fprintf( stderr, "failed to connect: %s (error code: %d)\n", orgb::enumString( status ), int( client.getLastSystemError() ) );
                return 1;
            }

            DeviceListResult result = client.requestDeviceList();
            if (result.status != RequestStatus(0)) {
                fprintf( stderr, "failed to get device list: %s (error code: %d)\n", orgb::enumString( result.status ), int( client.getLastSystemError() ) );
                return 1;
            }

            const Device* keyboard = result.devices.find(DeviceType::Keyboard);
            if (!keyboard) {
                fprintf( stderr, "device keyboard not found.\n" );
                return 1;
            }

            client.setDeviceColor(*keyboard, orgb::Color(0, 0, 0));
            vector<LED> lights = keyboard->leds;
            for (LED light : lights) {
                string key = light.name;
                size_t ind = key.find("Key: ");
                if (ind != string::npos){
                    key.erase(ind, 5); // erase function takes two parameter, the starting index in the string from where you want to erase characters and total no of characters you want to erase.
                }

                if (super.find(key) != super.end()) {
                    client.setLEDColor(light, orgb::Color(255, 0, 0));
                    this_thread::sleep_for(chrono::milliseconds(50));
                }
            }
            last_keys.emplace("super");
            last_keys.erase("shift");
        }
        if (!key_is_pressed(XK_Super_L) && last_keys.find("super") != last_keys.end()) {
            client.setDeviceColor(*keyboard, orgb::Color(0, 255, 255));
            this_thread::sleep_for(chrono::milliseconds(50));
            last_keys.erase("super");
        }
        if (key_is_pressed(XK_Super_L) && key_is_pressed(XK_Shift_L) &&
            (last_keys.find("super") == last_keys.end() || last_keys.find("shift") == last_keys.end())) {

            client.disconnect();
            ConnectStatus status = client.connect("127.0.0.1", 6742);
            if (status != ConnectStatus(0)) {
                fprintf( stderr, "failed to connect: %s (error code: %d)\n", orgb::enumString( status ), int( client.getLastSystemError() ) );
                return 1;
           }

            DeviceListResult result = client.requestDeviceList();
            if (result.status != RequestStatus(0)) {
                fprintf( stderr, "failed to get device list: %s (error code: %d)\n", orgb::enumString( result.status ), int( client.getLastSystemError() ) );
                return 1;
            }

            const Device* keyboard = result.devices.find(DeviceType::Keyboard);
            if (!keyboard) {
                fprintf( stderr, "device keyboard not found.\n" );
                return 1;
            }

            client.setDeviceColor(*keyboard, orgb::Color(0, 0, 0));
            vector<LED> lights = keyboard->leds;
            for (LED light : lights) {
                string key = light.name;
                size_t ind = key.find("Key: ");
                if (ind != string::npos){
                    key.erase(ind, 5);
                }

                if (supershift.find(key) != supershift.end()) {
                    client.setLEDColor(light, orgb::Color(255, 0, 0));
                    this_thread::sleep_for(chrono::milliseconds(50));
                }
            }
            last_keys.emplace("super");
            last_keys.emplace("shift");
        }
    }

    return 0;
}
