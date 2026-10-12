import QtQuick
import QtTest

Item {
    id: fixture
    width: 1920
    height: 1080
    property bool primaryScreen: true
    property var config: ({
        wallpaper: "assets/neon-dusk.png", blur: "0.7", font: "DejaVu Sans",
        clockFont: "DejaVu Sans", username: "prayuj", displayName: "Prayuj",
        session: "/usr/share/wayland-sessions/hyprland.desktop",
        newsCache: "/var/cache/sddm-news/news.json",
        marketCache: "/var/cache/sddm-news/markets.json"
    })
    property var keyboard: ({ capsLock: false })
    property var sessionModel: sessions
    property var sddm: greeter
    ListModel {
        id: sessions
        ListElement { name: "Other desktop"; file: "/usr/share/xsessions/other.desktop" }
        ListElement { name: "Hyprland"; file: "/usr/share/wayland-sessions/hyprland.desktop" }
    }
    QtObject {
        id: greeter
        property bool canPowerOff: true
        property bool canReboot: true
        property int attempts: 0
        property string submittedUser: ""
        property int submittedSession: -1
        property int submittedLength: 0
        signal loginFailed()
        signal loginSucceeded()
        signal informationMessage(string message)
        function login(user, password, session) {
            attempts++;
            submittedUser = user;
            submittedSession = session;
            submittedLength = password.length;
        }
        function reboot() {}
        function powerOff() {}
    }
    Loader { id: theme; anchors.fill: parent; source: "../Main.qml" }

    TestCase {
        name: "NeonDuskLogin"
        when: windowShown && theme.status === Loader.Ready
        function test_1_password_and_failure() {
            const password = findChild(theme.item, "password");
            const picker = findChild(theme.item, "sessionPicker");
            verify(password !== null);
            compare(picker.currentValue, fixture.config.session);
            verify(password.activeFocus);
            keyClick(Qt.Key_Return);
            compare(greeter.attempts, 0);
            keyClick(Qt.Key_A);
            keyClick(Qt.Key_B);
            compare(password.text, "ab");
            keyClick(Qt.Key_Return);
            compare(greeter.attempts, 1);
            compare(greeter.submittedUser, "prayuj");
            compare(greeter.submittedSession, 1);
            compare(greeter.submittedLength, 2);
            compare(password.text, "");
            verify(!password.enabled);
            verify(!picker.enabled);
            greeter.loginFailed();
            verify(password.enabled);
            verify(password.activeFocus);
            keyClick(Qt.Key_C);
            keyClick(Qt.Key_Return);
            compare(greeter.attempts, 2);
            greeter.loginFailed();
            keyClick(Qt.Key_D);
            keyClick(Qt.Key_Escape);
            compare(password.text, "");
        }
        function test_2_secondary_has_news_only() {
            fixture.primaryScreen = false;
            tryVerify(() => findChild(theme.item, "newsPanel") !== null);
            tryVerify(() => findChild(theme.item, "password") === null);
            tryVerify(() => findChild(theme.item, "submit") === null);
            const news = findChild(theme.item, "newsPanel");
            tryVerify(() => news.stories.length >= 5);
        }
        function test_3_mouse_scroll_survives_refresh() {
            const news = findChild(theme.item, "newsPanel");
            const feed = findChild(news, "newsFeed");
            verify(feed.contentHeight > feed.height);
            mouseWheel(feed, 200, 200, 0, -480);
            tryVerify(() => feed.contentY > 200);
            wait(250);
            const position = feed.contentY;
            const stories = news.stories;
            news.refresh();
            wait(200);
            verify(news.stories === stories);
            compare(feed.contentY, position);
        }
        function test_4_market_values_fade_without_rebuilding_cards() {
            const market = findChild(theme.item, "marketPanel");
            const card = findChild(market, "quote-BTC-USD");
            const value = findChild(card, "price-BTC-USD");
            const label = findChild(value, "valueLabel");
            tryVerify(() => market.quotes.get(4).available);
            tryVerify(() => value.shown !== "—");
            tryCompare(label, "opacity", 1);
            const price = market.quotes.get(4).price;
            const previous = value.shown;
            market.quotes.setProperty(4, "price", price + 1);
            compare(value.shown, previous);
            wait(70);
            verify(label.opacity < 1);
            tryVerify(() => value.shown !== previous);
            tryCompare(label, "opacity", 1);
            market.quotes.setProperty(4, "asOf", new Date().toISOString());
            compare(label.opacity, 1);
            market.refresh();
            wait(400);
            verify(findChild(market, "quote-BTC-USD") === card);
            const now = market.now;
            market.now = new Date("2026-10-10T12:00:00");
            compare(market.sessionLabel("index", "2026-10-09", new Date().toISOString()).slice(0, 12), "Friday close");
            verify(market.sessionLabel("crypto", "", new Date().toISOString()).indexOf("24h") === 0);
            market.now = now;
        }
    }
}
