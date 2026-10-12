import QtQuick
import QtTest
import ".."

Item {
    id: fixture
    width: 1920; height: 1032
    property var sample: ({})
    property var lastAction: []
    Dashboard {
        id: dashboard
        anchors.fill: parent
        snapshot: fixture.sample
        preferences: ({city: "New York, NY"})
        onSave: arguments => fixture.lastAction = arguments
    }
    Component.onCompleted: {
        const request = new XMLHttpRequest();
        request.open("GET", Qt.resolvedUrl("sample.json"));
        request.onreadystatechange = function() {
            if (request.readyState === XMLHttpRequest.DONE) fixture.sample = JSON.parse(request.responseText);
        };
        request.send();
    }
    TestCase {
        name: "WelcomeDashboard"
        when: windowShown && dashboard.stories.length === 16
        function test_1_scrolling_survives_stats_updates() {
            const feed = findChild(dashboard, "newsFeed");
            verify(feed.contentHeight > feed.height);
            mouseWheel(feed, 250, 200, 0, -480);
            tryVerify(() => feed.contentY > 200);
            wait(250);
            const position = feed.contentY;
            const stories = dashboard.stories;
            const snapshot = JSON.parse(JSON.stringify(fixture.sample));
            snapshot.stats.cpu = 42;
            fixture.sample = snapshot;
            wait(100);
            verify(dashboard.stories === stories);
            compare(feed.contentY, position);
        }
        function test_2_wheel_burst_moves_continuously() {
            const feed = findChild(dashboard, "newsFeed");
            feed.cancelFlick();
            feed.contentY = 0;
            let previous = 0;
            for (let notch = 0; notch < 8; notch++) {
                mouseWheel(feed, 250, 200, 0, -120, Qt.NoModifier, 0);
                for (let frame = 0; frame < 4; frame++) {
                    wait(10);
                    verify(feed.contentY >= previous, "Scrolling backwards during a downward wheel burst: " + previous + " → " + feed.contentY);
                    previous = feed.contentY;
                }
            }
            tryCompare(feed, "contentY", 768);
        }
        function test_3_reversing_wheel_cancels_queued_movement() {
            const feed = findChild(dashboard, "newsFeed");
            feed.contentY = 300;
            mouseWheel(feed, 250, 200, 0, -480, Qt.NoModifier, 0);
            wait(40);
            const turn = feed.contentY;
            mouseWheel(feed, 250, 200, 0, 120, Qt.NoModifier, 0);
            wait(40);
            verify(feed.contentY < turn, "Wheel reversal must move back immediately");
            tryCompare(feed, "contentY", turn - 96);
            mouseWheel(feed, 250, 200, 0, 12000, Qt.NoModifier, 0);
            tryCompare(feed, "contentY", 0);
            mouseWheel(feed, 250, 200, 0, -12000, Qt.NoModifier, 0);
            tryCompare(feed, "contentY", feed.contentHeight - feed.height);
        }
        function test_4_drag_interrupts_wheel_animation() {
            const feed = findChild(dashboard, "newsFeed");
            feed.contentY = 300;
            mouseWheel(feed, 250, 200, 0, -480, Qt.NoModifier, 0);
            wait(30);
            mousePress(feed, 250, 400, Qt.LeftButton, Qt.NoModifier, 0);
            mouseMove(feed, 250, 300, 20);
            mouseMove(feed, 250, 200, 20);
            tryCompare(feed, "dragging", true);
            const held = feed.contentY;
            wait(200);
            compare(feed.contentY, held);
            mouseRelease(feed, 250, 200, Qt.LeftButton, Qt.NoModifier, 0);
            feed.cancelFlick();
        }
        function test_5_prices_fade_and_cards_stay() {
            const card = findChild(dashboard, "marketBTC-USD");
            const price = findChild(card, "priceBTC-USD");
            const label = findChild(price, "fadingLabel");
            tryCompare(label, "opacity", 1);
            const previous = price.shown;
            const snapshot = JSON.parse(JSON.stringify(fixture.sample));
            snapshot.markets.find(row => row.symbol === "BTC-USD").price += 1;
            fixture.sample = snapshot;
            compare(price.shown, previous);
            wait(70); verify(label.opacity < 1);
            tryVerify(() => price.shown !== previous);
            tryCompare(label, "opacity", 1);
            fixture.sample = JSON.parse(JSON.stringify(fixture.sample));
            compare(label.opacity, 1);
            verify(findChild(dashboard, "marketBTC-USD") === card);
        }
        function test_6_weather_location() {
            mouseClick(findChild(dashboard, "changeCity"));
            const dialog = findChild(dashboard, "weatherDialog");
            tryCompare(dialog, "visible", true);
            const city = findChild(dialog, "weatherCity");
            compare(city.text, "New York, NY");
            city.text = "Gainesville, FL";
            mouseClick(findChild(dialog, "saveCity"));
            tryCompare(dialog, "visible", false);
            compare(fixture.lastAction, ["city", "Gainesville, FL"]);
        }
    }
}
