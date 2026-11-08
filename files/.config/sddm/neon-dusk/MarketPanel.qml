import QtQuick

Item {
    id: panel
    objectName: "marketPanel"
    property date now: new Date()
    property alias quotes: quotes
    function refresh() {
        const request = new XMLHttpRequest();
        request.open("GET", "file://" + config.marketCache);
        request.onreadystatechange = function() {
            if (request.readyState !== XMLHttpRequest.DONE)
                return;
            try {
                const items = JSON.parse(request.responseText).items;
                for (let i = 0; i < quotes.count; i++) {
                    const row = items.find(item => item.symbol === quotes.get(i).symbol);
                    if (row) {
                        for (const key of ["price", "change", "percent", "sessionDate", "asOf"])
                            quotes.setProperty(i, key, row[key]);
                        quotes.setProperty(i, "available", true);
                    }
                }
            } catch (error) {}
        };
        request.send();
    }
    function sessionLabel(kind, sessionDate, asOf) {
        const stale = asOf && now.getTime() - new Date(asOf).getTime() > 120000;
        let label = "Waiting for data";
        if (kind === "crypto" && asOf)
            label = "24h change · Trades every day";
        else if (sessionDate) {
            const session = new Date(sessionDate + "T12:00:00");
            const weekend = now.getDay() === 0 || now.getDay() === 6;
            if (weekend && session.getDay() === 5)
                label = "Friday close · " + Qt.formatDate(session, "MMM d");
            else if (Qt.formatDate(session, "yyyy-MM-dd") === Qt.formatDate(now, "yyyy-MM-dd"))
                label = "Today · vs previous close";
            else
                label = Qt.formatDate(session, "ddd, MMM d") + " · Last close";
        }
        return label + (stale ? " · Cached" : "");
    }
    function formatted(value, signed) {
        return (signed && value > 0 ? "+" : "") + Number(value).toLocaleString(Qt.locale("en_US"), "f", 2);
    }
    ListModel {
        id: quotes
        ListElement { symbol: "^GSPC"; name: "S&P 500"; kind: "index"; price: 0; change: 0; percent: 0; sessionDate: ""; asOf: ""; available: false }
        ListElement { symbol: "^IXIC"; name: "Nasdaq Composite"; kind: "index"; price: 0; change: 0; percent: 0; sessionDate: ""; asOf: ""; available: false }
        ListElement { symbol: "^DJI"; name: "Dow Jones"; kind: "index"; price: 0; change: 0; percent: 0; sessionDate: ""; asOf: ""; available: false }
        ListElement { symbol: "^RUT"; name: "Russell 2000"; kind: "index"; price: 0; change: 0; percent: 0; sessionDate: ""; asOf: ""; available: false }
        ListElement { symbol: "BTC-USD"; name: "Bitcoin"; kind: "crypto"; price: 0; change: 0; percent: 0; sessionDate: ""; asOf: ""; available: false }
        ListElement { symbol: "ETH-USD"; name: "Ethereum"; kind: "crypto"; price: 0; change: 0; percent: 0; sessionDate: ""; asOf: ""; available: false }
    }
    Component.onCompleted: refresh()
    Timer { interval: 10000; running: true; repeat: true; onTriggered: { panel.now = new Date(); panel.refresh(); } }
    Text {
        text: "MARKET SNAPSHOT"
        color: "#dcebf9"
        font.family: config.font
        font.pixelSize: 20
        font.weight: Font.DemiBold
        font.letterSpacing: 2
    }
    Text {
        y: 39
        text: "Index points / Crypto in USD"
        color: "#92aac4"
        font.family: config.font
        font.pixelSize: 14
    }
    Column {
        y: 86
        width: parent.width
        spacing: 16
        Repeater {
            model: quotes
            Rectangle {
                id: card
                required property string symbol
                required property string name
                required property string kind
                required property real price
                required property real change
                required property real percent
                required property string sessionDate
                required property string asOf
                required property bool available
                readonly property color movementColor: change > 0 ? "#9de3b1" : change < 0 ? "#ffab9a" : "#c3d4e7"
                objectName: "quote-" + symbol
                width: panel.width
                height: 144
                radius: 20
                color: "#b2132136"
                border.width: 1
                border.color: "#3b526c86"
                Text {
                    x: 24; y: 18
                    text: card.name
                    color: "#e8f1fd"
                    font.family: config.font
                    font.pixelSize: 21
                    font.weight: Font.DemiBold
                }
                Text {
                    x: 24; y: 49
                    text: panel.sessionLabel(card.kind, card.sessionDate, card.asOf)
                    color: "#96b0ca"
                    font.family: config.font
                    font.pixelSize: 12
                }
                FadingValue {
                    objectName: "price-" + card.symbol
                    x: 24; y: 72; width: 325; height: 42
                    value: card.available ? (card.kind === "crypto" ? "$" : "") + panel.formatted(card.price, false) : "—"
                    pixelSize: 31
                    fontWeight: Font.Medium
                }
                FadingValue {
                    x: parent.width - 182; y: 75; width: 154; height: 35
                    value: card.available ? panel.formatted(card.percent, true) + "%" : "—"
                    color: card.movementColor
                    pixelSize: 23
                    alignment: Text.AlignRight
                }
                FadingValue {
                    x: 24; y: 114; width: parent.width - 48; height: 20
                    value: card.available ? panel.formatted(card.change, true) + (card.kind === "crypto" ? " USD" : " pts") : ""
                    color: card.movementColor
                    pixelSize: 13
                }
            }
        }
    }
    Text {
        y: 1058
        width: parent.width
        text: "Yahoo Finance / Coinbase · ~10s refresh"
        color: "#819ab6"
        font.family: config.font
        font.pixelSize: 12
    }
}
