package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync/atomic"
	"time"
)

var apiKey = "0DGXRjtdyc48NvfuOeXw277qRgauGft2"

type TopStoriesResponse struct {
	Status      string    `json:"status"`
	Copyright   string    `json:"copyright"`
	Section     string    `json:"section"`
	LastUpdated string    `json:"last_updated"`
	NumResults  uint32    `json:"num_results"`
	Results     []Article `json:"results"`
}

type Article struct {
	Section       string `json:"section"`
	Subsection    string `json:"subsection"`
	Title         string `json:"title"`
	Abstract      string `json:"abstract"`
	URL           string `json:"url"`
	PublishedDate string `json:"published_date"`
}

func fetchTopStories(section string) (TopStoriesResponse, error) {
	var out TopStoriesResponse
	if apiKey == "" {
		return out, fmt.Errorf("NYTimes API key is empty; set the 'apiKey' variable at the top of the file")
	}

	url := fmt.Sprintf("https://api.nytimes.com/svc/topstories/v2/%s.json?api-key=%s", section, apiKey)
	client := &http.Client{Timeout: 15 * time.Second}

	resp, err := client.Get(url)
	if err != nil {
		return out, fmt.Errorf("HTTP error: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 4<<10))
		return out, fmt.Errorf("unexpected status %d: %s", resp.StatusCode, string(body))
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return out, fmt.Errorf("read body error: %w", err)
	}

	if err := json.Unmarshal(body, &out); err != nil {
		return out, fmt.Errorf("JSON parsing error: %w", err)
	}
	return out, nil
}

func buildFullText(resp TopStoriesResponse) string {
	var b strings.Builder
	for _, article := range resp.Results {
		fmt.Fprintf(&b, "%s - %s | ", article.Title, article.Abstract)
	}
	full := b.String()
	if strings.TrimSpace(full) == "" {
		full = "No articles available at the moment. | "
	}
	return full
}

// windowRunes returns a wrapping window of size width from rs starting at start.
func windowRunes(rs []rune, start, width int) []rune {
	if width <= 0 {
		return []rune{}
	}
	n := len(rs)
	if n == 0 {
		return []rune{}
	}
	out := make([]rune, width)
	for i := 0; i < width; i++ {
		out[i] = rs[(start+i)%n]
	}
	return out
}

// payload is the atomic value we swap between the marquee loop and the updater.
type payload struct {
	Text []rune
}

func main() {
	args := os.Args
	if len(args) < 3 {
		fmt.Fprintf(os.Stderr, "Usage: %s <width> <speed>\n", args[0])
		return
	}
	width, err := strconv.Atoi(args[1])
	if err != nil || width <= 0 {
		fmt.Fprintln(os.Stderr, "Width should be a positive number")
		return
	}
	speed, err := strconv.ParseFloat(args[2], 64)
	if err != nil || speed <= 0 {
		fmt.Fprintln(os.Stderr, "Speed should be a positive number (seconds)")
		return
	}

	sleepDur := time.Duration(speed * float64(time.Second))

	var current atomic.Value
	current.Store(payload{Text: []rune("Loading...")})

	go func() {
		for {
			current.Store(payload{Text: []rune("Refreshing...")})

			resp, err := fetchTopStories("politics")
			var text string
			if err != nil {
				text = "Fetch failed; will retry in 30 minutes. | "
			} else {
				text = buildFullText(resp)
			}

			rs := []rune(text)
			if len(rs) == 0 {
				rs = []rune(" | ")
			}
			current.Store(payload{Text: rs})

			time.Sleep(30 * time.Minute)
		}
	}()

	start := 0
	for {
		p := current.Load().(payload)
		rs := p.Text
		if len(rs) == 0 {
			rs = []rune(" | ")
		}

		chunk := windowRunes(rs, start, width)
		fmt.Println(string(chunk))
		start++
		if start >= len(rs) {
			start = 0
		}
		time.Sleep(sleepDur)
	}
}
