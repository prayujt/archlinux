use reqwest::{Error as ReqwestError, Client};
use serde_json::Error as SerdeError;
use serde::{Deserialize, Serialize};
use tokio::time;

use std::env;
use std::fmt;
use std::thread;
use std::time::Duration;

#[derive(Serialize, Deserialize, Debug)]
struct TopStoriesResponse {
    status: String,
    copyright: String,
    section: String,
    last_updated: String,
    num_results: u32,
    results: Vec<Article>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Article {
    section: String,
    subsection: String,
    title: String,
    r#abstract: String,
    url: String,
    published_date: String,
}


#[derive(Debug)]
enum FetchError {
    HttpError(ReqwestError),
    JsonError(SerdeError),
}

impl fmt::Display for FetchError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FetchError::HttpError(ref err) => write!(f, "HTTP Error: {}", err),
            FetchError::JsonError(ref err) => write!(f, "JSON Parsing Error: {}", err),
        }
    }
}

impl From<ReqwestError> for FetchError {
    fn from(err: ReqwestError) -> FetchError {
        FetchError::HttpError(err)
    }
}

impl From<SerdeError> for FetchError {
    fn from(err: SerdeError) -> FetchError {
        FetchError::JsonError(err)
    }
}

async fn fetch_top_stories(r#type: &str) -> Result<TopStoriesResponse, FetchError> {
    let url = format!("https://api.nytimes.com/svc/topstories/v2/{}.json?api-key=0DGXRjtdyc48NvfuOeXw277qRgauGft2", r#type);
    let client = Client::new();
    let response = client.get(url).send().await?;
    let body = response.text().await?;
    let parsed_body: Result<TopStoriesResponse, serde_json::Error> = serde_json::from_str(&body);

    parsed_body.map_err(FetchError::from)
}

#[tokio::main]
async fn main() -> Result<(), FetchError> {
    loop {
        let args: Vec<String> = env::args().collect();
        if args.len() < 3 {
            eprintln!("Usage: {0} <width> <speed>", args[0]);
            return Ok(());
        }
        let width: usize = args[1].parse().expect("Width should be a number");
        let speed: f64 = args[2].parse().expect("Speed should be a number");

        let data = fetch_top_stories("politics").await?;
        let mut full_text = String::new();
        for article in data.results {
            full_text += format!("{} - {} | ", article.title, article.r#abstract).as_str();
        }

        // This loop prints the scrolling text
        for _ in 0..(30.0 * 60.0 / speed) as usize {
            let cycled_chars = full_text.chars().cycle();
            for start in 0..full_text.len() {
                let chunk: String = cycled_chars.clone().skip(start).take(width).collect();
                println!("{}", chunk);
                thread::sleep(Duration::from_secs_f64(speed));
            }
        }

        // Wait for 30 minutes before restarting
        time::sleep(Duration::from_secs(30 * 60)).await;
    }
}
