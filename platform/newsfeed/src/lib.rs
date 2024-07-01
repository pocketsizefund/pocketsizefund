use reqwest;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::env;
use tracing::{debug, error, info, instrument, warn};

#[derive(Debug, Serialize, Deserialize)]
pub enum ClaudeModel {
    #[serde(rename = "claude-3-5-sonnet-20240620")]
    Claude35Sonnet,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Role {
    #[serde(rename = "user")]
    User,
    #[serde(rename = "system")]
    System,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Prompt {
    pub role: Role,
    pub content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ClaudeToolChoice {
    #[serde(rename = "type")]
    pub choice_type: String,
    pub name: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ClaudeTool {
    pub name: String,
    pub description: String,
    pub input_schema: Value,
}

impl ClaudeTool {
    pub fn new(
        name: String,
        description: String,
        input_schema: &str,
    ) -> Result<Self, serde_json::Error> {
        let input_schema: Value = serde_json::from_str(input_schema)?;

        Ok(ClaudeTool {
            name,
            description,
            input_schema,
        })
    }
}

const INPUT_SCHEMA: &str = r#"{
"name": "record_summary",
"description": "Record summary of an image using well-structured JSON.",
"input_schema": {
    "type": "object",
    "properties": {
        "key_colors": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "r": { "type": "number", "description": "red value [0.0, 1.0]" },
                    "g": { "type": "number", "description": "green value [0.0, 1.0]" },
                    "b": { "type": "number", "description": "blue value [0.0, 1.0]" },
                    "name": { "type": "string", "description": "Human-readable color name in snake_case, e.g. \"olive_green\" or \"turquoise\"" }
                },
                "required": [ "r", "g", "b", "name" ]
            },
            "description": "Key colors in the image. Limit to less then four."
        },
        "description": {
            "type": "string",
            "description": "Image description. One to two sentences max."
        },
        "estimated_year": {
            "type": "integer",
            "description": "Estimated year that the images was taken, if is it a photo. Only set this if the image appears to be non-fictional. Rough estimates are okay!"
        }
    },
    "required": [ "key_colors", "description" ]
}"#;

#[derive(Debug, Serialize, Deserialize)]
pub struct ClaudeMessage {
    pub model: ClaudeModel,
    pub max_tokens: u64,
    pub system: Option<String>,
    pub tool_choice: ClaudeToolChoice,
    pub tools: Vec<ClaudeTool>,
    pub messages: Vec<Prompt>,
}

impl ClaudeMessage {
    pub async fn generate(&self) -> Result<String, reqwest::Error> {
        let client = reqwest::Client::new();

        let claude_api_key = match env::var("CLAUDE_API_KEY") {
            Ok(result) => result,
            Err(e) => {
                error!("Failed to get ANTHROPIC_API_KEY: {:?}", e);
                "".to_string()
            }
        };

        let response = client
            .post("https://api.anthropic.com/v1/messages")
            .header("Content-Type", "application/json")
            .header("anthropic-version", "2023-06-01")
            .header("x-api-key", claude_api_key)
            .body(serde_json::to_string(self).unwrap())
            .send()
            .await?
            .text()
            .await?;

        Ok(response)
    }
}
