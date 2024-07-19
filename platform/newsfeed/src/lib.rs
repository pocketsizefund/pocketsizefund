use serde::{Deserialize, Serialize};
use std::env;
use tracing::error;

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
pub struct ClaudeMessage {
    pub model: ClaudeModel,
    pub max_tokens: u64,
    pub system: Option<String>,
    pub tool_choice: Option<serde_json::Value>,
    pub tools: Vec<serde_json::Value>,
    pub messages: Vec<Prompt>,
}

impl ClaudeMessage {
    pub async fn generate(&self) -> Result<String, reqwest::Error> {
        let client = reqwest::Client::new();

        let claude_api_key = match env::var("CLAUDE_API_KEY") {
            Ok(result) => result,
            Err(e) => {
                error!("Failed to get CLAUDE_API_KEY: {:?}", e);
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

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct SentimentResponse {
    pub confidence: u64,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct ClaudeResponseContent {
    #[serde(rename = "type")]
    type_: String,
    id: String,
    name: String,
    pub input: SentimentResponse,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct ClaudeResponseUsage {
    input_tokens: u64,
    output_tokens: u64,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct ClaudeResponse {
    pub id: String,
    #[serde(rename = "type")]
    pub type_: String,
    pub role: String,
    pub model: String,
    pub content: Vec<ClaudeResponseContent>,
    pub stop_reason: String,
    pub stop_sequence: Option<String>,
    pub usage: ClaudeResponseUsage,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chat_response_deserialization() {
        let chat_response = r#"{"id":"msg_01BxFThtEVBZqZWRh5YQ5AM1","type":"message","role":"assistant","model":"claude-3-5-sonnet-20240620","content":[{"type":"tool_use","id":"toolu_01QkmBQYKUd6tKi8EyQ63MUn","name":"earnings_report_sentiment","input":{"confidence":7}}],"stop_reason":"tool_use","stop_sequence":null,"usage":{"input_tokens":23641,"output_tokens":33}}"#;

        let sentiment_data: ClaudeResponse = match serde_json::from_str(&chat_response) {
            Ok(data) => data,
            Err(e) => {
                panic!("Failed to parse sentiment data: {:?}", e);
            }
        };

        let expected = ClaudeResponse {
            id: "msg_01BxFThtEVBZqZWRh5YQ5AM1".to_string(),
            type_: "message".to_string(),
            role: "assistant".to_string(),
            model: "claude-3-5-sonnet-20240620".to_string(),
            content: vec![ClaudeResponseContent {
                type_: "tool_use".to_string(),
                id: "toolu_01QkmBQYKUd6tKi8EyQ63MUn".to_string(),
                name: "earnings_report_sentiment".to_string(),
                input: SentimentResponse { confidence: 7 },
            }],
            stop_reason: "tool_use".to_string(),
            stop_sequence: None,
            usage: ClaudeResponseUsage {
                input_tokens: 23641,
                output_tokens: 33,
            },
        };

        assert_eq!(sentiment_data, expected);
        assert_eq!(
            sentiment_data.content[0].input,
            SentimentResponse { confidence: 7 }
        );
    }
}
