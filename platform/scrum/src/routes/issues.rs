use actix_web::post;
use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use octocrab::{models::IssueState, params};
use serde_json::json;

#[derive(Clone, Debug)]
pub struct User(String);

#[derive(Clone, Debug)]
pub struct Label {
    pub id: String,
    pub name: String,
    pub description: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Issue {
    pub id: String,
    pub number: u64,
    pub title: String,
    pub body: Option<String>,
    pub state: IssueState,
    pub locked: bool,
    pub user: User,
    pub assignees: Vec<User>,
    pub labels: Vec<Label>,
    pub comments: u32,
    pub milestone: Option<String>,
    pub pull_request: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub closed_at: Option<DateTime<Utc>>,
    pub closed_by: Option<User>,
}

impl Issue {
    pub fn new(issue: octocrab::models::issues::Issue) -> Self {
        let milestone: Option<String> = match issue.milestone {
            Some(ms) => Some(ms.url.to_string()),
            None => None,
        };

        let pull_request: Option<String> = match issue.pull_request {
            Some(pr) => Some(pr.url.to_string()),
            None => None,
        };

        let closed_by: Option<User> = match issue.closed_by {
            Some(user) => Some(User(user.login)),
            None => None,
        };

        Self {
            id: issue.url.to_string(),
            number: issue.number,
            title: issue.title,
            body: issue.body,
            state: issue.state,
            locked: issue.locked,
            user: User(issue.user.login),
            assignees: issue
                .assignees
                .into_iter()
                .map(|assignee| User(assignee.login))
                .collect(),
            labels: issue
                .labels
                .into_iter()
                .map(|label| Label {
                    id: label.url.to_string(),
                    name: label.name,
                    description: label.description,
                })
                .collect(),
            comments: issue.comments,
            milestone,
            pull_request,
            created_at: issue.created_at,
            updated_at: issue.updated_at,
            closed_at: issue.closed_at,
            closed_by,
        }
    }
}

#[post("/issues")]
pub async fn handler(_event: Event) -> Event {
    let issues = match list_all_issues().await {
        Ok(issues) => issues,
        Err(e) => {
            anyhow!("failed to parse issues: {}", e);
            return EventBuilderV10::new()
                .id(uuid::Uuid::new_v4().to_string())
                .ty("gh.issues.fetch.error")
                .source("platform.scrum")
                .data(
                    "application/json",
                    json!({
                        "status": "error"
                    }),
                )
                .build()
                .unwrap();
        }
    };
    tracing::info!("fetched and parsed {} issues", issues.len());

    // TODO: fan out and send one event per issue?
    // issues.into_par_iter().map(|issue| {
    //
    // })

    EventBuilderV10::new()
        .id(uuid::Uuid::new_v4().to_string())
        .ty("gh.issues.fetch.success")
        .source("platform.scrum")
        .data(
            "application/json",
            json!({
                "status": "success"
            }),
        )
        .build()
        .unwrap()
}

pub async fn list_all_issues() -> Result<Vec<Issue>> {
    let octo = octocrab::instance();

    let mut issues: Vec<Issue> = Vec::new();

    let mut page = octo
        .issues("pocketsizefund", "pocketsizefund")
        .list()
        .state(params::State::All)
        .per_page(100)
        .send()
        .await?;

    loop {
        for issue in &page.take_items() {
            issues.push(Issue::new(issue.clone()))
        }
        page = match octo
            .get_page::<octocrab::models::issues::Issue>(&page.next)
            .await?
        {
            Some(next_page) => next_page,
            None => break,
        }
    }

    Ok(issues)
}
