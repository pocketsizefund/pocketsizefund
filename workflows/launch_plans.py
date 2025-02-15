from flytekit import LaunchPlan, CronSchedule, WorkflowExecutionPhase, Email, PagerDuty, Slack

from workflows.flows.daily_bars import load_yesterdays_bars


daily_bars_dump = LaunchPlan.get_or_create(
    name="daily_bars_dump",
    workflow=load_yesterdays_bars,
    schedule=CronSchedule(schedule="@daily"),
    notifications=[
        Email(
            phases=[
                WorkflowExecutionPhase.FAILED,
                WorkflowExecutionPhase.TIMED_OUT,
                WorkflowExecutionPhase.ABORTED,
            ],
            recipients_email=["chris.william.addy@gmail.com"],
        ),
        Email(
            phases=[WorkflowExecutionPhase.SUCCEEDED],
            recipients_email=["chris.william.addy@gmail.com", "john.forstmeier@gmail.com"],
        ),
    ],
)
