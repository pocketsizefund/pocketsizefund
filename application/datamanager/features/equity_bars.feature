Feature: Equity Bars Data Management
    As a data manager application
    I want to fetch, store, retrieve, and delete equity bars data
    So that I can manage market data efficiently

    Background:
        Given the datamanager API is running
        And I have environment variables set for authentication

    Scenario: Fetch and store equity bars data
        When I send a POST request to "/equity-bars" for date "2023-05-01"
        Then the response status code should be 200
        And the response should contain a JSON with "date" and "count" fields
        And a parquet file should be created for "2023-05-01"

    Scenario: Retrieve equity bars data for a date range
        Given I have equity bars data for dates:
            | date       |
            | 2023-05-01 |
            | 2023-05-02 |
            | 2023-05-03 |
        When I send a GET request to "/equity-bars" with date range "2023-05-01" to "2023-05-02"
        Then the response status code should be 200
        And the response should contain equity bars data for the date range
        And the response should include a metadata section

    Scenario: Delete equity bars data for a specific date
        Given I have equity bars data for date "2023-05-01"
        When I send a DELETE request to "/equity-bars" for date "2023-05-01"
        Then the response status code should be 200
        And the equity bars data for "2023-05-01" should be deleted
        And the response should confirm successful deletion