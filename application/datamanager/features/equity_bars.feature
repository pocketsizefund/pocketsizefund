Feature: Equity Bars Data Management
    As a data manager application
    I want to fetch, store, retrieve, and delete equity bars data
    So that I can manage market data efficiently

    Background:
        Given the datamanager API is running

    Scenario: Fetch and store equity bars data
        Given I have date <start_date> and <end_date>
        When I send a POST request to "/equity-bars" for date range
        Then the response status code should be 200
        # Examples: Dates
        #   | start_date | end_date   |
        #   | 2023/05/01 | 2023/05/01 |
        # When I send a GET request to "/equity-bars" for date range "2023/05/01" to "2023/05/01"
        # Then the response status code should be 200
        # Then I can get that data back from the API for date range "2023/05/01" to "2023/05/01"


    # Scenario: Retrieve equity bars data for a date range
    #     Given I have equity bars data for dates:
    #         | date       |
    #         | 2023-05-01 |
    #         | 2023-05-02 |
    #         | 2023-05-03 |
    #     When I send a GET request to "/equity-bars" with date range "2023-05-01" to "2023-05-02"
    #     Then the response status code should be 200
    #     And the response should contain equity bars data for the date range
    #     And the response should include a metadata section

    # Scenario: Delete equity bars data for a specific date
    #     Given I have equity bars data for date "2023-05-01"
    #     When I send a DELETE request to "/equity-bars" for date "2023-05-01"
    #     Then the response status code should be 200
    #     And the equity bars data for "2023-05-01" should be deleted
    #     And the response should confirm successful deletion

    Scenario Outline: Manage bucket data for <start_date> to <end_date>
        Given I have date ranges:
            | start_date | end_date |
            | <start_date> | <end_date> |
        And the bucket object for "<start_date>" does not exist
        When I send a POST request to "/equity-bars" for date range
        Then the response status code should be 200
        And the bucket object for "<start_date>" should exist
        When I read the bucket object for "<start_date>" with DuckDB
        Then the result should have rows
        When I send a DELETE request to "/equity-bars" for date "<start_date>"
        Then the response status code should be 204
        And the bucket object for "<start_date>" should not exist

        Examples: dates
            | start_date | end_date |
            | 2025/05/01 | 2025/05/02 |
            | 2025/05/03 | 2025/05/04 |
