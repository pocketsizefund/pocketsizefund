Feature: Equity Bars Data Management
    As a data manager application
    I want to fetch, store, retrieve, and delete equity bars data
    So that I can manage market data efficiently

    Background:
        Given the datamanager API is running

    Scenario Outline: Manage bucket data for <start_date> to <end_date>
        Given I have date ranges:
            | start_date | end_date |
            | <start_date> | <end_date> |
        When I send a POST request to "/equity-bars" for date range
        Then the response status code should be 200
        When I send a GET request to "/equity-bars" for date range
        Then the response status code should be 200
        When I send a DELETE request to "/equity-bars" for date "<start_date>"
        Then the response status code should be 204

        Examples: dates
            | start_date | end_date   |
            | 2025-05-20 | 2025-05-20 |

    Scenario Outline: Skip weekends
