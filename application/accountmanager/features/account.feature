Feature: Account Endpoint
    As a client
    I want to retrieve my account details
    So that I can manage my portfolio

    Background:
        Given the accountmanager API is running

    Scenario: Account endpoint responds successfully
        When I send a GET request to "/account"
        Then the response status code should be 200
