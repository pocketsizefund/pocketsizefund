Feature: Health Check Endpoint
    As a client
    I want to check the health of the datamanager API
    So that I can ensure the service is running

    Background:
        Given the datamanager API is running

    Scenario: Health endpoint responds successfully
        When I send a GET request to "/health"
        Then the response status code should be 200