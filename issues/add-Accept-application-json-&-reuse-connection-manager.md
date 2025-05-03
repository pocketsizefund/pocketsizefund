# Minor: add Accept: application/json & reuse connection manager

Wreq defaults work, but being explicit helps when upstream changes content negotiation. Also, defaults allocates a new manager per request; consider Network.Wreq.Session or http-client manager reuse to cut latency.
