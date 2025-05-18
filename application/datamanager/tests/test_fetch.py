import json
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

import httpx
import pytest

from application.datamanager.src.datamanager.main import application


class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        body = json.dumps({"results": []}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(body)


@pytest.mark.integration
@pytest.mark.asyncio
async def test_fetch_equity_bars(monkeypatch, tmp_path):
    server = HTTPServer(("localhost", 0), Handler)
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    url = f"http://{server.server_address[0]}:{server.server_address[1]}"
    monkeypatch.setenv("POLYGON_API_KEY", "dummy")
    monkeypatch.setenv("POLYGON_BASE_URL", url)
    monkeypatch.chdir(tmp_path)
    async with application.router.lifespan_context(application):
        async with httpx.AsyncClient(app=application, base_url="http://test") as client:
            response = await client.post("/fetch-equity-bars")
    server.shutdown()
    thread.join()
    assert response.status_code == 200
    assert "date" in response.json()
