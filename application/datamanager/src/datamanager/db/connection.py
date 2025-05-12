import duckdb


class DatabaseConnection:
    @staticmethod
    def create_connection(gcp_key_id: str = "", gcp_secret: str = ""):
        connection = duckdb.connect()

        if gcp_key_id and gcp_secret:
            connection.execute("INSTALL httpfs;")
            connection.execute("LOAD httpfs;")

            connection.execute(f"""CREATE SECRET (
                TYPE gcs,
                KEY_ID '{gcp_key_id}',
                SECRET '{gcp_secret}'
            );""")

        return connection
