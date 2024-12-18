import polars as pl

# Example DataFrame
df = pl.DataFrame({"A": [10, 20, 30], "B": [40, 50, 60]})

# Calculate the minimum for each column
min_values = df.select([pl.col(col).min().alias(col) for col in df.columns]).to_dict()

# Subtract the minimum value of each column from the respective column
result = df.with_columns([pl.col(col) - min_values[col] for col in df.columns])

print(result)
