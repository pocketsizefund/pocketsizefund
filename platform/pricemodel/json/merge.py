import os
import json
import csv


def consolidate_json_to_csv(directory, output_file):
    # Define the CSV column names
    csv_columns = ["ticker", "c", "h", "l", "n", "o", "t", "v", "vw"]

    # List to hold all consolidated rows
    consolidated_data = []

    # Iterate through files in the specified directory
    for filename in os.listdir(directory):
        if filename.endswith(".json"):
            file_path = os.path.join(directory, filename)
            # Extract the ticker name from the file name (e.g., "MMM" from "MMM.json")
            ticker = os.path.splitext(filename)[0]
            try:
                # Open and load the JSON file
                with open(file_path, "r") as file:
                    data = json.load(file)
                    # Extract the "bars" array
                    bars = data.get("bars", [])
                    # Add the ticker field to each entry
                    for bar in bars:
                        bar["ticker"] = ticker
                    consolidated_data.extend(bars)
            except (json.JSONDecodeError, KeyError) as e:
                print(f"Skipping {filename}: {e}")

    # Write consolidated data to a CSV file
    with open(output_file, "w", newline="") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=csv_columns)
        writer.writeheader()  # Write the header row
        writer.writerows(consolidated_data)  # Write data rows

    print(f"Consolidated data written to {output_file}")


# Run the function in the current directory
if __name__ == "__main__":
    consolidate_json_to_csv(directory=".", output_file="consolidated_data.csv")
