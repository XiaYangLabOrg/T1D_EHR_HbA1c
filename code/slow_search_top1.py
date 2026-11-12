import csv
import sys
import json
from fuzzywuzzy import fuzz

# Function to load JSON data from a file
def load_json(fn):
    with open(fn, 'r') as file:
        return json.load(file)

# Function to calculate the highest fuzzy match score for the target against a list of names
def calculate_score(target, names):
    best_score = 0
    for name in names:
        score = fuzz.token_set_ratio(target, name)
        if score > best_score:
            best_score = score
    return best_score

print('YOU ARE RUNNING THE ADAPTED VERSION OF FUZZY SEARCH')
if len(sys.argv) != 3:
    print("Usage: python search.py json_file query_names_file")
    sys.exit(1)

fn_json = sys.argv[1]
fn_query_names = sys.argv[2]

# Read query names from the file
queryNames = []
with open(fn_query_names, 'r', encoding='utf-8') as query_names_file:
    for line in query_names_file:
        stripped_line = line.strip()  # Strip newline characters from the end of each line
        queryNames.append(stripped_line)

# Initialize the targetWords list for storing search results
targetWords = [[word, "", "", "", "", "", "", 0] for word in queryNames]

# Load JSON data
json_data = load_json(fn_json)
numRecords = len(json_data)
print(f"there are {numRecords} records in the JSON file")

# Iterate over each record in the JSON file
for i, record in enumerate(json_data):
    recordName = record['name']
    
    # Iterate over each query in targetWords
    for targetTrio in targetWords:
        targetWord = targetTrio[0]
        score = targetTrio[7]

        # Search in both name and synonyms fields
        synonyms = record.get('synonyms', [])
        all_names = synonyms + [recordName]  # Combine the record name with its synonyms
        newScore = calculate_score(targetWord, all_names)

        # Update targetTrio if a better match is found
        if newScore > score:
            targetTrio[1] = recordName
            targetTrio[2] = record.get('kegg_id', 'None')
            targetTrio[3] = record.get('pubchem_compound_id', 'None')
            targetTrio[4] = record.get('accession', 'None')
            targetTrio[5] = record.get('secondary_accession', 'None')
            targetTrio[6] = synonyms
            targetTrio[7] = newScore

    # Print progress
    print(f"processed record #{i + 1}")

print("Done searching, see final results in result.csv")

# Write results to CSV files
csv_file_path = "result.csv"
csv_detail_file_path = "details.csv"

fieldNames = ['QueryName', 'BestMatched', 'KeggID', 'FuzzyScore']
detailNames = ['QueryName', 'BestMatched', 'KeggID', 'pubchem_compound_id', 'accession', 'secondary_accession', 'synonyms', 'FuzzyScore']

with open(csv_file_path, 'w', newline='') as csvfile:
    with open(csv_detail_file_path, 'w', newline='') as detailFile:
        csv_writer = csv.DictWriter(csvfile, fieldnames=fieldNames)
        csv_writer.writeheader()

        detail_writer = csv.DictWriter(detailFile, fieldnames=detailNames)
        detail_writer.writeheader()

        # Write each result to the CSV files
        for targetTrio in targetWords:
            completeRow = {
                'QueryName': targetTrio[0], 
                'BestMatched': targetTrio[1], 
                'KeggID': targetTrio[2], 
                'FuzzyScore': targetTrio[7]
            }
            csv_writer.writerow(completeRow)

            completeDetailRow = {
                'QueryName': targetTrio[0], 
                'BestMatched': targetTrio[1], 
                'KeggID': targetTrio[2], 
                'pubchem_compound_id': targetTrio[3], 
                'accession': targetTrio[4], 
                'secondary_accession': targetTrio[5], 
                'synonyms': ', '.join(targetTrio[6]) if isinstance(targetTrio[6], list) else targetTrio[6], 
                'FuzzyScore': targetTrio[7]
            }
            detail_writer.writerow(completeDetailRow)

print("CSV file has been created successfully.")
