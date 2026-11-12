import csv
import sys
import json
from fuzzywuzzy import fuzz

# Function to load JSON data from a file
def load_json(fn):
    with open(fn, 'r') as file:
        return json.load(file)

# Function to calculate fuzzy match scores for the target against a list of names
def calculate_scores(target, names):
    scores = []
    for name in names:
        score = fuzz.token_set_ratio(target, name)
        scores.append((name, score))
    return scores

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
        stripped_line = line.strip()
        queryNames.append(stripped_line)

# Initialize the targetWords list for storing all matches
targetWords = {word: [] for word in queryNames}

# Load JSON data
json_data = load_json(fn_json)
numRecords = len(json_data)
print(f"There are {numRecords} records in the JSON file")

# Iterate over each record in the JSON file
for i, record in enumerate(json_data):
    recordName = record['name']
    
    # Iterate over each query in targetWords
    for targetWord in targetWords.keys():
        # Search in both name and synonyms fields
        synonyms = record.get('synonyms', [])
        all_names = synonyms + [recordName]  # Combine the record name with its synonyms
        scores = calculate_scores(targetWord, all_names)

        # Add each match with its details to the list for the target word
        for name, score in scores:
            match_details = {
                'BestMatched': recordName,
                'MatchedName': name,
                'KeggID': record.get('kegg_id', 'None'),
                'pubchem_compound_id': record.get('pubchem_compound_id', 'None'),
                'accession': record.get('accession', 'None'),
                'secondary_accession': record.get('secondary_accession', 'None'),
                'synonyms': synonyms,
                'FuzzyScore': score
            }
            targetWords[targetWord].append(match_details)

    # Print progress
    print(f"Processed record #{i + 1}")

print("Done searching, see final results in result.csv")

# Write results to CSV files
csv_file_path = "./T2D_all/result.csv"
csv_detail_file_path = "./T2D_all/details.csv"

fieldNames = ['QueryName', 'MatchedName', 'BestMatched', 'KeggID', 'FuzzyScore']
detailNames = ['QueryName', 'MatchedName', 'BestMatched', 'KeggID', 'pubchem_compound_id', 'accession', 'secondary_accession', 'synonyms', 'FuzzyScore']

with open(csv_file_path, 'w', newline='') as csvfile:
    with open(csv_detail_file_path, 'w', newline='') as detailFile:
        csv_writer = csv.DictWriter(csvfile, fieldnames=fieldNames)
        csv_writer.writeheader()

        detail_writer = csv.DictWriter(detailFile, fieldnames=detailNames)
        detail_writer.writeheader()

        # Write each result to the CSV files
        for targetWord, matches in targetWords.items():
            for match in matches:
                completeRow = {
                    'QueryName': targetWord, 
                    'MatchedName': match['MatchedName'],
                    'BestMatched': match['BestMatched'], 
                    'KeggID': match['KeggID'], 
                    'FuzzyScore': match['FuzzyScore']
                }
                csv_writer.writerow(completeRow)

                completeDetailRow = {
                    'QueryName': targetWord,
                    'MatchedName': match['MatchedName'],
                    'BestMatched': match['BestMatched'],
                    'KeggID': match['KeggID'],
                    'pubchem_compound_id': match['pubchem_compound_id'],
                    'accession': match['accession'],
                    'secondary_accession': match['secondary_accession'],
                    'synonyms': ', '.join(match['synonyms']) if isinstance(match['synonyms'], list) else match['synonyms'],
                    'FuzzyScore': match['FuzzyScore']
                }
                detail_writer.writerow(completeDetailRow)

print("CSV file has been created successfully.")
