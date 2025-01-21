import pandas as pd
import requests
import time

def fetch_articles_from_journal(api_key, journal_name):
    url = "https://api.semanticscholar.org/graph/v1/paper/search/bulk"
    token = None
    all_articles = []

    while True:
      params = {
        "venue": journal_name,
        "publicationTypes": "Review,JournalArticle",
        "year": "2005-2024",
        "fields": "title,abstract,year",
        "limit": 1000,
        "apiKey": api_key
      }
      if token:
              params['token'] = token


      response = requests.get(url, params=params)

      while response.status_code == 429:
        print("Waiting 1 second for access to the API...")
        time.sleep(1)
        response = requests.get(url, params=params)
      if response.status_code != 200:
        raise Exception(f"Error fetching data from Semantic Scholar: {response.status_code}")


      data = response.json()

      if 'data' not in data:
          return pd.DataFrame()  # Return an empty DataFrame if no data is found

    # Process each article and add to the list
      articles = [{
          'Title': article.get('title', ''),
          'Abstract': article.get('abstract', ''),
          'Year': article.get('year', '')
      } for article in data.get('data', [])]
      all_articles.extend(articles)

      token = data.get('token')  # Update the token from the response
      if not token:
        break  # Exit the loop if no next token is provided

    return pd.DataFrame(all_articles)
