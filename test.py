import urllib.parse
import hashlib
import hmac
import base64
import time
import os
import requests

# Read Kraken API key and secret stored in environment variables
api_url = "https://api.kraken.com"
api_key = "monWCG/XxqticGmfAScjTFnT2sgmqdV4EOxR6FYkoj4RNL2qLDjaJSRD"
api_sec = "yOyYtiGXfaSLBriXyGORo2RLHT3/bA2qcrBCMFqNDJEY+lrIZrIph2a0u90/OUL5q/ZVC+7WO3n5+JZPpETPDw=="

def get_kraken_signature(urlpath, data, secret):

    postdata = urllib.parse.urlencode(data)
    encoded = (str(data['nonce']) + postdata).encode()
    print("utf8EncodedDataStr: ", str(data['nonce']) + postdata)
    message = urlpath.encode() + hashlib.sha256(encoded).digest()

    print("urlenc: ", urlpath.encode())
    print("encoded: ", encoded)
    print("encoded sha: ", hashlib.sha256(encoded).digest())
    print("message: ", message)
    print("private key: ", base64.b64decode(secret))

    mac = hmac.new(base64.b64decode(secret), message, hashlib.sha512)
    # mac = hmac.new(base64.b64decode(secret), "/0/private/Balance".encode(), hashlib.sha512)
    sigdigest = base64.b64encode(mac.digest())
    print("sigdigest: ", sigdigest)
    return sigdigest.decode()

# Attaches auth headers and returns results of a POST request
def kraken_request(uri_path, data, api_key, api_sec):
    headers = {}
    headers['API-Key'] = api_key
    # get_kraken_signature() as defined in the 'Authentication' section
    headers['API-Sign'] = get_kraken_signature(uri_path, data, api_sec)
    print("header: ")
    print(headers)
    print("data: ")
    print(data)
    req = requests.post((api_url + uri_path), headers=headers, data=data)
    return req

# Construct the request and print the result
resp = kraken_request('/0/private/TradeBalance', {
    "nonce": str(int(1000*time.time())),
    "asset": "ZUSD"
    # "cookie": "",
    # "accept": "application/json"
}, api_key, api_sec)

print(resp.json())
