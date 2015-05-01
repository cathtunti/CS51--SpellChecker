import csv
import re

def main():

  # open text files
  frequency_dict = open("frequency_dict.txt", "r")
  unix_dict = open("dictionary.txt", "r")

  # store text file in arrays
  frequency_arr = frequency_dict.readlines()
  unix_arr = unix_dict.readlines()

  # close files
  frequency_dict.close()
  unix_dict.close()

  # create a hash table to search frequency of a word
  hash_tab = {} 
  for line in frequency_arr:
    word = re.sub(r"[^A-Za-z]+", '', line)
    freq = re.sub("[^0-9]", "", line)
    hash_tab[word] = freq

  # calculate total frequency and keep on words that are in both dictionaries
  total_frequency = 0.0
  both_dict = []
  for line in unix_arr: 
    word = line[:-1]
    if word in hash_tab: 
      both_dict.append([word,hash_tab[word]])
      total_frequency = total_frequency + int(hash_tab[word])

  # write new dict file
  output = open("dict.txt", "w")
  for elt in both_dict:
    freq = float(elt[1]) / total_frequency
    output.write("%s\n" % elt[0])
    output.write("%s\n" % str(freq))

  output.close()

if __name__ == '__main__':
  main()