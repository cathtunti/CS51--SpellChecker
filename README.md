# Spell-Checker - BK trees and Levensthein Distance
CS51 Final Project
------------------
By Catherine Tuntiserirat, Mick (Chaowat) Kajornrattana, Erika Puente, Billie Wei


	
Running the code
----------------

To complie the file, go inside "code" folder and 
```
make 
```
or
```
make clean; make
````

To run, 
````
./project.native
```

Then at terminal screen, put in a word to prompt the program to return a list of suggested word (default number of return is set at 10)


Customizing

- To change the number of suggested word return, change

```
 let display_num = (int)
```

- To change the dictionary used when the program runs
```
let dict = BKTreeDynamic.load_dict "[destination of dictionary]"
```

- To change the tolerance level used in search, modify...
```

  let search (word: string) (tree: tree) : string list = 
    (* tolerance +1 for every 5 chars *)
    let tolerance = (String.length word) / 5 + 1 in
```

**WARNING**:
------------
Adjusting tolerance level affect the search time and the number of suggested word returns.

In general, the higher tolerance means more words are searched in the data structure and more words are considered to be "similar".

Per default setting, If your word has a length of 5, then its tolerance is 1. This means that the program will only tolerate 1 mistake and return only the words that have atmost 1 edit distance from input word. 

For example, if you intended "google"  but typed in "mgoogel" (tolerance is 1 but mistake is 2), then it might not find any matches. Meanwhile, if you typed in "googel", it can properly suggests the word. 


Organization
------------

Here is a description of what each file does:

- report.pdf: Final write up, including all links and explanations for the algorithms
- code/cleandict.py: use to turn a dictionary file into desired format with probability
- colde/project.ml: The program itself. Includes Distance modules, BK-Tree
- data/cleaned_dict.txt: big dictionary that we use to suggest words.
- data/test_dict: smaller dictionary used to test run time.
