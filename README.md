# Spell-Checker - BK trees and Levensthein Distance
CS51 Final Project
By Catherine Tuntiserirat, Mick (Chaowat) Kajornrattana, Erika Puente, Billie Wei


	
Running the code
----------------

To complie the file: 
```
make 
```
or
``
make
````

````
Options:
  -h, --help            show this help message and exit
  -t FILE, --train=FILE
                        train using data from FILE
  -g, --generate        generate new haikus using data aquired from training
  -f, --fresh           overwrite old training databases (old information WILL
                        be lost)
  -m, --markov          generate a haiku using a markov chain process
  --vocabulary          only use training data for training vocabulary
```

Example usage:
```
python3 main.py -t data.txt # train using data.txt
python3 main.py -g # generate poems using evolutionary approach
python3 main.py -m # generate poems using markov approach
```

Organization
------------

Here is a description of what each file will do:
- data.txt: cleaned database of haikus (Basho, ...)
- dictionary.py: module for interacting with several external libraries
- training.py: training input data
- evaluate.py: evaluating the goodness of poems
- evolve_population.py: the poem evolution code
- evo_object.py: class Evo_object
- monogram.py: class Monogram
- bigram.py: class Bigram
- line_haiku.py: class Line_Haiku
- line_type.py: class Line_type
- population_haiku.py: class Population_haiku
- main.py: using the program