cd data
python2 ../lib/materialize_folds_nodiscr.py
rm -r output
cd ..
python lib/materialize_folds_KDD.py
