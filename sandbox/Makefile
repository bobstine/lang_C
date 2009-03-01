
model.dat: blocks.csv
	rm -f model.dat temp.dat
	./csv_parser --input-file blocks.csv --output-file temp.dat
	~/bin/insert_column -f temp.dat  -o model.dat -y1 -x3 -r0.4
	rm -f temp.dat

test: model.dat
	rm -f test
	./build_model --input-file model.dat --output-path log/ -r 800 -v 1

