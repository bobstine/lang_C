These instructions show how to build the executable programs.


In order to build these, you will need to have installed the following
application libraries:

	boost	(www.boost.org,  version 1.37+)
	gsl     (gnu scientific library, www.gnu.org/software/gsl/)

These are referenced in the Makefile that builds the executables and
must be in the search path used by the compiler (I use gcc).


Once these libraries are installed, use the supplied Makefile:

	make

which will build two executables: build_model and csv_parser.

Instructions in the following section describe how to use these
executables to prepare the data (converting from csv to a streaming,
internal format) and then build a model.



###################################################################




The following instructions show how to use the executable components
of the model selection code to select and estimate a regression-type
model. The model can also be used to fit the estimation data as well
as predict other cases not used in estimation.



*** Step 1.  Arrange input data variables as columns in a CSV file.

The input data format should be a standard Excel-type CSV file. The
first row of the file must hold names that identify the data in each
column, and the data file must not have empty columns.  The names of
the columns must begin with a character and may include alpha-numeric
characters as well as embedded spaces. Special characters should be
avoided and may not be parsed correctly. Parentheses, for example, are
not allowed in the names of the variables; spaces are okay.

In the usual application, some of the rows are excluded from the model
building, but are to be predicted. In this situation, the first column
of the CSV file must identify the cases (rows) that should be used in
estimation. If the first column of the input data file consists of the
strings 'in' and 'out' (without the "'"), then it will be interpreted
as a subset selector. Any row labeled 'out' in this column will not be
used in the fitting procedure, but will receive a predicted value
after the modeling is completed.  If all of the cases are to be used
in the fitting, this column may be excluded.

The next column of the CSV file (after the optional inclusion/
exclustion indicator) holds the variable that is to be the response in
the regression modeling.  The remaining columns define predictors for
the regression modeling. These may include categorical variables
(columns with non-numerical data, such as 'male'/'female', or group
identifiers such as 'g1', 'g2', ..., 'gk'). Missing data is denoted by
an empty cell in the table.  No missing values are allowed in the
response column or the inclusion/exclusion column. (If the model is to
be used to predict values for the response that are not known, simply
fill this column with 0 or some other value. Do not leave values of
the response empty or the file will not be corrected parsed.)

The input CSV file will resemble the following layout (which has 3
explanatory variables and an inclusion indicator in the first column)

	Use, Response, X1, X2, X3
	in , 23, 33,44,yes
	in,12,32,3,no
	in,   3, 32, ,yes
	out, 22,3, no
	...

In this example, X3 (the last column) is a categorical variable and
the the column X2 is missing a value for the third case. The 4th case
on the line that begins with 'out' would receive a fitted value, but
not be used in the model selection or estimation.



*** Step 2.  Run the CSV parser to convert the csv file into raw data

The executable csv_parser converts an input CSV file into the raw
format used by the model selection routine. For example, suppose that
the data for modeling is in the file mydata.csv.  The following
command converts the csv file to the format expected by the modeling
routines. The output data file (in this example) is named input.data

	csv_parser --input-file mydata.csv --output-file model.dat

If the argument file names are not supplied, data is read from
standard input and written to standard output.



*** Step 3.  Run the model building and fitting procedure

Pass in the name of the data file constructed in Step 2 and indicate
the number of rounds of selection.  Supply a path (including the
terminating separator) for the output files.  The number of rounds
controls the number of variables that the method will try.  The size
limit should depend on the size of the data set. For a data set with k
explanatory variables, the number of rounds should be on the order of
several multiples of k, up to about half of k^2. 

In the following example, the model-building program reads data from
the input file model.dat and writes several output files into the path
log/.  The procedure runs for 800 rounds.

	build_model --input-file model.dat --output-path log/ -r 800

Three useful output files are

  output_path/model.txt		Summary of fitted model, coefs, etc
  output_path/model.html	Summary of fitted model, coefs, etc (not yet there)
  output_path/model_data.csv	Data from fitted model

The model data file is only written if the procedure finds any
meaningful explanatory variables. If none are found, these files are
empty.  The first line of the model data file gives names of the
columns in the rest of the file.  The second line are values of these
variables for the first case, and so forth.  Additional files
describes the activity during rounds (alpha.dat) and progress messages
(progress.log).

The leading 4 columns of the model data file are	

	Binary indicator of included cases (1 included, 0 were not)
	Observed fitting error (y - y^)
	Fitted value (prediction if the case is not included)
	Response variable

These are followed by columns that hold the values of the explanatory
variables as constructed and used in the modeling. Any explanatory
variables used in the regression are here, including copies of any
input variables as well as variables such as interactions formed as
part of the modeling.



*** Test example

The subdirectory test contains a sample data file that you can use to
try out the algorithm.  The sample data file includes an optional
selector that picks out the interesting cases.  To build the test,
enter the command

	make test

while in the directory in which you built the code.  You can explore
the commands used to build the test in the file Makefile and see the
results of running the test in the files found in test/log/.

The model constructed has the form

Linear Model: (n=202)     SS 3.21848e+08 --> 8.82478e+07 R2 = 0.725809
             Model has 9 explanatory variables.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                         Summary of Estimates
        Name                   Estimate            SE          t         p
                Intercept           175.565 
                       x1           791.072   2.22e+03       0.35      0.722
                       x2          -1023.38   1.87e+03      -0.54      0.583
                       x3             19.02       3.49       5.44          0
                       x6          -3.58046       24.6      -0.14      0.884
                       x7           43.0713       9.66       4.45   8.22e-06
                      x10          -13.8719       2.98      -4.65   3.27e-06
                      x13           629.175        208       3.02    0.00249
                      x15           899.756        141       6.37          0
                 x10*x13            12.1808       3.04          4   6.31e-05
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

You can use the data in the first model_data.csv to explore the model.




*** Comments

At this moment, the model selects variables 'cautiously', only
including variables that are demonstrably useful for out-of-sample
prediction. This conservative approach may lead to a smaller, less
predictive model than expected.




