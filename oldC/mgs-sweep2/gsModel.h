// $Id: gsModel.h,v 1.14 2001/11/28 21:28:33 bob Exp $

/*
  22 Jul 01 ... Start to add covariance features.
  30 Jun 01 ... Alter structure of gsModel, adding XtX, XtY names
  21 Jun 01 ... Created for new version of sweeper.
*/

#ifndef _GSMODEL_
#define _GSMODEL_  

#include <vector>

#include "dataArray.h"
#include "operator.h"
#include "queue.h"
#include "vector.h"
#include "raggedArray.h"

////////////////////////////////////////////////////////////////////////////////

typedef struct aGSModel
{
  char path[32];           // where to look for files
  int q;                   // number of predictors, *including* the constant
  int nPad;                // room for expansion; exits when no more room
  double sumWts;           // sum of the observation weights
  double tss, rss;         // total SS and current residual SS
  DataArray database;      // input data arranged as w, Y , X1..Xp *not* altered
  DataArray wYQ, X;        // (w, Y~, Q1..Qq)  and (1, X1, X2,..Xq) in raw form
  RaggedArray R;           // triangular array R of X = QR; const as 0, so q+1 cols
  Vector QtY;
  Matrix QVQ;              // padded with zeros
  double *temp, *temp2;    // temp space, of max size
} aGSModel;

typedef struct aGSModel *GSModel;

////////////////////////////////////////////////////////////////////////////////

// --- New, delete, print ---

GSModel
NewGSModel (char *inputDataFile, char *modelPath, int nPad);
/*
  Format of the input data file is
      weight, y, X1..Xp (predictors)
  GS model will hence use the prefix (which can denote a subdir) to denote its files.
*/
      
void
DeleteGSModel (GSModel gs);

void
PrintGSModel (GSModel gs, char *msg);


// --- File operations ---

int
WriteGSModelToFile (GSModel gs);
/*
  GS Models use their own naming conventions, using the prefix
  of the model.
*/

GSModel
ReadGSModelFromFile(char *fileName);


// --- Basic properties ---

long
NumberOfObservationsGSModel (GSModel gs);

long
NumberOfPossiblePredictorsGSModel (GSModel gs);

long
NumberOfPredictorsGSModel (GSModel gs);

double
ResidualDFGSModel (GSModel gs);

double
ResidualVarianceGSModel (GSModel gs);

// --- Modeling operations ---

double
ChangeInRSSGSModel (GSModel gs, Operator f);
/*
  Evaluates the change in RSS achieved if the indicated function applied
  to the data array is added to the current model.
*/

double
PartialFGSModel (GSModel gs, Operator f);
/*
  Evaluates the conservative partail F if the indicated function applied
  to the data array is added to the current model.
*/

Operator
FindBestPredictorForGSModel (GSModel gs, vector<Operator> &ops, FILE *reply);
/*
  Finds the best predictor among the operators given in the input ops
  vector.  Returns the operator that performed best, with its score
  field filled with the obtained out-of-model fStat.
*/

Operator
EvaluatePredictorsForGSModel (GSModel gs, OperatorQueue opQueue, FILE *reply);
/*
  Considers the operators in the queue, returning first operator
  that meets the goal.
    If no operator meets the goal, then returns nil.  If operator meets
  goal, its position slot is filled by line occupied in the file and its
  score is the obtained conservative f-stat.
    Writes a reply to the *open* destination file as each is evaluated,
  in the format of the following Lisp nested list
           ((accept or reject) operator threshold score)
*/


long
CommitPredictorToGSModel (GSModel gsModel, Operator f);
/*
  This procedure adds the indicated predictor, first forming it as
  a predictor in the data array, then in centered form which is swept in the
  model array.  Returns the size of the new model.

  This is *modified* GS algorithm, so requires q passes over the
  model array to compute.
*/



#endif
