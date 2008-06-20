// $Id: sweep_matrix.h,v 1.7 2003/08/12 15:30:39 bob Exp $

/*
  22 May 03 ... Ported from old sweeper code for use in C++ sequential models.
*/

#ifndef _SWEEP_MATRIX_H_
#define _SWEEP_MATRIX_H_

#include <ostream>
#include <vector>
#include <functional>

#define SWEEP_NEAR_ZERO			1.0E-20

//////////////////////////////////////////////////////////////////////////////////

typedef std::vector<double> Vector;

class SweepMatrix;

namespace {

  class IndexedVector
    {
    private:
      int    mIndex;
      Vector mVector;
    public:
      IndexedVector()
	: mIndex(-1), mVector(0) {};
      IndexedVector(int index, Vector const& vec)
	: mIndex(index), mVector(vec) {}
      IndexedVector(IndexedVector const& iv)  // copy constructor
	: mIndex(iv.mIndex), mVector(iv.mVector) {};

      int     size () const            { return mVector.size(); }
      double  operator[](int k) const  { return mVector[k]; }
      double  diagonal() const         { return mVector[mIndex]; }
      
      void    replace_contents_from (IndexedVector const& iv);
      void    convert_to_pivot_form ();
      void    sweep_out (IndexedVector const& kv);
      
      void    write_to(std::ostream& os, int maxPrint = 5) const;
      
      friend class SweepMatrix;
    };
}

inline
std::ostream&
operator<< (std::ostream& os, IndexedVector const& iv)
{
  iv.write_to(os); return os;
}

class SweepMatrix {
  int                 mNSkip;                     // Number of skipped cols
  std::vector <bool>  mSkipVector;                // Columns to skip in further calculation
  Vector              mRawSS;                     // SS about mean in columns
  Vector              mResidualSS;                // Updated residual SS about mean
  Vector              mAvg;                       // Centers of the columns
  int                 mQBase;                     // Used in adaptive thresholding
  std::vector<IndexedVector> mIncrSweepMat;       // incremental sweep matrix
  std::vector<IndexedVector> mFullSweepMat;       // fully swept matrix
  
 public:
  SweepMatrix(Vector const& cp, Vector const& ss, Vector const& avg)
    : mNSkip(0), mSkipVector(ss.size()), mRawSS(ss), mResidualSS(ss), mAvg(avg),
      mQBase(0) { initialize(cp); }
  
  ~SweepMatrix() {  }

  double
    add_predictor(int j, Vector const& covs, bool forced=false);            // returns new RSS

  /*
    Forced option implies that this predictor was chosen a priori
    rather than found through the use of a variable selection search
    using this data.  If forced, the qBase slot is incremented.
  */
  
  //  ACCESSORS  ACCESSORS  ACCESSORS  ACCESSORS  ACCESSORS  ACCESSORS  ACCESSORS

  double              RSS       () const;
  double              r_squared () const;

  Vector const&       residual_ss_vector() const { return mResidualSS; }
  Vector const&       residual_cp_vector() const { return mIncrSweepMat[0].mVector; }
  
  int                 number_of_variables  () const;
  int                 number_of_predictors () const;
  bool                skip(int j)             const;
  std::vector<int>    predictors           () const;
  Vector              slopes               () const;
  double              intercept            () const;
  
  inline double       partial_beta(int row, int col) const
    { return mFullSweepMat[row].mVector[col]; }

  void    write_to (std::ostream& os, int maxPrint = 5) const;

  //  PRIVATE  PRIVATE  PRIVATE  PRIVATE  PRIVATE  PRIVATE  PRIVATE  PRIVATE
  
 private:
  void  initialize(Vector const& cp);

  std::vector<int>
    pick_predictors(int n, std::vector< std::pair<int,double> > *dFit);
  
  void  mark_constant_columns ();
  void	sweep_last_row();
  void	adjust_rss    (IndexedVector const& CPj);
}; 

inline
std::ostream&
operator<<(std::ostream& os, SweepMatrix const& sm)
{ sm.write_to(os); return(os); }



    
#endif
