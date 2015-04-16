#ifndef _FEATURES_H_
#define _FEATURES_H_

/* 
  Feature is a wrapper class that has a pointer to the actual feature.  This works nicely
  since messages that go to the feature get communicated to one thing, rather than many.

  These underlying features are named ranges, with optional information about the use of
  that data in a model or application. Features do *not* hold any data, so they are
  'lightweight'. For example, column features hold a reference to a column, but do not
  new/delete the space.

  Column feature

       These directly reference a column in the underlying data.
       These features are like 'names' that JMP puts in the heads
       of the columns in its data file.

  Interaction feature

       The cross product of two other features, which also may
       be interactions as well.  Automatically centers components.

  LinearCombination feature

       Linear combinations are used to represent, for example, the
       fit in a model or other simple linear transformations.

  Unary feature

       These use a lightweight function object to define the range.
       Since the function object is light, no big deal about copying
       the function.

  16 Aug 04 ... Splines as a new type of feature.
  22 Apr 04 ... Linear combination, model features
   5 Apr 04 ... Read/Write to support virtual constructor
   1 Apr 04 ... ABC version
  22 Mar 04 ... Cleaner version 
   9 Mar 04 ... Created to support abstraction of auction.
*/

#include "auction_base_types.h"

#include "featureABC.h"
#include "column.Template.h"
#include "range_stats.h"
#include "operator_traits.h"
#include "function_utils.h"
#include "simple_eigenword_dictionary.h"

#include <iostream>
#include <sstream>
#include <vector>
#include <list>

////  Envelope class

class Feature
{
 public:
  typedef SCALAR Scalar;
  
 private:
  FeatureABC *mFP;

 public:
  ~Feature() { if(--mFP->mRefCount <= 0) delete mFP; }
  
  // copy
  Feature(Feature const& f)    : mFP(f.mFP)     { ++f.mFP->mRefCount;  }

  //  empty has a null column
  Feature();

  //  column feature
  Feature(Column<Scalar> const &c);

  //  lag feature
  Feature(Feature const& f, size_t lag, size_t blockSize=1);
  
  //  interaction feature
  Feature(Feature const& f1, Feature const& f2);

  //  linear combination
  Feature(int n, std::string name, std::vector<Scalar> b, std::vector<Feature> const& fv);
  Feature(int n,  std::vector<Scalar> b, std::vector<Feature> const& fv);

  //  unary feature (copy the operator!)
  template<class Op>
    Feature(Op op, Feature const &x);

  //  binary feature
  template<class Op>
    Feature(Op op, Feature const &x1, Feature const& x2);

  //  output
  void write_to(std::ostream& os) const;

  bool operator==(Feature const& f)        const  { return mFP == f.mFP; }
  bool operator< (Feature const& f)        const  { return mFP->name() < f.mFP->name(); }
  
  Feature&    operator=(Feature const& f);
  FeatureABC* operator->()                 const  { return mFP; }  
};

inline
std::ostream&
operator<< (std::ostream& os, Feature const& feature) { feature.write_to(os); return os; }

////  Convenience definitions

typedef bool (*FeaturePredicate)(Feature const&) ;


////  Convenience functions for collections

typedef  std::vector<Feature>  FeatureVector;
typedef  std::list  <Feature>  FeatureList;

FeatureVector
features_with_name(std::string name, FeatureVector const& fv);

FeatureVector
features_with_attribute(std::string attribute, std::string value, FeatureVector const& fv);

FeatureVector
powers_of_column (Column<SCALAR> const& col, std::vector<int> const& powers);
  
Feature
make_indexed_feature (Feature const& f, IntegerColumn const& i);


void
write_feature_vector_to (std::ostream& os, FeatureVector const& fv);

inline
std::ostream&
operator<< (std::ostream& os, FeatureVector const& featureVec) { write_feature_vector_to(os, featureVec); return os; }



////  Column     Column     Column     Column     Column     Column     Column     Column     Column     Column     

class ColumnFeature : public FeatureABC
{
 private:
  Column<Scalar> mColumn;  // store by value ; columns are lightweight with ref-counted pointer

 public:
  virtual ~ColumnFeature() {};
  
 ColumnFeature(Column<Scalar> c) : FeatureABC(c->size(), c->attributes()), mColumn(c)  { }

  string        class_name()       const;
  string        name()             const;
  string        operator_name()    const;
  DependenceMap dependence_map()   const;
  int           degree()           const;
  Arguments     arguments()        const;
  Column<Scalar>column()           const;
  Iterator      begin()            const;
  Iterator      end()              const;
  Range         range()            const;
  bool          is_dummy()         const;
  bool          is_constant()      const;
  Scalar        average()          const; 
  Scalar        center()           const;
  Scalar        scale()            const;
  void          write_to(std::ostream& os) const;

};  


////  LagFeature     LagFeature     LagFeature     LagFeature     LagFeature     LagFeature     LagFeature     

class LagFeature : public FeatureABC
{
  typedef std::string string;
  
 private:
  Feature     mFeature;
  int         mLag;
  string      mLagStr;
  
 public:
  virtual ~LagFeature() {};
  
 LagFeature(Feature f, size_t lag, size_t blockSize=1) : FeatureABC(f->size()), mFeature(f), mLag((int)lag*(int)blockSize)
    { std::ostringstream ss; ss << lag; mLagStr = ss.str(); }

  string        class_name()       const;
  string        name()             const;
  string        operator_name()    const;
  DependenceMap dependence_map()   const;
  int           degree()           const;
  Arguments     arguments()        const;
  Column<Scalar>column()           const;
  Iterator      begin()            const;
  Iterator      end()              const;
  Range         range()            const;
  bool          is_dummy()         const;
  bool          is_constant()      const;
  Scalar        average()          const; 
  Scalar        center()           const;
  Scalar        scale()            const;
  void          write_to(std::ostream& os) const;

  int           lag ()             const;
};
  

//  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

class InteractionFeature : public FeatureABC
{
  typedef std::string string;

  Feature       mFeature1;
  Feature       mFeature2;
  DependenceMap mDependenceMap;
  Scalar        mCtr1, mCtr2;
  string        mName;

 public:
  virtual ~InteractionFeature() {}
  
  InteractionFeature(Feature const& f1, Feature const& f2)                              // names built in using map to define canonical order
    : FeatureABC(f1->size()), mFeature1(f1), mFeature2(f2), mDependenceMap(), mCtr1(0.0), mCtr2(0.0)
    { make_dependence_map(); center_features();  collect_attributes(); make_name();  }

  string         class_name()       const;
  string         name()             const;
  string         operator_name()    const;
  DependenceMap  dependence_map()   const;
  int            degree()           const;
  Arguments      arguments()        const;
  Column<Scalar> column()           const;
  Iterator       begin()            const;
  Iterator       end()              const;
  Range          range()            const;
  bool           is_dummy()         const;
  bool           is_constant()      const;
  Scalar         average()          const; 
  Scalar         center()           const;
  Scalar         scale()            const;
  void           write_to(std::ostream& os) const;

 private:
  void        make_dependence_map();
  void        center_features();
  void        collect_attributes();
  void        make_name();
};


//  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  

class LinearCombinationFeature : public FeatureABC
{
  typedef std::string string;
  
  std::vector<Scalar>  mBeta;      // b[0] is constant, as in regr
  std::vector<Feature> mFeatures;
  string               mName;
  Column<Scalar>       mColumn;    // place to store the lin comb
  
 public:
  virtual ~LinearCombinationFeature() {}
  
 LinearCombinationFeature(int n, std::string name, std::vector<Scalar> const& b, std::vector<Feature> const& fv)
    :                                                      
    FeatureABC(n), 
      mBeta(b), mFeatures(fv), mName(name       ), mColumn(Column<Scalar>("Linear Comb", n)) { if (valid_args()) fill_column(); }

 LinearCombinationFeature(int n, std::vector<Scalar> const& b, std::vector<Feature> const& fv)
   : LinearCombinationFeature(n, LinearCombinationFeature::make_name(b), b, fv) { }
    
  string         class_name()       const;
  string         name()             const;
  string         operator_name()    const;
  DependenceMap  dependence_map()   const;
  int            degree()           const;
  Arguments      arguments()        const;
  Column<Scalar> column()           const;
  Iterator       begin()            const;
  Iterator       end()              const;
  Range          range()            const;
  bool           is_dummy()         const;
  bool           is_constant()      const;
  Scalar         average()          const; 
  Scalar         center()           const;
  Scalar         scale()            const;
  void           write_to(std::ostream& os) const;

  string        long_name()        const;
 private:
  std::string   make_name(std::vector<Scalar> b)     const ;
  bool          valid_args()    const ;
  void          fill_column();
};




////  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  

template<class Op>
class UnaryFeature : public FeatureABC
{
  typedef std::string string;

  Op             mOp;
  Feature        mFeature;
  mutable Scalar mMean;  // cache

 public:
  virtual ~UnaryFeature() {}
  
  UnaryFeature(Op op, Feature const& f)
    : FeatureABC(f->size()), mOp(op), mFeature(f), mMean(std::nan("")) { }
  
  string         class_name()       const;
  string         name()             const;
  string         operator_name()    const;
  DependenceMap  dependence_map()   const;
  int            degree()           const;
  Arguments      arguments()        const;
  Column<Scalar> column()           const;
  Iterator       begin()            const;
  Iterator       end()              const;
  Range          range()            const;
  bool           is_dummy()         const;
  bool           is_constant()      const;
  Scalar         average()          const; 
  Scalar         center()           const;
  Scalar         scale()            const;
  void           write_to(std::ostream& os) const;

};


////  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature

template<class Op>
class BinaryFeature : public FeatureABC
{
  typedef std::string string;

  Op mOp;
  Feature   mFeature1;
  Feature   mFeature2;

 public:  
  BinaryFeature(Op op, Feature const& f1, Feature const& f2)
    : FeatureABC(f1->size()), mOp(op), mFeature1(f1), mFeature2(f2) { }

  string         class_name()       const;
  string         name()             const;
  string         operator_name()    const;
  DependenceMap  dependence_map()   const;
  int            degree()           const;
  Arguments      arguments()        const;
  Column<Scalar> column()           const;
  Iterator       begin()            const;
  Iterator       end()              const;
  Range          range()            const;
  bool           is_dummy()         const;
  bool           is_constant()      const;
  Scalar         average()          const; 
  Scalar         center()           const;
  Scalar         scale()            const;
  void           write_to(std::ostream& os) const;
  
};


//  Feature Source    Feature Source    Feature Source    Feature Source    Feature Source    Feature Source    Feature Source    Feature Source

/*
  Converts collection of columns into features.  Reports summary properties of collection,
  such as number of cases, streams, etc.   Then provides filtering of these features.
*/


class FeatureSource
{
 public:
  typedef SCALAR                   Scalar;
  typedef std::vector<std::string> StringVector;
  typedef std::set<std::string>    StringSet;

 private:
  const int        mSkip;           // skip this count of leading cases when transfer to model
  StringVector     mStreams;        // use vector to keep ordered
  FeatureVector    mFeatures;
  
 public:

 FeatureSource(std::vector<Column<Scalar>> const& cols, int skip = 0)    : mSkip(skip) { initialize(cols); }

  int             number_skipped_cases ()   const  { return mSkip; }
  int             number_of_streams  ()     const  { return (int) mStreams.size();  }
  std::string     stream_name(int i)        const  { return mStreams[i]; }
  StringVector    stream_names()            const  { return mStreams; }
  int             number_of_features ()     const  { return (int) mFeatures.size(); }
  
  FeatureVector   features_with_attribute (std::string attr)                    const;
  FeatureVector   features_with_attributes(StringSet const& attrs)              const;
  FeatureVector   features_with_attribute (std::string attr, std::string value) const;
  
  void print_summary (std::ostream& os)     const;
  
 private:
  void initialize (std::vector<Column<Scalar>> cols);

};


FeatureVector
make_eigenword_feature_vector (std::string fileName, size_t dim, Text::SimpleEigenwordDictionary const& dict);


#endif

