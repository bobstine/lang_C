// $Id: feature_factory.h,v 1.1 2008/01/20 19:55:40 bob Exp $

/*
 The feature factory takes care to combine features that are used
 by some recommenders, particularly those that are compositions of
 several others (such as X^3 * Z^2).
*/

#ifndef _FEATURE_FACTORY_H_
#define _FEATURE_FACTORY_H_

#include "my_features.h"
#include <iostream>
#include <map>


class FeatureFactory : public std::unary_function<Column, FeatureABC*>
{
 public:
  typedef std::vector<Column>                ColumnVector;
  typedef std::vector<FeatureABC*>           FeatureVector;
  typedef std::map<std::string, FeatureABC*> FeatureMap;
  typedef FeatureABC* (FeatureFactory::*     Constructor    )(std::istream&, FeatureVector const&);

 private:
  std::map<std::string, Constructor> mStreamConstructorMap;
  FeatureMap                         mColumnMap;
  FeatureMap                         mInteractionMap;
  FeatureMap                         mLinearCombinationMap;
  FeatureMap                         mUnaryMap;
  FeatureMap                         mBinaryMap;
  ColumnVector                       mColumns;
  
 public:
  ~FeatureFactory() { free_column_memory(); }

  FeatureFactory(ColumnVector const& cv)
    : mStreamConstructorMap(), mColumnMap(), mInteractionMap(), mLinearCombinationMap(), mUnaryMap(), mBinaryMap(), mColumns(cv)
    { build_stream_constructor_map(); convert_initial_column_vector(); }

  FeatureVector        features(std::string const& className) const;
  
  FeatureABC*          operator()(Column const&c) { return make_column_feature_ptr(c); }

  template<class Iter>
    void               append_features_from_stream(std::istream& is, FeatureVector const& cols, Iter it);
  FeatureABC*          make_feature_ptr_from_stream(std::istream& is, FeatureVector const& fv);

  Column*              make_empty_column_ptr(std::string const& name);
  Column*              make_empty_column_ptr(std::string const& name, int dim);
  ColumnFeature*       make_column_feature_ptr(std::string const& name, std::vector<double> const& v);
  ColumnFeature*       make_column_feature_ptr(Column const& c);
  
  InteractionFeature*  make_interaction_feature_ptr(FeatureABC const* f1, FeatureABC const* f2);

  LinearCombinationFeature*
                       make_linear_combination_feature_ptr(std::vector<double> const& b, std::vector<FeatureABC*> const& fv);
  template<class Op>
    UnaryFeature<Op>*  make_unary_feature_ptr(Op const& op, FeatureABC const* f);
  template<class Op>
    BinaryFeature<Op>* make_binary_feature_ptr(Op const& op, FeatureABC const* f1, FeatureABC const* f2);
  
 private:  
  void                 convert_initial_column_vector();
  void                 build_stream_constructor_map();
  void                 free_column_memory();
  
  FeatureABC  *        find_feature (FeatureABC const* f,  FeatureVector const& fv);
  FeatureABC  *        find_feature (std::string const& s, FeatureVector const& fv);
  
  FeatureABC  *        make_column_feature_ptr_from_stream            (std::istream& is, FeatureVector const& source);
  FeatureABC  *        make_interaction_feature_ptr_from_stream       (std::istream& is, FeatureVector const& source);
  FeatureABC  *        make_linear_combination_feature_ptr_from_stream(std::istream& is, FeatureVector const& source);
  FeatureABC  *        make_unary_feature_ptr_from_stream             (std::istream& is, FeatureVector const& source);
  FeatureABC  *        make_binary_feature_ptr_from_stream            (std::istream& is, FeatureVector const& source);
};



inline
std::ostream&
operator<<(std::ostream& os, FeatureFactory::FeatureVector const& fv)
{
  std::cout << "      ";
  std::copy(fv.begin(), fv.end(), std::ostream_iterator<FeatureABC*>(os, "\n      "));
  return os;
}

#include "feature_factory.Template.h"

#endif
