#include "adapter.h"
#include "debug.h"
#include <assert.h>



//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream


template<class Model>
bool
FitStream<Model>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  assert (used.size() > 0);                                                  // otherwise the stream should be empty
  std::string lastVarName = used.back()->name();                             // check name of last used feature for name signature
  bool foundSig = (std::string::npos != lastVarName.find(mSignature));       // ::npos means not found
  if (foundSig) 
  { debugging::debug("FSTR",4) << "Fit stream already used; current feature is not okay.\n";
    mLastQ = mModel.q();                                                     // signals empty
    return false;
  }
  else
  { debugging::debug("FSTR",4) << "Fit stream current feature is okay.\n";
    return true;
  }
}


template<class Model>
void
FitStream<Model>::build_next_feature()
{
  std::vector<int> powers;
  mFit = Column(feature_name().c_str(), mSkip + mModel.n_total_cases());     // grab current fit
  double *fit (mFit->begin());
  for(int i=0; i<mSkip; ++i)
    *fit++ = 0;
  mModel.fill_with_fit(mFit->begin() + mSkip);
  mFit->update();
  for (int j = 2; j <= mPower; ++j)
    powers.push_back(j);
  debugging::debug("FSTR",4) << "Fit stream constructs powers 2-" << mPower <<" of " << mFit->name() << std::endl;
  mLastQ = mModel.q();                                                       // will be empty until next is added
  mHead = powers_of_column_feature(mFit,powers);
}


///////////////    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteractgion Streams
//
//         f1    f2   f3   f4
//    f1    *     *    *    *
//    f2          *    *    *
//    f3               *    *
//    f4                    *
//                               mPos2 says which column; mPos1 tracks row, moving down from first row

template<class Source>
void
InteractionStream<Source>::inc_counters()      // no checks; just increment indices
{
  unsigned Km1 (mSource.size()-1);
    
  if (mPos2 < Km1)
  { ++mPos2;
    return;
  }
  if (mPos1 < Km1)
  { ++mPos1;
    mPos2 = mPos1 + (mDiag?0:1);
    return;
  }
  mPos1 = mSource.size();
}


template<class Source>
void
InteractionStream<Source>::increment_positions()
{
  unsigned k (mSource.size());
  inc_counters();
  if ((mPos1 == k) || (mPos2 == k)) return;
  // skip constants
  while (mSource[mPos1]->is_constant() && (mPos1 < k-1))
  { ++mPos1;
    mPos2 = mPos1 + (mDiag?0:1);
  }
  if ((mPos1 == k) || (mPos2 == k)) return;
  // dont square a dummy
  if ((mPos1 == mPos2) && mSource[mPos1]->is_dummy())
    ++mPos2;
  if (mPos2 == k) return;
  // need different parents
  while((mPos1 < k) && (mPos2 < k) && indicators_from_same_parent(mSource[mPos1], mSource[mPos2]))
    inc_counters();
}


template<class Source>
bool
InteractionStream<Source>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const& skipped) const
{
  if(mHead.size() == 0) return false;
  std::string name (feature_name());
  //  if (found_name_in_feature_vector(name, used, "model features") || found_name_in_feature_vector(name,skipped, "skipped features"))
  //{ debugging::debug("INST",4) << "Interaction stream " << mName << " avoiding redundant variable." << std::endl;
  //  return false;
  //}
  return true;
}
  
template<class Source>
int   
InteractionStream<Source>::number_remaining() const 
{
  // indices at limit indicate empty
  if ((mPos1 == mSource.size()) || (mPos2 == mSource.size()))
    return 0;
  int d (mSource.size()); 
  int m (mPos2+1); 
  return (d * (d+1))/2 - (m*(1+m))/2 + mPos1;
}


template<class Source>
void
InteractionStream<Source>::build_next_feature()
{
  Feature  x1 (mSource[mPos1]);  
  Feature  x2 (mSource[mPos2]);
  // increment position
  increment_positions();
  // make the interaction
  mHead.push_back(Feature(x1,x2));
}


//  Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream

template<class Source1, class Source2>
  void
  CrossProductStream<Source1, Source2>::print_to(std::ostream& os)  const
{
  os << "CPST: " << name() << ": " ;
  if(empty())
    os << " is empty.";
  else
    os << mSlowPos << " @ " << mPos[mSlowPos];
}


template<class Source1, class Source2>
  void
  CrossProductStream<Source1, Source2>::update_position_vector()    const         // update mutable sources
{
  while (mPos.size() < mSlowSource.size())
    mPos.push_back(0);
}


template<class Source1, class Source2>
  int
  CrossProductStream<Source1, Source2>::number_remaining()          const
{
  update_position_vector();
  int num (0);
  for(std::vector<int>::const_iterator it=mPos.begin(); it != mPos.end(); ++it)
    num += mFastSource.size() - *it;
  return num;
}


template<class Source1, class Source2>
  bool
  CrossProductStream<Source1, Source2>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  return (mHead.size() != 0)       &&
    (!mHead[0]->is_constant())     &&
    (!found_feature_name_in_vector(feature_name(), used, "model features")) ;
}


template<class Source1, class Source2>
  void
  CrossProductStream<Source1, Source2>::build_next_feature()
{
  debugging::debug("CPST",3) << name() << " slow[" << mSlowPos << "/" << mSlowSource.size()
			     << "] x fast[" << mPos[mSlowPos] << "/" << mFastSource.size() << "]: "
			     << mSlowSource[0]->name() << " x " << mFastSource[0]->name() << std::endl;
  Feature  xs (mSlowSource[mSlowPos]);
  Feature  xf (mFastSource[mPos[mSlowPos]]);
  // increment position
  //       (mSlowPos >= (int) mSlowSource.size())   // happens when increment_position but none left
  ++mPos[mSlowPos];  
  mSlowPos = 0;                                 // now go back to check that list has not grown
  update_position_vector();
  for(std::vector<int>::const_iterator it=mPos.begin(); it!=mPos.end(); ++it)
  { if (*it < (int) mFastSource.size())
      break;
    else
      ++mSlowPos;
  }
  debugging::debug("CPST",4) << "Increment position to " << mSlowPos << " x " << mPos[mSlowPos] << std::endl;
  // store feature
  mHead.push_back(Feature(xs,xf));
}



///  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream


template<class Source>
void
PolynomialStream<Source>::build_next_feature()
{
  Feature x  (mSource[mPos]);
  debugging::debug("PLYS",4) << "Stream " << name() << " making polynomial subspace from feature " <<  x->name() << std::endl;
  // increment position
  ++mPos; 
  // construct new head
  if (!x->is_used_in_model())    // include X if not in model
    mHead.push_back(x);
  mHead.push_back(Feature(Function_Utils::Square(), x));
  if(mDegree>2) 
    mHead.push_back(Feature(Function_Utils::Cube(), x));
  for (int j=4; j<=mDegree; ++j)
    mHead.push_back(Feature(Function_Utils::Power(j), x));
}


template<class Source>
bool
PolynomialStream<Source>::current_feature_is_okay(std::vector<Feature> const&, std::vector<Feature> const&)
{ 
  Feature  feature (mSource[mPos]);
  std::string name (feature->name());
  debugging::debug("PLYS",4) << " Polynomial stream is considering variable '" << name << "'\n";
  // avoid calibration variables, powers, interactions
  if (feature->degree()>1)                              return false;
  if (name.size() >= 4 && "cube" == name.substr(0,4))   return false;
  if (name.size() >= 6 && "square" == name.substr(0,6)) return false;
  if (std::string::npos != name.find("Y_hat_") )        return false;
  return ( ! (feature->is_dummy() || (feature->is_constant()) ) );
}




//  NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams  


template<class Source>
void
NeighborhoodStream<Source>::build_next_feature()   
{ 
  Feature x  (mSource[mPos]);
  debugging::debug("NBDS",4) << "Stream " << name() << " making indexed feature from " <<  x->name() << std::endl;
  ++mPos; 
  mHead.push_back(make_indexed_feature(x,mIndexColumn));
}


template<class Source>
void
NeighborhoodStream<Source>::print_to(std::ostream& os)          const
{
  os << "Neighborhood feature stream " << name();
  if (empty())
    os << " is empty.";
  else
    os << " has " << number_remaining() << " features.";
}

template<class Source>
bool
NeighborhoodStream<Source>::current_feature_is_okay(FeatureVector const&, FeatureVector const&)   const
{
  Feature  feature (mSource[mPos]);
  std::string name (feature->name());
  debugging::debug("NBDS",4) << " Neighborhood stream is considering variable '" << name << "'\n";
  if (name.size() >= 4 && "cube" == name.substr(0,4))                  // avoid calibration variables, powers
    return false;
  if (name.size() >= 6 && "square" == name.substr(0,6))
    return false;
  if (std::string::npos == name.find(mSignature))                      // must include the signature
    return false;
  if (std::string::npos != name.find(mIndexColumn->name()))            // avoid already-indexed variables
    return false;
  if (std::string::npos != name.find("Y_hat_") )
    return false;
  return ( (!feature->is_constant()) && (!feature->is_dummy()) );
}



///   SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream

template<class Source, class Pred, class Trans>
  void
  SubspaceStream<Source, Pred, Trans>::build_next_feature()
{
  debugging::debug("SUBS",4) << mName << " building bundle with " << mBundleSize << " elements.\n";
  //  show names
  //  for (FeatureVector::const_iterator it = mBundle.begin(); it != mBundle.end(); ++it)   std::cout << (*it)->name() << " "; std::cout << std::endl;

  //  Dump the bundle to a file to see what's going on: stream to file and then transpose
  /*  
      std::string file ("/Users/bob/C/auctions/data/anes/bundle.txt");
      std::ofstream bs (file.c_str());
      if (bs)
      { debugging::debug("SUBS", 3) << "Writing bundle to file " << file << std::endl;
        for (unsigned int j=0; j<mBundle.size(); ++j)
        { mBundle[j]->write_values_to(bs);
          bs << std::endl;  }}
      else  debugging::debug("SUBS", 3) << " Non-fatal error; could not open file " << file << " to dump feature bundle.\n ";
  */
  // read input data into a feature vector
  FeatureVector input;
  while (((int)input.size()<mBundleSize) && (mPos<(int)mSource.size()))
  { if(mPredicate(mSource[mPos]))
      input.push_back(mSource[mPos]);
    ++mPos;
  }
  // apply transformation to the block of variables
  mHead = mTransformation(input);
  debugging::debug("SUBS",4) << mName << " transformation completed.\n";  
}

  



