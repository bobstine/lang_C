#include "adapter.h"
#include "debug.h"
#include "range_traits.h"

#include <assert.h>


//  BaseStream

template< class Collection >
bool
BaseStream::found_name_among_features (std::string const& name, Collection const& features, std::string const& description) const
{
   if (name.size() == 0)
     return false;
   for (typename range_traits<Collection>::const_iterator it = Ranges::begin(features); it != Ranges::end(features); ++it)
  { if (name == (*it)->name())
    { debugging::debug("FETR", 4) << "Found feature " << name << " in " << description << std::endl;
      return true; 
    }
  }
  return false;
}



//  NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams

template<class Source>
void
NeighborhoodStream<Source>::build_next_feature()
{
  while (mRemain>0)
  { std::string fname ((*mpFeature)->name());
    if (
	((*mpFeature)->is_constant())                           ||
	((*mpFeature)->is_dummy())                              ||
	(fname.size() >= 4 && "cube" == fname.substr(0,4))      ||     // avoid powers
	(fname.size() >= 6 && "square" == fname.substr(0,6))    ||
	(std::string::npos != fname.find(mSignature))           ||     // includes calibration signature
	(std::string::npos != fname.find(mIndexColumn->name())) ||     // already-indexed variable
	(std::string::npos != fname.find("Y_hat_") )                   // calibration variable
	)
    { debugging::debug("NBDS",4) << name() << " rejected Building. feature from " << fname << std::endl;
      --mRemain; ++mpFeature;
    }
    else  // build feature using current position
    { debugging::debug("NBDS",4) << name() << " making neighborhood feature from " << fname << std::endl;
      set_head(make_indexed_feature(*mpFeature,mIndexColumn));
      --mRemain; ++mpFeature;
      return;
    }
  }
}


//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
void
FitStream<Model>::build_next_feature()
{
  // check for logic error
  assert(mLastQ != mModel.q());
  // need to check name of last accepted; dont have one if its us
  std::string lastName (last_name_in_list(mAccepted));
  if (std::string::npos != lastName.find(mSignature))                          // npos means not found; != npos means found
    debugging::debug("FITS", 3) << "Found fit variable in most recent model; cannot build feature.\n";
  else // build powers of current fit
  { std::vector<int> powers;
    mFit = Column(feature_name().c_str(), mSkip + mModel.n_total_cases());     // grab current fit
    double *fit (mFit->begin());
    for(int i=0; i<mSkip; ++i)
      *fit++ = 0;
    mModel.fill_with_fit(mFit->begin() + mSkip);
    mFit->update();
    for (int j = 2; j <= mPower; ++j)
      powers.push_back(j);
    debugging::debug("FSTR",4) << "Fit stream constructs powers 2-" << mPower <<" of " << mFit->name() << std::endl;
    set_head(powers_of_column_feature(mFit,powers));
  }
  mLastQ = mModel.q();                                                        //  empty until other predictor is added to model
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
InteractionStream<Source>::inc_pointers()
{
  ++mpColFeature; --mRemain;
  if (mpColFeature == Ranges::end(mFeatureRange))   // move diagonal "row" pointer
  { ++mpDiagFeature;
    mpColFeature = mpDiagFeature;
    if (!mIncludeDiagonal)
      ++mpColFeature;
    else if ((*mpDiagFeature)->is_dummy())  // dont square a dummy
    { ++mpColFeature;
      -- mRemain;
    }
  }
}


template<class Source>
bool
InteractionStream<Source>::find_next_position()
{
  inc_pointers();
  if (mRemain == 0) return false;
  // skip constants
  while ((*mpDiagFeature)->is_constant())
    inc_pointers();
  // need different parents
  while(mRemain > 0 && indicators_from_same_parent(*mpDiagFeature, *mpColFeature))
  { debugging::debug("INTS",4) << name() << " encountered features with common parent: " << (*mpDiagFeature)->name() << " & " << (*mpColFeature)->name() << std::endl;
    inc_pointers();
  }
  if (mRemain == 0)
  { debugging::debug("INTS",4) << name() << " has run out of features.\n";
    return false;
  }
  return true;
}

template<class Source>
void
InteractionStream<Source>::build_next_feature()
{
  assert(mRemain > 0);
  assert(mpColFeature != Ranges::end(mFeatureRange));
  set_head(Feature(*mpDiagFeature, *mpColFeature));
  find_next_position();
}



//  Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream

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
  void
  CrossProductStream<Source1, Source2>::build_next_feature()
{
  debugging::debug("CPST",3) << name() << " slow[" << mSlowPos << "/" << mSlowSource.size()
			     << "] x fast[" << mPos[mSlowPos] << "/" << mFastSource.size() << "]: "
			     << mSlowSource[0]->name() << " x " << mFastSource[0]->name() << std::endl;
  while (number_remaining() > 0)
  { Feature  xs (mSlowSource[mSlowPos]);
    Feature  xf (mFastSource[mPos[mSlowPos]]);
    Feature candidate(xs,xf);
    if ( (!candidate->is_constant()) &&
	 (!found_name_among_features(feature_name(), mAccepted, "model features")) )
    { set_head(candidate);
      return;
    }
    else
    { ++mPos[mSlowPos];  
      mSlowPos = 0;                                 // now go back to check that list has not grown
      update_position_vector();
      for(std::vector<int>::const_iterator it=mPos.begin(); it!=mPos.end(); ++it)
      { if (*it < (int) mFastSource.size())
	  break;
	else
	  ++mSlowPos;
      }
      debugging::debug("CPST",4) << "Increment position to " << mSlowPos << " x " << mPos[mSlowPos] << std::endl;
    }
  }
}



///  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream


template<class Source>
void
PolynomialStream<Source>::build_next_feature()
{
  while (mPos < mSource.size())
  { std::string fname (mSource[mPos]->name());
    debugging::debug("PLYS",4) << " Polynomial stream is considering variable '" << fname << "' \n";
    if ( (mSource[mPos]->degree()>1)                          ||       // avoid calibration variables, powers, interactions
	 (fname.size() >= 4 && "cube" == fname.substr(0,4))   ||   
	 (fname.size() >= 6 && "square" == fname.substr(0,6)) ||
	 (std::string::npos != fname.find("Y_hat_") )         ||
	 (mSource[mPos]->is_dummy())                          ||
	 (mSource[mPos]->is_constant())      )               
    {
      ++mPos;
    }
    else // use this feature
    { debugging::debug("PLYS",4) << "Stream " << name() << " making polynomial subspace from feature " <<  mSource[mPos]->name() << std::endl;
      FeatureVector powers;
      if (!mSource[mPos]->is_used_in_model())    // include X if not in model
	powers.push_back(mSource[mPos]);
      powers.push_back(Feature(Function_Utils::Square(), mSource[mPos]));
      if(mDegree>2) 
	powers.push_back(Feature(Function_Utils::Cube(), mSource[mPos]));
      for (int j=4; j<=mDegree; ++j)
	powers.push_back(Feature(Function_Utils::Power(j), mSource[mPos]));
      set_head(powers);
      ++mPos;
      return;
    }
  }
}


//   SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream


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

template<class Source, class Pred, class Trans>
  void
  SubspaceStream<Source, Pred, Trans>::build_next_feature()
{
  debugging::debug("SUBS",4) << mName << " building bundle with " << mBundleSize << " elements.\n";
  // read input data into a feature vector
  FeatureVector input;
  while (((int)input.size()<mBundleSize) && (mPos<(int)mSource.size()))
  { if(mPredicate(mSource[mPos]))
      input.push_back(mSource[mPos]);
    ++mPos;
  }
  // apply transformation to the block of variables
  set_head(mTransformation(input));
  debugging::debug("SUBS",4) << mName << " transformation completed.\n";  
}

  



