#include "adapter.h"
#include "debug.h"
#include <assert.h>




//  NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams

template<class Source>
void
NeighborhoodStream<Source>::build_next_feature()
{
  while (number_remaining()>0)
  { std::string fname (mSource[mPos]->name());
    if (
	(mSource[mPos]->is_constant())                          ||
	(mSource[mPos]->is_dummy())                             ||
	(fname.size() >= 4 && "cube" == fname.substr(0,4))      ||     // avoid powers
	(fname.size() >= 6 && "square" == fname.substr(0,6))    ||
	(std::string::npos != fname.find(mSignature))           ||     // includes calibration signature
	(std::string::npos != fname.find(mIndexColumn->name())) ||     // already-indexed variable
	(std::string::npos != fname.find("Y_hat_") )                   // calibration variable
	)
    { debugging::debug("NBDS",4) << name() << " rejected building feature from " << mSource[mPos]->name() << std::endl;
      ++mPos;
    }
    else  // build feature using current position
    { debugging::debug("NBDS",4) << name() << " making neighborhood feature from " << mSource[mPos]->name() << std::endl;
      set_head(make_indexed_feature(mSource[mPos],mIndexColumn));
      ++mPos;
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
  if (std::string::npos != accepted.back()->name().find(mSignature))                   // npos means not found; != npos means found
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
int   
InteractionStream<Source>::number_remaining() const 
{
  int k (mSource.size());
  // indices at limit indicate empty
  if ((mPos1 == k) || (mPos2 == k))
    return 0;
  else // crude, quick positive estimate
  { int rowsLeft (k-mPos1);
    return k-mPos2 + (rowsLeft * (rowsLeft-1))/2;
  }
}


template<class Source>
void
InteractionStream<Source>::inc_counters()
{
  int Km1 (mSource.size()-1);
  
  if (mPos2 < Km1)          // increment column counter only
    ++mPos2;
  else if (mPos1 < Km1)     // move to next row and reset column counter
  { ++mPos1;
    if (mIncludeDiagonal && !mSource[mPos1]->is_dummy())    // dont square a dummy
      mPos2 = mPos1;
    else
      mPos2 = mPos1 + 1;
  }
  else
    mPos1 = mSource.size();
}


template<class Source>
bool
InteractionStream<Source>::find_next_position()
{
  int k (mSource.size());
  inc_counters();
  if ((mPos1 == k) || (mPos2 == k)) return false;
  // skip constants
  while (mSource[mPos1]->is_constant() && (mPos1 < k-1))
  { debugging::debug("INTS",4) << name() << " encountered constant feature " << mSource[mPos1]->name() << "; skipping.\n";
    mPos2 = k;
    inc_counters();
  }
  // need different parents
  while((mPos1 < k) && (mPos2 < k) && indicators_from_same_parent(mSource[mPos1], mSource[mPos2]))
  { debugging::debug("INTS",4) << name() << " encountered features with common parent: " << mSource[mPos1]->name() << " & " << mSource[mPos2] << std::endl;
    inc_counters();
  }
  if ((mPos1 == k) || (mPos2 == k))
  { debugging::debug("INTS",4) << name() << " is empty with indices " << mPos1 << "," << mPos2 << std::endl;
    return false;
  }
  else
    return true;
}

template<class Source>
void
InteractionStream<Source>::build_next_feature()
{
  // increment position
  if( find_next_position() )
    set_head(Feature(mSource[mPos1], mSource[mPos2]));
  else
    debugging::debug("INTS",3) << "Interaction stream not able to find new pair.\n";
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
	 (!found_name_in_feature_vector(feature_name(), accepted, "model features")) )
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
    if ( (mSource[mPos]->degree()>1)                        ||       // avoid calibration variables, powers, interactions
	 (fname.size() >= 4 && "cube" == fname.substr(0,4))   ||   
	 (fname.size() >= 6 && "square" == fname.substr(0,6)) ||
	 (std::string::npos != fname.find("Y_hat_") )        ||
	 (mSource[mPos]->is_dummy())                        ||
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

  



