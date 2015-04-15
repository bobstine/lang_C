// -*- c++ -*-
#include "column_stream.h"

#include "read_utils.h"
#include "nan_utils.h"
#include "debug.h"

#include <iostream>
#include <map>
#include <set>
#include <cmath>         // floor
#include <utility>       // pair
#include <assert.h>

typedef float Scalar;

const std::string tag = "CSTR";

using std::string;
using debugging::debug;

/////  These functions write the bundles  (adapted from embed_auction.cc in prep_error project)

size_t
insert_common_feature(string fieldName, Attributes attributes, std::vector<string> const& data,  // writes 2 if missing present
			 std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter);
size_t
insert_categorical_bundle(string fieldName, Attributes attributes, std::vector<string> const& data,
			  std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter);
size_t
insert_eigenword_bundle(string fieldName, Attributes attributes, std::vector<string> const& data,
			std::map<std::string, std::vector<Scalar>> const& dictionary,
			std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter);
size_t
insert_bundle(string bundleName, Attributes attributes,
	      std::vector<std::vector<Scalar>> const& coor, std::vector<double> const& sum, int nMissing,
	      std::vector<string> const& labels, string attributeOfLabels,   // var name labels optionally added as attribute if not empty
	      std::back_insert_iterator<std::vector< Column<Scalar>>> columnIterator);


/////
  
std::pair<size_t,size_t>
insert_columns_from_stream (std::istream &input,
			    std::map<string, std::vector<Scalar>> const& dictionary,      // handles domain=word
			    std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter)
{
  assert (input.good());
  string theLine;                                                   // assume n obs on first line 
  std::getline(input, theLine);
  size_t nObs = (size_t) std::stoi(theLine);
  debug(tag,2) << "Reading columns with " << nObs << " observations from input stream.\n";
  size_t nInputFeatures = 0;
  size_t nOutputColumns = 0;
  while(!input.eof())
  { // read three lines for each feature
    string fieldName;
    std::getline(input, fieldName);
    read_utils::cleanup_name(fieldName);
    string description;
    std::getline(input, description);
    Attributes attributes{description};
    std::vector<string> data{nObs};
    for(size_t i=0; i<nObs; ++i) input >> data[i];
    std::getline(input, theLine);  // flush rest
    ++nInputFeatures;
    // expand mapped features as needed
    if (attributes["type"]=="map")
    { attributes.erase_attribute("type");           
      attributes.add_attribute("parent", fieldName);
      string domain = attributes["domain"];
      assert(domain != "");
      if(domain == "words")
	nOutputColumns += insert_eigenword_bundle   (fieldName, attributes, data, dictionary, columnInserter);
      else if (domain == "categories")
	nOutputColumns += insert_categorical_bundle (fieldName, attributes, data,             columnInserter);
      else
	std::cerr << tag << ":  *** ERROR *** Unidentified map domain `" << domain << "' found in column input stream.\n";
    }
    else
      nOutputColumns += insert_common_feature(fieldName, attributes, data, columnInserter);
  }
  debug(tag,2) << "Inserted " << nOutputColumns << " columns, each of length " << nObs << " from input of "
	       << nInputFeatures << " features.\n";
  return std::make_pair(nInputFeatures,nOutputColumns);
}

//     insert_common_feature     insert_common_feature     insert_common_feature     insert_common_feature     insert_common_feature     

size_t
insert_common_feature(string fieldName, std::vector<string> const& data,
			std::back_insert_iterator<std::vector<Column<Scalar>>> colIter)
{
  const size_t n = data.size();
  size_t nMissing = 0;
  double sum = 0.0;
  std::vector<size_t> missing (n,0);
  std::vector<float>  numbers (n);
  for(size_t i=0; i<n; ++i)
  { if(data[i]=="NA")
    { missing[i] = 1;
      ++nMissing;
      numbers[i] = 0;
    }
    else
    { sum += (double) numbers[i];
      std::istringstream(data[i]) >> numbers[i];
    }
  }
  if (0<nMissing)
  { float mean = float( (sum/(double)(n-nMissing)) );
    for(size_t i=0; i<n; ++i)
      if(1==missing[i]) numbers[i] = mean;
  }
  Column<Scalar> var1(fieldName, Attributes("role=x,stream=main"), n, numbers.begin());
  colIter = var1;
  if (0<nMissing)
  { Column<Scalar> var2(fieldName+"_missing", Attributes("role=x,stream=main,indicator=missing,parent="+fieldName), n, missing.begin());
    colIter = var2;
  }
  return (0 < nMissing) ? 2 : 1;
}		      


//     write_categorical_bundle     write_categorical_bundle     write_categorical_bundle     write_categorical_bundle
inline size_t max_size(size_t a, size_t b) { return (a < b) ? b : a; }

void
recode_small_categories (std::map<string,size_t>& categories, std::vector<string> const& data)
{
  // threshold for whether to create a 'OTHERS' category
  const size_t minCount = max_size(25, (size_t) floor((Scalar) 0.001 * (Scalar)(data.size())));
  debug(tag,3) << "Each category must represent at least " << minCount << " observations.\n";
  string otherCategoryName = "OTHERS";
  // recode small categories
  std::set<string> smallCategories;
  size_t smallTotalCount = 0;
  for(auto p : categories)
    if(p.second < minCount) smallCategories.insert(p.first);
  if(!smallCategories.empty())
  { for(string c : smallCategories)
    { auto it = categories.find(c);
      smallTotalCount += it->second;
      assert(it != categories.end());
      categories.erase(it);
    }
    categories[otherCategoryName] = smallTotalCount;
    debug(tag,2) << "Combining " << smallCategories.size() << " small categories with total of " << smallTotalCount << " observations.\n";
    for (auto xi : data)
      if (smallCategories.count(xi)) xi = otherCategoryName;
  }
}

size_t
insert_categorical_bundle(string fieldName, Attributes attributes, std::vector<string> const& data,
			  std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter)
{
  const size_t n = data.size();
  std::map<string, size_t> categories;
  for(string item : data) ++categories[item];
  recode_small_categories(categories, data);
  const size_t nCategories = categories.size();
  debug(tag,2) << "Found " << nCategories << " categories for field " << fieldName << std::endl;
  std::vector<std::vector<Scalar>> dummyVars (n);  // must be vector of rows to agree with eigen coor
  for (size_t i=0; i<n; ++i)
  { std::vector<float> row(nCategories);
    auto cat = categories.begin();
    for (size_t j=0; j<nCategories; ++j, ++cat)
    { if (cat->first == data[i])
      { row[j]=1; break; }
    }
    dummyVars[i] = row;
  }
  std::vector<string> labels{categories.size()};
  auto cat = categories.begin();
  for(size_t i=0; i<categories.size(); ++i, ++cat) labels[i] = cat->first;  
  string streamName = fieldName + "_category";
  attributes.add_attribute("stream", streamName);
  return insert_bundle(fieldName, attributes, dummyVars, std::vector<double>(0), 0, labels, "category", columnInserter);
}

//     write_eigenword_bundle     write_eigenword_bundle     write_eigenword_bundle     write_eigenword_bundle

size_t
insert_eigenword_bundle(string fieldName, Attributes attributes, std::vector<string> const& data,
			std::map<string,std::vector<Scalar>> const& dictionary,
			std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter)
{
  std::vector<std::vector<Scalar>> coord (data.size());   // vector of rows
  size_t dictDim = dictionary.begin()->second.size();
  debug(tag,3) << "Processing eigendata for field " << fieldName << std::endl;
  int nMissing = 0;
  std::vector<double> sum (dictDim, 0.0);
  for (size_t i=0; i<data.size(); ++i)
  { string token = data[i];
    bool missing = false;
    if (token == "NA")                                        // a simple dict assigns nan to 'NA' token
    { ++nMissing;
      missing = true;
    }
    else if (dictionary.count(token) == 0)
      token = "OOV";                                          // if not found, label as OOV
    coord[i] = dictionary.find(token)->second;  
    if (!missing) for(size_t j=0; j<dictDim; ++j) sum[j] += (double) coord[i][j];
  }
  debug(tag,3) << "Found " << nMissing << " missing cases for eigenword bundle " << fieldName << std::endl;
  std::vector<string> labels{dictDim};
  for (size_t i=0; i<dictDim; ++i) labels[i] = "ew" + std::to_string(i);
  string attributeForLabels("");
  attributes.add_attribute("stream",fieldName);
  return insert_bundle(fieldName, attributes, coord, sum, nMissing, labels, attributeForLabels, columnInserter);
}
  
//     insert_bundle     insert_bundle     insert_bundle     insert_bundle     insert_bundle     insert_bundle     insert_bundle     
//
//      Encodes any missing using the input sum and count.
//

size_t
insert_bundle(string bundleName, Attributes const& commonAttributes,
	     std::vector<std::vector<float>> const& coor, std::vector<double> const& sum, int nMissing, 
	     std::vector<string> const& labels, string attributeOfLabels,
	     std::back_insert_iterator< std::vector<Column<Scalar>> > &columnInserter)
{
  size_t n = coor.size();
  size_t dictDim = coor[0].size();
  assert (dictDim == labels.size());
  if (0 < nMissing)  // write one missing indicator for the bundle
  { string varName = bundleName + "_" + "Missing";
    Attributes attributes = commonAttributes;
    attributes.add_attribute("indicator", "missing");
    Column<Scalar> missingIndicator{varName, attributes, n};
    Scalar *ptr = missingIndicator->begin();
    for(size_t i=0; i<n; ++i)
      *ptr++ = (IsNanf((float)coor[i][0])) ? 1.0 : 0.0;
    columnInserter = missingIndicator;
  }
  for(size_t d=0; d<dictDim; ++d)
  { string varName = bundleName + "_" + labels[d];
    Attributes attributes = commonAttributes;
    if (!attributeOfLabels.empty())
      attributes.add_attribute(attributeOfLabels, labels[d]);
    Column<Scalar> theColumn{varName, attributes, n};
    Scalar *ptr = theColumn->begin();
    if(nMissing == 0)
      for(size_t i=0; i<n-1; ++i) *ptr++ = coor[i][d];
    else
    { double mean = sum[d]/(double)(n-nMissing);
      for(size_t i=0; i<n; ++i)
      { float x = (float)coor[i][d];
	if (IsNan(x))
	  *ptr++ = (float)mean;
	else
	  *ptr++ = x;
      }
    }
  }
  return (0 < nMissing) ? dictDim+1 : dictDim;
}
