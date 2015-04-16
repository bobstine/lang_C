// -*- c++ -*-
#include "column_stream.h"
#include "column.Template.h"

#include "read_utils.h"
#include "nan_utils.h"
#include "debug.h"

#include <iostream>
#include <map>
#include <set>
#include <cmath>         // floor
#include <utility>       // pair
#include <functional>    // std::function
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
insert_categorical_bundle(string fieldName, Attributes attributes, std::vector<string> & data,  // note use of references for data
			  size_t minCategorySize,
			  std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter);
size_t
insert_eigenword_bundle(string fieldName, Attributes attributes, std::vector<string> const& data,
			std::map<std::string, std::vector<Scalar>> const& dictionary,
			std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter);
size_t
insert_bundle(string bundleName, Attributes attributes, std::vector<std::vector<Scalar>> const& coor,
	      std::vector<double> const& sum, size_t nMissing,
	      std::vector<string> const& labels, string attributeOfLabels,   // var name labels optionally added as attribute if not empty
	      std::back_insert_iterator<std::vector< Column<Scalar>>> columnIterator);

/////
  
std::pair<size_t,size_t>
insert_columns_from_stream (std::istream &input,
			    size_t minCatSize,
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
  std::map<string, size_t> allFieldNames;
  while(!input.eof())
  { // read three lines for each feature
    string fieldName;
    std::getline(input, fieldName);
    read_utils::cleanup_name(fieldName);
    debug(tag,3) << "Building column from field " << fieldName << std::endl;
    if (0 < allFieldNames[fieldName])
    { ++allFieldNames[fieldName];
      debug(tag,1) << "Input column has duplicate name `" << fieldName << "'.\n";
      fieldName += "_dup_" + std::to_string(allFieldNames[fieldName]);
    }
    ++allFieldNames[fieldName];
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
	nOutputColumns += insert_categorical_bundle (fieldName, attributes, data, minCatSize, columnInserter);
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
insert_common_feature(string fieldName, Attributes attributes, std::vector<string> const& data,
			std::back_insert_iterator<std::vector<Column<Scalar>>> colIter)
{
  const size_t n = data.size();
  size_t nMissing = 0;
  double sum = 0.0;
  std::vector<Scalar> missing (n,0);
  std::vector<Scalar> numbers (n);
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
  attributes.add_attribute("role","x");
  if(!attributes.present("stream")) attributes.add_attribute("stream","main");  
  Column<Scalar> var1(fieldName, attributes, n, numbers.begin());
  colIter = var1;
  if (0<nMissing)
  { Column<Scalar> var2(fieldName+"_missing", Attributes("role=x,stream=main,indicator=missing,parent="+fieldName), n, missing.begin());
    colIter = var2;
  }
  return (0 < nMissing) ? 2 : 1;
}

//     insert_categorical_bundle     insert_categorical_bundle     insert_categorical_bundle     insert_categorical_bundle

void
recode_small_categories (size_t minCatSize, std::map<string,size_t> & categories, std::vector<string> & data)  // Warning: references
{
  const string otherCategoryName = "OTHERS";
  std::set<string> smallCategories;
  size_t           smallTotalCount = 0;
  for(auto p : categories)
    if(p.second < minCatSize) smallCategories.insert(p.first);
  if(smallCategories.size() <= 1) return;  // no need to recode one
  for(string c : smallCategories)
  { auto it = categories.find(c);
    smallTotalCount += it->second;
    assert(it != categories.end());
    categories.erase(it);
  }
  categories[otherCategoryName] = smallTotalCount;
  debug(tag,2) << "Combining " << smallCategories.size() << " small categories with total of " << smallTotalCount << " observations.\n";
  for (auto it = data.begin(); it != data.end(); ++it)
    if (0 < smallCategories.count(*it)) *it = otherCategoryName;
}

size_t
insert_categorical_bundle(string fieldName, Attributes attributes, std::vector<string> & data,   // references to attr & data allow modification
			  size_t minCategorySize,
			  std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter)
{
  const size_t n = data.size();
  std::map<string, size_t> categories;
  for(string item : data) ++categories[item];
  recode_small_categories(minCategorySize, categories, data);
  debug(tag,2) << "Found " << categories.size() << " categories for field " << fieldName << std::endl;
  auto orderedCategoryMap = std::multimap<size_t, string, std::function<bool(const size_t, const size_t)>> { // sort by entropy
    ([n](size_t f, size_t s) -> bool { return  (f * (n-f)) > (s * (n - s));})
  }; 
  for(auto p : categories)
    orderedCategoryMap.insert(std::make_pair(p.second,p.first));
  /*
  std::cout << "TEST: Category elements are : ";
  for(auto p : orderedCategoryMap)    std::cout << "[" << p.second << "=" << p.first << "]";
  std::cout << std::endl;
  */     
  const size_t nCategories = orderedCategoryMap.size();
  std::vector<std::vector<Scalar>> dummyVars (n);  // must be vector of rows to agree with eigen coor
  for (size_t i=0; i<n; ++i)
  { std::vector<Scalar> row(nCategories,0);
    auto cat = orderedCategoryMap.begin();
    for (size_t j=0; j<nCategories; ++j, ++cat)
    { if (cat->second == data[i])
      { row[j]=1; break; }
    }
    dummyVars[i] = row;
  }
  std::vector<string> labels{nCategories};
  auto cat = orderedCategoryMap.begin();
  for(size_t j=0; j<nCategories; ++j, ++cat) labels[j] = cat->second;  
  string streamName = fieldName + "_category";
  attributes.add_attribute("stream", streamName);
  return insert_bundle(fieldName, attributes, dummyVars, std::vector<double>(0), 0, labels, "category", columnInserter);
}

//     insert_eigenword_bundle     insert_eigenword_bundle     insert_eigenword_bundle     insert_eigenword_bundle

size_t
insert_eigenword_bundle(string fieldName, Attributes attributes, std::vector<string> const& data,
			std::map<string,std::vector<Scalar>> const& dictionary,
			std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter)
{
  std::vector<std::vector<Scalar>> coord (data.size());       // vector of rows
  size_t dictDim = dictionary.begin()->second.size();
  debug(tag,3) << "Processing eigendata for field " << fieldName << std::endl;
  size_t nMissing = 0;
  size_t nOOV = 0;
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
    if(token == "OOV") ++ nOOV;
    coord[i] = dictionary.find(token)->second;  
    if (!missing) for(size_t j=0; j<dictDim; ++j) sum[j] += (double) coord[i][j];
  }
  debug(tag,2) << "Found " << nMissing << " missing cases and " << nOOV << " OOVs for eigenword bundle " << fieldName << std::endl;
  std::vector<string> labels{dictDim};
  for (size_t i=0; i<dictDim; ++i) labels[i] = "ew" + std::to_string(i);
  string attributeForLabels("");
  attributes.add_attribute("stream",fieldName+"_coord");
  return insert_bundle(fieldName, attributes, coord, sum, nMissing, labels, attributeForLabels, columnInserter);
}

//     insert_bundle     insert_bundle     insert_bundle     insert_bundle     insert_bundle     insert_bundle     insert_bundle     
//
//      Encodes any missing using the input sum and count.
//

size_t
insert_bundle(string bundleName, Attributes commonAttributes, std::vector<std::vector<Scalar>> const& coor,
	      std::vector<double> const& sum, size_t nMissing, 
	      std::vector<string> const& labels, string attributeOfLabels,
	      std::back_insert_iterator< std::vector<Column<Scalar>> > columnInserter)
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
    missingIndicator.init_properties();
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
      for(size_t i=0; i<n; ++i) *ptr++ = coor[i][d];
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
    theColumn.init_properties();
    columnInserter = theColumn;
  }
  return (0 < nMissing) ? dictDim+1 : dictDim;
}
