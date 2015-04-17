// Code was superceded in NLP auction by reading text map features directly in input stream

#include "simple_vocabulary.h"
#include "simple_eigenword_dictionary.h"

FeatureVector
make_eigenword_feature_vector (std::string fileName, size_t dim, Text::SimpleEigenwordDictionary const& dict);

int main()
{
  const bool downcase = false;
  Text::SimpleVocabulary vocab = Text::make_simple_vocabulary("/home/bob/C/text/test.txt", downcase);
  const int seed = 1234;
  const size_t dim = 10;
  Text::SimpleEigenwordDictionary dict = Text::make_random_simple_eigenword_dictionary(seed, dim, vocab);
  FeatureVector fv = make_eigenword_feature_vector("/home/bob/C/text/test_tokens.txt", dim, dict);
  for (size_t i=0; i<fv.size(); ++i)
    std::cout << "TEST: Eigenword feature... " << fv[i] << " avg=" << fv[i]->average() << std::endl;
  std::cout << std::endl;
}


FeatureVector
make_eigenword_feature_vector (std::string fileName, size_t dim, Text::SimpleEigenwordDictionary const& dict)
{
  std::ifstream input{fileName};
  if (!input.good())
  { std::cerr << tag << "*** ERROR *** Cannot open file " << fileName << " to build eigenword features.\n";
    return FeatureVector();
  }
  std::string theLine;
  std::getline(input, theLine);
  std::istringstream ss{theLine};
  std::string varName;
  ss >> varName;                              // name features 'varName'_ew##
  std::getline(input,theLine);                // dump description, attributes line
  std::vector<std::string> tokens;
  while (input.good())                        // read tokens from 3rd line; need an eol on 3rd line
  { std::string word;
    input >> word;
    trim(word);
    if(word.empty()) break;
    tokens.push_back(word);
  }
  size_t n = tokens.size();
  debugging::debug("FETR",2) << "Read " << n << " tokens (" << tokens[0] << " ... " << tokens[tokens.size()-1]
			     << ") to define " << dim << "-dim eigenwords from " << fileName << ".\n";
  std::vector<std::vector<Scalar>> eigenCoord (dim);
  for (size_t d=0; d<dim; ++d)
    eigenCoord[d] = std::vector<Scalar>(n);    // d vectors, each of length n
  int nMissing = 0;                            // will fill missing values (ie, word not found) with mean
  std::vector<double> sum (dim);
  for (size_t i=0; i<n; ++i)
  { std::string token = tokens[i];
    if (token == "NA")                         // fill these rows later with mean
      ++nMissing;
    else
    { if (dict.count(token) == 0)              // not found != missing (we found a word, but its not in dict)
	token = "OOV";
      std::vector<Scalar> row = dict.find(token)->second;  
      for(size_t d=0; d<dim; ++d)
      { sum[d] += (double) row[d];             // to find mean to fill missing
	eigenCoord[d][i] = row[d];             // transpose
      }
    }
  }
  debugging::debug ("FETR",2) << "Found " << nMissing << " missing cases for eigenword " << varName << std::endl;
  if (0 < nMissing)
  { for(auto x : sum)
      x /= double (nMissing);
    for(size_t i=0; i<n; ++i)
    { if (tokens[i] == "NA")
      {	for(size_t d=0; d<dim; ++d)
	  eigenCoord[d][i] = (Scalar) sum[d];
      }
    }
  }
  FeatureVector fv;                            // finally construct vector of features
  for (size_t d=0; d<dim; ++d)
  { Column<Scalar> column(varName + "_ew" + std::to_string(d), "role x type eigenword stream " + varName, n, eigenCoord[d].begin());
    fv.push_back( Feature(column) );
    debugging::debug("FETR",4) << "Eigenword file " << fileName << " produces feature " << fv[d]->name() << std::endl;
  }
  return fv;
}
