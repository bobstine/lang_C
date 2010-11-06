template< class Collection >
bool
FeaturePredicates::found_name_among_features (std::string const& name, Collection const& features, std::string const& description)
{
  if (name.size() == 0)
    return false;
  for (typename Collection::const_iterator it = features.begin(); it != features.end(); ++it)
  { if (name == (*it)->name())
    { debugging::debug("FPRD", 4) << "Found feature " << name << " in " << description << std::endl;
      return true; 
    }
  }
  return false;
}




