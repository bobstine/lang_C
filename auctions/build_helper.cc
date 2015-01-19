
void
FiniteCauchyShare::init()
{
  for (int j=0; j<mCount; ++j)
    mSum = mSum + p(j);
}

double
FiniteCauchyShare::operator()(int j) const
{
  assert(j >= 0);
  assert(j < mCount);
  return mTotalAlpha * p(j) / mSum;
}


double
FiniteCauchyShare::p(int j)
{
  return 1.0/(double)((j+1)*(j+1)); }
}


double
time_since(time_t const& start)
{
  return  double(clock() - start)/CLOCKS_PER_SEC;
}

std::pair< std::pair<int,double>, std::pair<int,double> >
initialize_sums_of_squares(std::vector<Column> y);

