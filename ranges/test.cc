class CharBase
{
public:
  typedef char* data_type ;
};

class DoubleBase
{
public:
  typedef double data_type;
};


template <class T>
class foo : public CharBase
{
  data_type y;
};

class foo<double> : public DoubleBase
{
  data_type x;
};

int main()
{
  foo<double> dFoo;
  foo<int> iFoo;

  dFoo.y;
  dFoo.x * 7;
  dFoo.x * iFoo.y;

}

