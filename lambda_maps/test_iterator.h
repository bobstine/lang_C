
// the following three functions check that basic functions of iterators work.
// I think every function of an iterator is called at least once.  (Or more accurately,
// everyone we implement, which hopefully is all the ones that should exist.)
template <class Iter>
inline
void
test_forward_iterator(Iter i)
{
  assert(i == i);
  Iter j = i;
  assert(i == j);
  assert((*i) == (*j));
  j++;
  assert(i != j);
  ++i;
  assert(i == j);
  assert((*i) == (*j));
}

 template <class Iter>
 inline
 void
 test_bidirectional_iterator(Iter i)
 {
   test_forward_iterator(i);
   Iter j = i;
   j++;
   i++;
   assert(i == j);
   j--;
   assert(i != j);
   --i;
   assert(i == j);
 }

 template <class Iter>
 inline
 void
 test_random_access_iterator(Iter i)
 {
   test_bidirectional_iterator(i);
   assert(!(i < i));
   assert(!(i > i));

   Iter big = i + 3;
   assert(i < big);
   assert(big > i);
   assert(i <= big);
   assert(big >= i);
   assert(big - i == 3);

   Iter j = big - 3;
   assert(i == j);
   i += 1;
   assert(i != j);
   j = j + 1;
   assert(i == j);
   i -= 1;
   assert(i != j);
   j = j - 1;
   assert(i == j);
 }
