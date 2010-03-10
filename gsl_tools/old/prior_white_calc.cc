  TestResult  old_white_f_test ()          { double drss(old_white_change_in_rss());
                                             return Stat_Utils::f_test(drss, mDimZ, mRSS-drss, df_residual()-mDimZ); }

  double old_white_change_in_rss () ;

template <class Data, class Engine>
double
gslRegression<Data,Engine>::old_white_change_in_rss() 
{
  if (mZIsSingular) return 0.0;
  // find Z'D Z
  gsl_matrix_const_view    vZ  (gsl_matrix_const_submatrix(mZResids, 0,0, mN, mDimZ));
  const gsl_matrix         *z  (&vZ.matrix);
  gsl_matrix             *zdz  (mpData->temp_mat(mDimZ, mDimZ)); 
  gsl_matrix_set_zero(zdz);
  mEngine.blas_dsyr(z, mpData->e(), zdz);
  // scale for s2 effect
  double s2 (mRSS / df_residual());
  gsl_matrix_scale(zdz, 1.0/s2);
  // scalar case avoids matrices
  if (1 == mDimZ)
  { double zz00   (gsl_matrix_get(mZZ,0,0));
    double zdz00  (gsl_matrix_get(zdz,0,0));
    if (zdz00 < 1.0e-30)
    { debug("GSLR",2) << "Warning; near singular White variance. Setting to zero.\n";
      return 0.0;
    }
    double c (zz00 * gsl_vector_get(mC,0));
    return c * c / zdz00;   // s2 imbedded in zdz
  }
  // compute (Z'Z) (Z'DZ)º (Z'Z)
  int gslError (0);  
  { gsl_error_handler_t *builtIn (gsl_set_error_handler_off());
    gslError = gsl_linalg_cholesky_decomp(zdz);
    gsl_set_error_handler(builtIn);
  }
  if (gslError) 
  { debug("GLSR",3) << " *** Error ***   Z'DZ not PSD in Cholesky decomp. Return dRSS = 0.\n";
    return 0.0;
  }
  // fill in ZZ matrix (which was lower triangular)
  gsl_matrix_const_view   vZZ  (gsl_matrix_const_submatrix(mZZ, 0,0, mDimZ, mDimZ));
  const gsl_matrix        *zz  (&vZZ.matrix);
  gsl_matrix * symZZ (gsl_matrix_alloc(mDimZ, mDimZ));
  for (int i=0; i<mDimZ; ++i)
  { gsl_matrix_set(symZZ,i,i,gsl_matrix_get(zz,i,i));
    for (int j=0; j<i; ++j)
    { double x (gsl_matrix_get(zz,i,j));
      gsl_matrix_set(symZZ,i,j,x);
      gsl_matrix_set(symZZ,j,i,x);
    }
  }
  gsl_matrix * temp  (gsl_matrix_alloc(mDimZ, mDimZ));
  gsl_matrix * temp2 (gsl_matrix_alloc(mDimZ, mDimZ));
  for (int j=0; j<mDimZ; ++j)               // (Z'D Z)º (Z'Z), one column at a time (note s2 embedded in zdz)
  { gsl_vector_const_view vzzj (gsl_matrix_const_column(symZZ,j));
    gsl_linalg_cholesky_solve (zdz, &vzzj.vector, &gsl_matrix_column(temp,j).vector);
  }  
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, symZZ, temp, 0.0, temp2); // s2 (Z'Z) (Z'D Z)º (Z'Z)
  double dSS (change_in_rss(temp2));
  gsl_matrix_free (symZZ);
  gsl_matrix_free (temp);
  gsl_matrix_free (temp2);
  return dSS;
}

template <class Data, class Engine>
double 
gslRegression<Data,Engine>::change_in_rss (gsl_matrix const* sandwich)  const 
{
  if (mZIsSingular)
  { debug("GSLR",0) << "Singularity among added predictors\n";
    return 0.0;
  }
  const gsl_matrix *zz;
  if (sandwich) zz = sandwich;
  else          zz = &(gsl_matrix_const_submatrix (mZZ,0,0,mDimZ, mDimZ)).matrix;
  if (1 == mDimZ) 
  { double c (gsl_vector_get(mC,0));
    return c * c * gsl_matrix_get(zz,0,0);
  }
  gsl_vector_const_view vC  (gsl_vector_const_subvector (mC,0,mDimZ));
  const gsl_vector      *c  (&vC.vector);
  gsl_vector_view       tmp (gsl_vector_subvector(mpData->temp_vec(0),0,mDimZ));
  gsl_blas_dsymv(CblasLower, 1.0, zz, c, 0.0, &tmp.vector);   // (Z'Z) c
  double ss;
  gsl_blas_ddot(c,&tmp.vector,&ss);                           // c'(Z'Z)c
  return ss;
}
