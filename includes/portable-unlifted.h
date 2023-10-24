#ifdef false
/*
 * Copyright 2023 Gigaparsec Contributors <https://github.com/j-mie6/gigaparsec/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

// This file enables the use of `UnliftedDatatypes` from 9.2 in a portable way
// include this file at the top underneath the requisite `CPP` extension, then
// have a `CPP_import_PortableUnlifted` import; `UnliftedDatatype` is now a kind
// that can be freely used.
#endif

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE UnliftedDatatypes #-}

#define CPP_import_PortableUnlifted import GHC.Exts (TYPE, RuntimeRep(BoxedRep), Levity(Unlifted))
#define UnliftedDatatype (TYPE ('BoxedRep 'Unlifted))

#else

#define CPP_import_PortableUnlifted
#define UnliftedDatatype *

#endif
