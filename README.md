
System.ReadEnvVar
=================

[![Build Status](https://secure.travis-ci.org/cdepillabout/read-env-var.svg)](http://travis-ci.org/cdepillabout/read-env-var)
[![Hackage](https://img.shields.io/hackage/v/read-env-var.svg)](https://hackage.haskell.org/package/read-env-var)
[![Stackage LTS](http://stackage.org/package/read-env-var/badge/lts)](http://stackage.org/lts/package/read-env-var)
[![Stackage Nightly](http://stackage.org/package/read-env-var/badge/nightly)](http://stackage.org/nightly/package/read-env-var)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

This Haskell module exports functions for safely reading environment variables.

## Usage

```haskell
>>> import System.ReadEnvVar (readEnvVarDef, setEnv)
>>> setEnv "TEST_ENV_VAR" "1000"

>>> readEnvDef "TEST_ENV_VAR" 5 :: IO Int
1000
>>> readEnvDef "THIS_ENV_VAR_WILL_NOT_EXIST" 5 :: IO Int
5
```

See the [module documentation](https://hackage.haskell.org/package/read-env-var/docs/System-ReadEnvVar.html)
on Hackage for many more examples of usage.
