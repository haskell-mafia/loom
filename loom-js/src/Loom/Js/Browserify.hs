{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Js.Browserify (
    Browserify
  , installBrowserify
  , browserifyVersion
  , browserifyDeps
  ) where


import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Hashable (Hashable (..))

import           Loom.Core.Data
import           Loom.Js

import           P

import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, renameDirectory)
import           System.FilePath ((</>), takeDirectory)
import           System.IO  (IO, FilePath)
import           System.IO.Temp (createTempDirectory)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either


-- | A token to be passed around when Browserify is to be run.
newtype Browserify = Browserify {
    _browserifyPath :: FilePath
  } deriving (Eq, Ord, Show)

-- | Ensure the desired version of Browserify is installed.
-- This means simply constructing the 'Browserify' token if already installed,
-- or fetching and unpacking everything.
installBrowserify :: LoomHome -> EitherT JsError IO Browserify
installBrowserify home = do
  let dir = browserifyDestination home
      bin = browserifyEntryPoint dir
      tmpdir = browserifyTempDir home
  ifM
    (liftIO (doesDirectoryExist dir))
    (pure (Browserify bin))
    (do liftIO $ createDirectoryIfMissing True tmpdir
        tmp <- liftIO $ createTempDirectory tmpdir "loom-"
        let nodes = tmp </> "node_modules"
        tars <- fetchJsNpm home browserifyDeps
        unpackJs (JsUnpackDir nodes) tars
        liftIO $ createDirectoryIfMissing True (takeDirectory dir)
        liftIO $ renameDirectory tmp dir
        pure (Browserify bin))

-- | The directory we expect browserify installed into.
browserifyDestination :: LoomHome -> FilePath
browserifyDestination home =
  loomHomeFilePath home </> "js" </> "browserify-" <> browserifyVersion

-- | The browserify entry point.
browserifyEntryPoint :: FilePath -> FilePath
browserifyEntryPoint root =
  -- TODO this does not exist
  root </> "thing"

-- | The tmpdir we use while unpacking.
browserifyTempDir :: LoomHome -> FilePath
browserifyTempDir home =
  loomHomeFilePath home </> "tmp"

-- | A version string for our JS post-processing monstrosity.
-- This is concocted from the hashes of all the hashes.
browserifyVersion :: [Char]
browserifyVersion =
  printf "%x" . hash $
      "loom" -- Replace this constant to force a version change.
    : (fmap (unSha1 . ndSha1) browserifyDeps)

browserifyDeps :: [NpmDependency]
browserifyDeps =
  fmap (\(n, v, h) -> NpmDependency (NpmPackage n) (NpmPackageVersion v) (Sha1 h)) [
      ("JSONStream"                , "1.3.1",   "707f761e01dae9e16f1bcf93703b78c70966579a")
    , ("acorn"                     , "4.0.11",  "edcda3bd937e7556410d42ed5860f67399c794c0")
    , ("align-text"                , "0.1.4",   "0cd90a561093f35d0a99256c22b7069433fad117")
    , ("array-filter"              , "0.0.1",   "7da8cf2e26628ed732803581fd21f67cacd2eeec")
    , ("array-map"                 , "0.0.0",   "88a2bab73d1cf7bcd5c1b118a003f66f665fa662")
    , ("array-reduce"              , "0.0.0",   "173899d3ffd1c7d9383e4479525dbe278cab5f2b")
    , ("asn1.js"                   , "4.9.1",   "48ba240b45a9280e94748990ba597d216617fd40")
    , ("assert"                    , "1.4.1",   "99912d591836b5a6f5b345c0f07eefc08fc65d91")
    , ("astw"                      , "2.2.0",   "7bd41784d32493987aeb239b6b4e1c57a873b917")
    , ("balanced-match"            , "0.4.2",   "cb3f3e3c732dc0f01ee70b403f302e61d7709838")
    , ("base64-js"                 , "1.2.0",   "a39992d723584811982be5e290bb6a53d86700f1")
    , ("bn.js"                     , "4.11.6",  "53344adb14617a13f6e8dd2ce28905d1c0ba3215")
    , ("brace-expansion"           , "1.1.6",   "7197d7eaa9b87e648390ea61fc66c84427420df9")
    , ("brorand"                   , "1.1.0",   "12c25efe40a45e3c323eb8675a0a0ce57b22371f")
    , ("browser-pack"              , "6.0.2",   "f86cd6cef4f5300c8e63e07a4d512f65fbff4531")
    , ("browser-resolve"           , "1.11.2",  "8ff09b0a2c421718a1051c260b32e48f442938ce")
    , ("browserify-aes"            , "1.0.6",   "5e7725dbdef1fd5930d4ebab48567ce451c48a0a")
    , ("browserify-cipher"         , "1.0.0",   "9988244874bf5ed4e28da95666dcd66ac8fc363a")
    , ("browserify-des"            , "1.0.0",   "daa277717470922ed2fe18594118a175439721dd")
    , ("browserify-rsa"            , "4.0.1",   "21e0abfaf6f2029cf2fafb133567a701d4135524")
    , ("browserify-sign"           , "4.0.4",   "aa4eb68e5d7b658baa6bf6a57e630cbd7a93d298")
    , ("browserify-zlib"           , "0.1.4",   "bb35f8a519f600e0fa6b8485241c979d0141fb2d")
    , ("browserify"                , "14.3.0",  "fd003a2386ac1aec127f097885a3cc6373b745c4")
    , ("buffer-shims"              , "1.0.0",   "9978ce317388c649ad8793028c3477ef044a8b51")
    , ("buffer-xor"                , "1.0.3",   "26e61ed1422fb70dd42e6e36729ed51d855fe8d9")
    , ("buffer"                    , "5.0.6",   "2ea669f7eec0b6eda05b08f8b5ff661b28573588")
    , ("builtin-status-codes"      , "3.0.0",   "85982878e21b98e1c66425e03d0174788f569ee8")
    , ("cached-path-relative"      , "1.0.1",   "d09c4b52800aa4c078e2dd81a869aac90d2e54e7")
    , ("camelcase"                 , "1.2.1",   "9bb5304d2e0b56698b2c758b08a3eaa9daa58a39")
    , ("center-align"              , "0.1.3",   "aa0d32629b6ee972200411cbd4461c907bc2b7ad")
    , ("cipher-base"               , "1.0.3",   "eeabf194419ce900da3018c207d212f2a6df0a07")
    , ("cliui"                     , "2.1.0",   "4b475760ff80264c762c3a1719032e91c7fea0d1")
    , ("combine-source-map"        , "0.7.2",   "0870312856b307a87cc4ac486f3a9a62aeccc09e")
    , ("concat-map"                , "0.0.1",   "d8a96bd77fd68df7793a73036a3ba0d5405d477b")
    , ("concat-stream"             , "1.5.2",   "708978624d856af41a5a741defdd261da752c266")
    , ("console-browserify"        , "1.1.0",   "f0241c45730a9fc6323b206dbf38edc741d0bb10")
    , ("constants-browserify"      , "1.0.0",   "c20b96d8c617748aaf1c16021760cd27fcb8cb75")
    , ("convert-source-map"        , "1.1.3",   "4829c877e9fe49b3161f3bf3673888e204699860")
    , ("core-util-is"              , "1.0.2",   "b5fd54220aa2bc5ab57aab7140c940754503c1a7")
    , ("create-ecdh"               , "4.0.0",   "888c723596cdf7612f6498233eebd7a35301737d")
    , ("create-hash"               , "1.1.2",   "51210062d7bb7479f6c65bb41a92208b1d61abad")
    , ("create-hmac"               , "1.1.4",   "d3fb4ba253eb8b3f56e39ea2fbcb8af747bd3170")
    , ("crypto-browserify"         , "3.11.0",  "3652a0906ab9b2a7e0c3ce66a408e957a2485522")
    , ("date-now"                  , "0.1.4",   "eaf439fd4d4848ad74e5cc7dbef200672b9e345b")
    , ("decamelize"                , "1.2.0",   "f6534d15148269b20352e7bee26f501f9a191290")
    , ("defined"                   , "1.0.0",   "c98d9bcef75674188e110969151199e39b1fa693")
    , ("deps-sort"                 , "2.0.0",   "091724902e84658260eb910748cccd1af6e21fb5")
    , ("des.js"                    , "1.0.0",   "c074d2e2aa6a8a9a07dbd61f9a15c2cd83ec8ecc")
    , ("detective"                 , "4.5.0",   "6e5a8c6b26e6c7a254b1c6b6d7490d98ec91edd1")
    , ("diffie-hellman"            , "5.0.2",   "b5835739270cfe26acf632099fded2a07f209e5e")
    , ("domain-browser"            , "1.1.7",   "867aa4b093faa05f1de08c06f4d7b21fdf8698bc")
    , ("duplexer2"                 , "0.1.4",   "8b12dab878c0d69e3e7891051662a32fc6bddcc1")
    , ("elliptic"                  , "6.4.0",   "cac9af8762c85836187003c8dfe193e5e2eae5df")
    , ("envify"                    , "4.0.0",   "f791343e3d11cc29cce41150300a8af61c66cab0")
    , ("esprima"                   , "3.1.3",   "fdca51cee6133895e3c88d535ce49dbff62a4633")
    , ("events"                    , "1.1.1",   "9ebdb7635ad099c70dcc4c2a1f5004288e8bd924")
    , ("evp_bytestokey"            , "1.0.0",   "497b66ad9fef65cd7c08a6180824ba1476b66e53")
    , ("extend"                    , "1.3.0",   "d1516fb0ff5624d2ebf9123ea1dac5a1994004f8")
    , ("fs.realpath"               , "1.0.0",   "1504ad2523158caa40db4a2787cb01411994ea4f")
    , ("function-bind"             , "1.1.0",   "16176714c801798e4e8f2cf7f7529467bb4a5771")
    , ("glob"                      , "7.1.1",   "805211df04faaf1c63a3600306cdf5ade50b2ec8")
    , ("has"                       , "1.0.1",   "8461733f538b0837c9361e39a9ab9e9704dc2f28")
    , ("hash.js"                   , "1.0.3",   "1332ff00156c0a0ffdd8236013d07b77a0451573")
    , ("hmac-drbg"                 , "1.0.0",   "3db471f45aae4a994a0688322171f51b8b91bee5")
    , ("htmlescape"                , "1.1.1",   "3a03edc2214bca3b66424a3e7959349509cb0351")
    , ("https-browserify"          , "1.0.0",   "ec06c10e0a34c0f2faf199f7fd7fc78fffd03c73")
    , ("ieee754"                   , "1.1.8",   "be33d40ac10ef1926701f6f08a2d86fbfd1ad3e4")
    , ("indexof"                   , "0.0.1",   "82dc336d232b9062179d05ab3293a66059fd435d")
    , ("inflight"                  , "1.0.6",   "49bd6331d7d02d0c09bc910a1075ba8165b56df9")
    , ("inherits"                  , "2.0.3",   "633c2c83e3da42a502f52466022480f4208261de")
    , ("inline-source-map"         , "0.6.2",   "f9393471c18a79d1724f863fa38b586370ade2a5")
    , ("insert-module-globals"     , "7.0.1",   "c03bf4e01cb086d5b5e5ace8ad0afe7889d638c3")
    , ("is-buffer"                 , "1.1.5",   "1f3b26ef613b214b88cbca23cc6c01d87961eecc")
    , ("isarray"                   , "1.0.0",   "bb935d48582cba168c06834957a54a3e07124f11")
    , ("json-stable-stringify"     , "0.0.1",   "611c23e814db375527df851193db59dd2af27f45")
    , ("jsonify"                   , "0.0.0",   "2c74b6ee41d93ca51b7b5aaee8f503631d252a73")
    , ("jsonparse"                 , "1.3.0",   "85fc245b1d9259acc6941960b905adf64e7de0e8")
    , ("kind-of"                   , "3.1.0",   "475d698a5e49ff5e53d14e3e732429dc8bf4cf47")
    , ("labeled-stream-splicer"    , "2.0.0",   "a52e1d138024c00b86b1c0c91f677918b8ae0a59")
    , ("lazy-cache"                , "1.0.4",   "a1d78fc3a50474cb80845d3b3b6e1da49a446e8e")
    , ("lexical-scope"             , "1.2.0",   "fcea5edc704a4b3a8796cdca419c3a0afaf22df4")
    , ("lodash.memoize"            , "3.0.4",   "2dcbd2c287cbc0a55cc42328bd0c736150d53e3f")
    , ("longest"                   , "1.0.1",   "30a0b2da38f73770e8294a0d22e6625ed77d0097")
    , ("miller-rabin"              , "4.0.0",   "4a62fb1d42933c05583982f4c716f6fb9e6c6d3d")
    , ("minimalistic-assert"       , "1.0.0",   "702be2dda6b37f4836bcb3f5db56641b64a1d3d3")
    , ("minimalistic-crypto-utils" , "1.0.1",   "f6c00c1c0b082246e5c4d99dfb8c7c083b2b582a")
    , ("minimatch"                 , "3.0.3",   "2a4e4090b96b2db06a9d7df01055a62a77c9b774")
    , ("minimist"                  , "1.2.0",   "a35008b20f41383eec1fb914f4cd5df79a264284")
    , ("module-deps"               , "4.1.1",   "23215833f1da13fd606ccb8087b44852dcb821fd")
    , ("once"                      , "1.4.0",   "583b1aa775961d4b113ac17d9c50baef9dd76bd1")
    , ("os-browserify"             , "0.1.2",   "49ca0293e0b19590a5f5de10c7f265a617d8fe54")
    , ("pako"                      , "0.2.9",   "f3f7522f4ef782348da8161bad9ecfd51bf83a75")
    , ("parents"                   , "1.0.1",   "fedd4d2bf193a77745fe71e371d73c3307d9c751")
    , ("parse-asn1"                , "5.1.0",   "37c4f9b7ed3ab65c74817b5f2480937fbf97c712")
    , ("path-browserify"           , "0.0.0",   "a0b870729aae214005b7d5032ec2cbbb0fb4451a")
    , ("path-is-absolute"          , "1.0.1",   "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f")
    , ("path-parse"                , "1.0.5",   "3c1adf871ea9cd6c9431b6ea2bd74a0ff055c4c1")
    , ("path-platform"             , "0.11.15", "e864217f74c36850f0852b78dc7bf7d4a5721bf2")
    , ("pbkdf2"                    , "3.0.9",   "f2c4b25a600058b3c3773c086c37dbbee1ffe693")
    , ("process-nextick-args"      , "1.0.7",   "150e20b756590ad3f91093f25a4f2ad8bff30ba3")
    , ("process"                   , "0.11.9",  "7bd5ad21aa6253e7da8682264f1e11d11c0318c1")
    , ("public-encrypt"            , "4.0.0",   "39f699f3a46560dd5ebacbca693caf7c65c18cc6")
    , ("punycode"                  , "1.3.2",   "9653a036fb7c1ee42342f2325cceefea3926c48d")
    , ("querystring-es3"           , "0.2.1",   "9ec61f79049875707d69414596fd907a4d711e73")
    , ("querystring"               , "0.2.0",   "b209849203bb25df820da756e747005878521620")
    , ("randombytes"               , "2.0.3",   "674c99760901c3c4112771a31e521dc349cc09ec")
    , ("read-only-stream"          , "2.0.0",   "2724fd6a8113d73764ac288d4386270c1dbf17f0")
    , ("readable-stream"           , "2.2.6",   "8b43aed76e71483938d12a8d46c6cf1a00b1f816")
    , ("repeat-string"             , "1.6.1",   "8dcae470e1c88abc2d600fff4a776286da75e637")
    , ("resolve"                   , "1.1.7",   "203114d82ad2c5ed9e8e0411b3932875e889e97b")
    , ("right-align"               , "0.1.3",   "61339b722fe6a3515689210d24e14c96148613ef")
    , ("ripemd160"                 , "1.0.1",   "93a4bbd4942bc574b69a8fa57c71de10ecca7d6e")
    , ("sha.js"                    , "2.4.8",   "37068c2c476b6baf402d14a49c67f597921f634f")
    , ("shasum"                    , "1.0.2",   "e7012310d8f417f4deb5712150e5678b87ae565f")
    , ("shell-quote"               , "1.6.1",   "f4781949cce402697127430ea3b3c5476f481767")
    , ("source-map"                , "0.5.6",   "75ce38f52bf0733c5a7f0c118d81334a2bb5f412")
    , ("stream-browserify"         , "2.0.1",   "66266ee5f9bdb9940a4e4514cafb43bb71e5c9db")
    , ("stream-combiner2"          , "1.1.1",   "fb4d8a1420ea362764e21ad4780397bebcb41cbe")
    , ("stream-http"               , "2.7.0",   "cec1f4e3b494bc4a81b451808970f8b20b4ed5f6")
    , ("stream-splicer"            , "2.0.0",   "1b63be438a133e4b671cc1935197600175910d83")
    , ("string_decoder"            , "0.10.31", "62e203bc41766c6c28c9fc84301dab1c5310fa94")
    , ("subarg"                    , "1.0.0",   "f62cf17581e996b48fc965699f54c06ae268b8d2")
    , ("syntax-error"              , "1.3.0",   "1ed9266c4d40be75dc55bf9bb1cb77062bb96ca1")
    , ("through2"                  , "2.0.3",   "0004569b37c7c74ba39c43f3ced78d1ad94140be")
    , ("through"                   , "2.3.8",   "0dd4c9ffaabc357960b1b724115d7e0e86a2e1f5")
    , ("timers-browserify"         , "1.4.2",   "c9c58b575be8407375cb5e2462dacee74359f41d")
    , ("to-arraybuffer"            , "1.0.1",   "7d229b1fcc637e466ca081180836a7aabff83f43")
    , ("tty-browserify"            , "0.0.0",   "a157ba402da24e9bf957f9aa69d524eed42901a6")
    , ("typedarray"                , "0.0.6",   "867ac74e3864187b1d3d47d996a78ec5c8830777")
    , ("uglify-js"                 , "2.8.21",  "1733f669ae6f82fc90c7b25ec0f5c783ee375314")
    , ("uglify-to-browserify"      , "1.0.2",   "6e0924d6bda6b5afe349e39a6d632850a0f882b7")
    , ("uglifyify"                 , "3.0.4",   "487e080a5a7798880e68e90def9b06681fb13bd2")
    , ("umd"                       , "3.0.1",   "8ae556e11011f63c2596708a8837259f01b3d60e")
    , ("url"                       , "0.11.0",  "3838e97cfc60521eb73c525a8e55bfdd9e2e28f1")
    , ("util-deprecate"            , "1.0.2",   "450d4dc9fa70de732762fbd2d4a28981419a0ccf")
    , ("util"                      , "0.10.3",  "7afb1afe50805246489e3db7fe0ed379336ac0f9")
    , ("vm-browserify"             , "0.0.4",   "5d7ea45bbef9e4a6ff65f95438e0a87c357d5a73")
    , ("window-size"               , "0.1.0",   "5438cd2ea93b202efa3a19fe8887aee7c94f9c9d")
    , ("wordwrap"                  , "0.0.2",   "b79669bb42ecb409f83d583cad52ca17eaa1643f")
    , ("wrappy"                    , "1.0.2",   "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f")
    , ("xtend"                     , "4.0.1",   "a5c6d532be656e23db820efb943a1f04998d63af")
    , ("yargs"                     , "3.10.0",  "f7ee7bd857dd7c1d2d38c0e74efbd681d1431fd1")
    ]
