-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

module Thrift.Types where

import Data.Foldable (foldl')
import Data.Hashable ( Hashable, hashWithSalt )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector

instance (Hashable k, Hashable v) => Hashable (Map.HashMap k v) where
  hashWithSalt salt = foldl' hashWithSalt salt . Map.toList

instance (Hashable a) => Hashable (Set.HashSet a) where
  hashWithSalt salt = foldl' hashWithSalt salt

instance (Hashable a) => Hashable (Vector.Vector a) where
  hashWithSalt salt = Vector.foldl' hashWithSalt salt
