{-# LANGUAGE TypeFamilies, DataKinds #-}
module Database where

import Opaleye (Column, Nullable,
                         Table(Table), required, queryTable,
                         Query, (.==), aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8)

type family Field      f a b n
type family TableField f a b n req
data Hask
data Op
data Nulls
data W
data NN
data N
data Req
data Opt

type instance Field Hask h o NN = h
type instance Field Hask h o N  = Maybe h
type instance Field Op h o NN = Column o
type instance Field Op h o N  = Column (Nullable o)
type instance TableField Hask     h o n b   = Field Hask h o n
type instance TableField Op    h o n b   = Field Op h o n
type instance TableField W     h o n Req = Field Op h o n
type instance TableField W     h o n Opt = Maybe (Field Op h o n)
type instance TableField Nulls h o n b   = Column (Nullable o)
