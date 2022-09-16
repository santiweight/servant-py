from __future__ import annotations
import json
import parcel_utils
from abc import (ABC, abstractmethod)
from dataclasses import dataclass
from pathlib import Path
from typing import *

from dataclasses import dataclass
from typing import *
from urllib import parse
import requests
@dataclass (frozen=True)
class SingleConstr:
    value : str
    def encode(self) -> dict:
        return self.value
    @classmethod
    def decode(cls, json:dict ) -> SingleConstr:
        return cls(value=json["value"])
@dataclass (frozen=True)
class Record:
    n1 : int
    n2 : int
    def encode(self) -> dict:
        return {"n1": self.n1, "n2": self.n2}
    @classmethod
    def decode(cls, json:dict ) -> Record:
        return cls(n1=json["n1"], n2=json["n2"])
@dataclass (frozen=True)
class Newtype:
    value : str
    def encode(self) -> dict:
        return self.value
    @classmethod
    def decode(cls, json:dict ) -> Newtype:
        return cls(value=json["value"])
@dataclass (frozen=True)
class ListNewtype:
    value : list[str]
    def encode(self) -> dict:
        return parcel_utils.encode_list(self.value, lambda elem: elem)
    @classmethod
    def decode(cls, json:dict ) -> ListNewtype:
        return cls(value=parcel_utils.decode_list(json["value"], lambda elem: elem))
class EitherIntNewtype(ABC):
    @abstractmethod
    def encode(self) -> dict:
        pass
    @classmethod
    def decode(cls, json:dict ) -> EitherIntNewtype:
        if json['tag'] == "LeftInt":
            return LeftInt(value=json['contents'])
        elif json['tag'] == "RightNewtype":
            return RightNewtype(value=Newtype.decode(json['contents']))
        else:
            raise
@dataclass (frozen=True)
class LeftInt(EitherIntNewtype):
    value : int
    def encode(self) -> dict:
        return {"tag": "LeftInt", "contents": self.value}
@dataclass (frozen=True)
class RightNewtype(EitherIntNewtype):
    value : Newtype
    def encode(self) -> dict:
        return {"tag": "RightNewtype", "contents": self.value.encode()}
@dataclass (frozen=True)
class Client:
    api_base : str
    def get_addparam(self, n1:Optional[int] , n2:Optional[int] ) -> int:
        url = self.api_base + "/add-param"
        params = {"n1": n1, "n2": n2}
        resp = requests.get(url, params=params)
        resp.raise_for_status()
        return resp.json()
    def post_addbody(self, data:Tuple[int,int] ) -> int:
        url = self.api_base + "/add-body"
        resp = requests.post(url, json=[data[0], data[1]])
        resp.raise_for_status()
        return resp.json()
    def get_addcapture_by_n1_by_n2(self, n1:int , n2:int ) -> int:
        url = self.api_base + "/add-capture/{n1}/{n2}".format(
n1=parse.quote(str(n1)),n2=parse.quote(str(n2)))
        resp = requests.get(url)
        resp.raise_for_status()
        return resp.json()
    def get_addheader(self, data:int , headerSomeHeader:Optional[int] ) -> int:
        url = self.api_base + "/add-header"
        headers = {"Some-Header": headerSomeHeader}
        resp = requests.get(url, headers=headers, json=data)
        resp.raise_for_status()
        return resp.json()
    def post_addall_by_n1(self, n1:int , n2:Optional[int] , data:int , headern3:Optional[int] ) -> int:
        url = self.api_base + "/add-all/{n1}".format(
n1=parse.quote(str(n1)))
        params = {"n2": n2}
        headers = {"n3": headern3}
        resp = requests.post(url, headers=headers, params=params, json=data)
        resp.raise_for_status()
        return resp.json()
    def post_addmap(self, data:dict[int,int] ) -> int:
        url = self.api_base + "/add-map"
        resp = requests.post(url, json=parcel_utils.encode_map(data, lambda k: str(k), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addmapnewtypekey(self, data:list[Tuple[Newtype,int]] ) -> int:
        url = self.api_base + "/add-map-newtype-key"
        resp = requests.post(url, json=parcel_utils.encode_map_as_list(data, lambda k: k.encode(), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addmapnewtypelistkey(self, data:list[Tuple[ListNewtype,int]] ) -> int:
        url = self.api_base + "/add-map-newtype-list-key"
        resp = requests.post(url, json=parcel_utils.encode_map_as_list(data, lambda k: k.encode(), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addmapsingleconstrkey(self, data:list[Tuple[SingleConstr,int]] ) -> int:
        url = self.api_base + "/add-map-single-constr-key"
        resp = requests.post(url, json=parcel_utils.encode_map_as_list(data, lambda k: k.encode(), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addmapsumtykey(self, data:list[Tuple[EitherIntNewtype,int]] ) -> int:
        url = self.api_base + "/add-map-sumty-key"
        resp = requests.post(url, json=parcel_utils.encode_map_as_list(data, lambda k: k.encode(), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addrecord(self, data:Record ) -> int:
        url = self.api_base + "/add-record"
        resp = requests.post(url, json=data.encode())
        resp.raise_for_status()
        return resp.json()