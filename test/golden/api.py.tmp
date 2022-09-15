from __future__ import annotations
import json
import parcel_utils
from dataclasses import dataclass
from pathlib import Path
from typing import *

from dataclasses import dataclass
from typing import *
from urllib import parse
import requests
@dataclass (frozen=True)
class Record:
    n1 : int
    n2 : int
    def encode(self) -> dict:
        return {"tag": "Record", "n1": self.n1, "n2": self.n2}
    @classmethod
    def decode(cls, json:dict ) -> Record:
        return cls(n1=json["n1"], n2=json["n2"])
    @classmethod
    def load(cls) -> Record:
        currPath = Path(__file__)
        dataDirPath = currPath.parent
        jsonPath = dataDirPath / "data/Record.json"
        with jsonPath.open() as f :
            res = json.load(f)
            return cls.decode(res)
@dataclass (frozen=True)
class Newtype:
    contents : str
    def encode(self) -> dict:
        return self.contents
    @classmethod
    def decode(cls, json:dict ) -> Newtype:
        return cls(contents=json["contents"])
    @classmethod
    def load(cls) -> Newtype:
        currPath = Path(__file__)
        dataDirPath = currPath.parent
        jsonPath = dataDirPath / "data/Newtype.json"
        with jsonPath.open() as f :
            res = json.load(f)
            return cls.decode(res)
@dataclass (frozen=True)
class ListNewtype:
    contents : list[str]
    def encode(self) -> dict:
        return self.contents
    @classmethod
    def decode(cls, json:dict ) -> ListNewtype:
        return cls(contents=parcel_utils.decode_list(json["contents"], lambda elem: elem))
    @classmethod
    def load(cls) -> ListNewtype:
        currPath = Path(__file__)
        dataDirPath = currPath.parent
        jsonPath = dataDirPath / "data/ListNewtype.json"
        with jsonPath.open() as f :
            res = json.load(f)
            return cls.decode(res)
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
    def post_addmapnonprimkey(self, data:list[Tuple[Newtype,int]] ) -> int:
        url = self.api_base + "/add-map-non-prim-key"
        resp = requests.post(url, json=parcel_utils.encode_map_as_list(data, lambda k: k.encode(), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addmapnonprimlistkey(self, data:list[Tuple[ListNewtype,int]] ) -> int:
        url = self.api_base + "/add-map-non-prim-list-key"
        resp = requests.post(url, json=parcel_utils.encode_map_as_list(data, lambda k: k.encode(), lambda v: v))
        resp.raise_for_status()
        return resp.json()
    def post_addrecord(self, data:Record ) -> int:
        url = self.api_base + "/add-record"
        resp = requests.post(url, json=data.encode())
        resp.raise_for_status()
        return resp.json()