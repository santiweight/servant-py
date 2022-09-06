from urllib import parse
from typing import *
from dataclasses import dataclass

import requests

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
        resp = requests.post(url, json=data)
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