from typing import *

def encode_bool(bool):
  return "true" if bool else "false"

# TODO type hints
def decode_list(json, decode_elem):
    res = list()
    for e in json:
        res.append(decode_elem(e))
    return res


def encode_list(data, encode_elem):
    res = list()
    for e in data:
        res.append(encode_elem(e))
    return res


def encode_map(data, key_to_str, encode_elem):
    res = dict()
    for k, v in data.items():
        res[key_to_str(k)] = encode_elem(v)
    return res


def encode_map_as_list(data, encode_key, encode_elem):
    res = list()
    for k, v in data.items():
        res.append([encode_key(k), encode_elem(v)])
    return res


def encode_optional(data, encode_elem):
    if data is None:
        return None
    else:
        return encode_elem(data)


def decode_map(data, key_from_str, decode_elem):
    res = dict()
    for k, v in data.items():
        res[key_from_str(k)] = decode_elem(v)
    return res


def decode_map_from_list(data, decode_key, decode_elem):
    res = dict()
    for k, v in data:
        res[decode_key(k)] = decode_elem(v)
    return res
