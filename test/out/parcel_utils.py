from typing import *


def encode_bool(bool):
    return "true" if bool else "false"


# TODO type hints
def decode_list(json, decode_elem):
    return [decode_elem(e) for e in json]


def encode_list(data, encode_elem):
    return [encode_elem(e) for e in data]


def encode_map(data, key_to_str, encode_elem):
    return {key_to_str(k): encode_elem(v) for k, v in data.items()}


def encode_map_as_list(data, encode_key, encode_elem):
    return [[encode_key(k), encode_elem(v)] for k, v in data.items()]


def encode_optional(data, encode_elem):
    if data is None:
        return None
    else:
        return encode_elem(data)


def decode_map(data, key_from_str, decode_elem):
    return {key_from_str(k): decode_elem(v) for k, v in data.items()}


def decode_map_from_list(data, decode_key, decode_elem):
    return {decode_key(k): decode_elem(v) for k, v in data}
