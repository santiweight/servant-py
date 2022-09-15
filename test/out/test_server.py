import sys
import api
import parcel_utils

port = int(sys.argv[1])

# TODO there are two inappropriate str calls here, because headers are handled a little wrong
client = api.Client("http://localhost:" + str(port))
assert client.get_addparam(2, 3) == 5
assert client.get_addparam(None, 3) == -1
assert client.post_addbody((2, 3)) == 5
assert client.get_addcapture_by_n1_by_n2(2, 3) == 5
assert client.get_addheader(2, str(3)) == 5
assert client.get_addheader(2, None) == -1
assert client.post_addall_by_n1(1, 2, 3, str(4)) == 10
assert client.post_addall_by_n1(1, None, 3, None) == 4
assert client.post_addmap({1: 2, 3: 4}) == 10
assert client.post_addmapnonprimkey([(api.Newtype("foo"), 1), (api.Newtype("bar"), 2)]) == 3
assert client.post_addmapnonprimlistkey([(api.Newtype(["foo"]), 1), (api.Newtype(["bar"]), 2)]) == 3
assert client.post_addrecord(api.Record(1,2)) == 3