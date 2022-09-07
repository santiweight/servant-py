import sys
import api

port = int(sys.argv[1])

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
assert client.post_addrecord(api.Record(1,2)) == 3