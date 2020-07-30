import hypothesis.strategies as st
from hypothesis import given

@given(st.floats(min_value = 0, max_value=10000))
def test(f):
    print(f)
    assert f < 5000
