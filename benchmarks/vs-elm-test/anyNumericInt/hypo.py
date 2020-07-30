import hypothesis.strategies as st
from hypothesis import given

@given(st.integers(min_value = 0, max_value=10000))
def test(i):
    print(i)
    assert f < 5000
