rm -rf deps
rm -rf ebin
./rebar get-deps
./rebar clean compile eunit

# erl -env ERL_LIBS "..:deps"