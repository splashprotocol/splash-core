use aiken/bytearray
use aiken/list
use aiken/string
use splash/plutus.{Asset}

// Generate some tokens policies:
pub fn generate_assets(seed: String, name_seed: String, num: Int) -> List<Asset> {
  list.map(
    list.range(0, num - 1),
    fn(x) {
      Asset {
        policy: bytearray.from_string(string.concat(seed, string.from_int(x))),
        name: bytearray.from_string(
          string.concat(name_seed, string.from_int(x)),
        ),
      }
    },
  )
}

test test_generate_tradable_assets() {
  let policies = generate_assets(@"some_pool_token", @"some_pool_token_name", 3)
  list.length(policies) == 3 && list.length(list.unique(policies)) == 3
}
