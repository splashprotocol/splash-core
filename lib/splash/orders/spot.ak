pub type Action {
  /// Execute order
  Exec { successor_ix: Int }
  Cancel
}

pub const beacon_name = #"01"