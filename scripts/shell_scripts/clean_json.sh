jq 'map(
  .energyWeekdaySched = (.energyWeekdaySched // [range(12) | [range(24) | 1]]) |
  .energyWeekendSched = (.energyWeekendSched // [range(12) | [range(24) | 1]]) |
  .demandWeekdaySched = (.demandWeekdaySched // [range(12) | [range(24) | 1]]) |
  .demandWeekendSched = (.demandWeekendSched // [range(12) | [range(24) | 1]]) |
  .flatDemandMonths = (.flatDemandMonths // [range(12) | 1]) |
  .demandRateStrux = (.demandRateStrux // [{"demandRateTiers": [{"rate": 0.0}]}]) |
  .energyRateStrux = (.energyRateStrux // [{"energyRateTiers": [{"rate": 0.0}]}]) |
  .flatDemandStrux = (.flatDemandStrux // [{"flatDemandTiers": [{"rate": 0.0}]}])
)' usurdb.json > usurdb_fixed.json
