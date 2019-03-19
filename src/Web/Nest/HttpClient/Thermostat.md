# Root API data

Nested structures available as paths e.g. `/devices/thermostats/peyiJNo0IldT2YlIVtYaGQ`.

Example:

```json
{
  "devices": {
    "thermostats": {
      "peyiJNo0IldT2YlIVtYaGQ": {
        "device_id" : "peyiJNo0IldT2YlIVtYaGQ",
        "locale" : "en-US",
        "software_version" : "4.0",
        "structure_id" : "VqFabWH21nwVyd4RWgJgNb292wa7hG_dUwo2i2SG7j3-BOLY0BA4sw",
        "name" : "Hallway (upstairs)",
        "name_long" : "Hallway Thermostat (upstairs)",
        "last_connection" : "2016-10-31T23:59:59.000Z",
        "is_online" : true,
        "can_cool" : true,
        "can_heat" : true,
        "is_using_emergency_heat" : true,
        "has_fan" : true,
        "fan_timer_active" : true,
        "fan_timer_timeout" : "2016-10-31T23:59:59.000Z",
        "has_leaf" : true,
        "temperature_scale" : "C",
        "target_temperature_f" : 72,
        "target_temperature_c" : 21.5,
        "target_temperature_high_f" : 80,
        "target_temperature_high_c" : 24.5,
        "target_temperature_low_f" : 65,
        "target_temperature_low_c" : 19.5,
        "eco_temperature_high_f" : 80,
        "eco_temperature_high_c" : 24.5,
        "eco_temperature_low_f" : 65,
        "eco_temperature_low_c" : 19.5,
        "away_temperature_high_f" : 80,
        "away_temperature_high_c" : 24.5,
        "away_temperature_low_f" : 65,
        "away_temperature_low_c" : 19.5,
        "hvac_mode" : "heat",
        "previous_hvac_mode" : "heat",
        "ambient_temperature_f" : 72,
        "ambient_temperature_c" : 21.5,
        "humidity" : 40,
        "hvac_state" : "heating",
        "where_id" : "UNCBGUnN24...",
        "is_locked" : true,
        "locked_temp_min_f" : 65,
        "locked_temp_max_f" : 80,
        "locked_temp_min_c" : 19.5,
        "locked_temp_max_c" : 24.5,
        "label" : "Pat's room",
        "where_name" : "Hallway",
        "sunlight_correction_enabled" : true,
        "sunlight_correction_active" : true,
        "fan_timer_duration" : "15",
        "time_to_target" : "~15",
        "time_to_target_training" : "training"
      }
    }
  },
  "structures": {
    "VqFabWH21nwVyd4RWgJgNb292wa7hG_dUwo2i2SG7j3-BOLY0BA4sw": {
        "structure_id" : "VqFabWH21nwVyd4RWgJgNb292wa7hG_dUwo2i2SG7j3-BOLY0BA4sw",
        "thermostats" : ["peyiJNo0IldT2YlIVtYaGQ", ...],
        "smoke_co_alarms" : ["RTMTKxsQTCxzVcsySOHPxKoF4OyCifrs", ...],
        "cameras" : ["awJo6r...", ...],
        "away" : "home",
        "name" : "Home",
        "country_code" : "US",
        // "postal_code" : "94304",
        "peak_period_start_time" : "2016-10-31T23:59:59.000Z",
        "peak_period_end_time" : "2016-10-31T23:59:59.000Z",
        "time_zone" : "America/Los_Angeles",
        // "eta": {
        // },
        // "eta_begin" : "2016-08-04T13:21:37-07:00",
        // "co_alarm_state" : "ok",
        // "smoke_alarm_state" : "ok",
        // "rhr_enrollment" : true,
        // "wwn_security_state" : "ok",
        "wheres": {
            "Fqp6wJI...": {
                "where_id" : "Fqp6wJI...",
                "name" : "Bedroom"
            }
        }
    }

}
}
```