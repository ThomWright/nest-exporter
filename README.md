# nest-exporter

## Developing

```bash
# build
stack build

# run
stack exec -- next-exporter --help

# format source files
hfmt -w
```

Useful curl command:

```bash
curl https://developer-api.nest.com/devices/thermostats \
  -L --location-trusted \
  -H "Authorization: Bearer ACCESS_TOKEN" | jq
```