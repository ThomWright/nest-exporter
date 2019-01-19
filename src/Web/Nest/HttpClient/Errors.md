# Errors

From the Nest Api docs:

Example:

```
{
  "error": "Temperature '$temp' is in wrong format",
  "type": "https://developers.nest.com/documentation/cloud/error-messages#format-error",
  "message": "Temperature '$temp' is in wrong format",
  "instance": "31441a94-ed26-11e4-90ec-1681e6b88ec1",
  "details": {
    "field_name": "$temp"
  }
}
```

All error messages contain the error, type, message, and instance fields. The details object is optional.

- `error` Short error message format.
- `type` Provides a URL to detailed information about the error condition (this page).
- `message` Long error message format that may use variables to provide additional details. When a variable is included in the message, it will appear in the details object.
- `instance` A text string that holds an error identifier that is unique to each individual call. We may ask you for the instance number if you report an issue with the service.
- `details` Optional. Contains variables that are inserted into the message. Messages can contain multiple variables.

