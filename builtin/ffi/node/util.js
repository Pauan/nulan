export const callback = (success, error) =>
  (err, value) => {
    if (err) {
      return error(err);
    } else {
      return success(value);
    }
  };

export const callback_null = (success, error) =>
  (err) => {
    if (err) {
      return error(err);
    } else {
      return success(null);
    }
  };
