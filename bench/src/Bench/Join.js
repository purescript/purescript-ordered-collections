let lastResult;

export const retainResult = (operation) => (unit) => {
  lastResult = operation(unit);
  return lastResult;
};

export const clearResult = () => {
  lastResult = undefined;
};
