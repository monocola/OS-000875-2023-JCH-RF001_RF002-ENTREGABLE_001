export class RequestGeneric<T> {
  trace: {
    traceId: string,
  };
  payload: T;

  constructor(payload: T) {
    this.payload = payload;
  }
}

