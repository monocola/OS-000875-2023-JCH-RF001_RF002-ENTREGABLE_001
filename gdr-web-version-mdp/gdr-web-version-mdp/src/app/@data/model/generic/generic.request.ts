export class GenericRequest<T> {
  trace: Trace;
  payload: T;

  constructor(payload: T) {
    this.trace = new Trace();
    this.payload = payload;
  }
}

class Trace {
  traceId: string;
}
