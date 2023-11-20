export class ResponseGeneric<T> {
  trace: {
    traceId: string,
  };
  status: {
    success: boolean,
    error: {
      code: string,
      httpcCode: string,
      messages: string[]
    }
  };
  payload: T;

  constructor(payload: T) {
    this.payload = payload;
  }
}
