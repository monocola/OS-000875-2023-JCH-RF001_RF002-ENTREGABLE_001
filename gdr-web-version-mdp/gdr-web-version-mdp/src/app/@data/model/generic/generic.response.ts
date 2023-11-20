export interface GenericResponse<T> {
  trace: Trace;
  status: Status;
  payload: T;
}

interface Trace {
  traceId: string;
}

interface Error {
  code: string;
  httpCode: string;
  messages: string[];
}

interface Status {
  success: boolean;
  error: Error;
}
