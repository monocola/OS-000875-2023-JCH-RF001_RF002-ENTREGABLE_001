export class ResponseRequest {
    trace: Trace;
    status: Status;
    payload: any;
}

export class Trace {
    traceId?: number;
}

export class Status {
    traceId?: number;
    error: ErrorResponse;
    success: boolean;
    info: string;
}

export class ErrorResponse {
    code: number;
    httpCode: number;
    messages: string[];
}

export class Response {
  trace: Trace;
  status: Status;
  payload: Payload;
}

export class Payload {
  count: any;
  items: any[];
  flag: any;
}
