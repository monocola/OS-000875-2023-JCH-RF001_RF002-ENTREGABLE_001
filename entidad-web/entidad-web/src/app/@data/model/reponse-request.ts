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
}

export class ErrorResponse {
    code: number;
    httpCode: number;
    messages: string[];
}
