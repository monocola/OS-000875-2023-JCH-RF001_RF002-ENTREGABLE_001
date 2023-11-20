package pe.gob.servir.mensajeria.advice;

import java.util.ArrayList;
import java.util.List;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindException;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import pe.gob.servir.mensajeria.exception.ValidationException;
import pe.gob.servir.mensajeria.response.RespBase;

@ControllerAdvice
public class ControllerExceptionHandler extends ResponseEntityExceptionHandler {

	@Override
	protected ResponseEntity<Object> handleMethodArgumentNotValid(final MethodArgumentNotValidException ex,
			final HttpHeaders headers, final HttpStatus status, final WebRequest request) {
		return handleException(ex, ex.getBindingResult(), headers, request);
	}

	@Override
	protected ResponseEntity<Object> handleBindException(final BindException ex, final HttpHeaders headers,
			final HttpStatus status, final WebRequest request) {
		return handleException(ex, ex.getBindingResult(), headers, request);
	}

	private ResponseEntity<Object> handleException(final Exception ex, final BindingResult bindingResult,
			final HttpHeaders headers, final WebRequest request) {
		final List<String> errors = new ArrayList<>();
		for (final FieldError error : bindingResult.getFieldErrors()) {
			errors.add(error.getField() + ": " + error.getDefaultMessage());
		}
		for (final ObjectError error : bindingResult.getGlobalErrors()) {
			errors.add(error.getObjectName() + ": " + error.getDefaultMessage());
		}

		RespBase<Object> response = new RespBase<>();
		response.getStatus().setSuccess(Boolean.FALSE);
		response.getStatus().getError().setMessages(errors);
		response.getStatus().getError().setCode(null);
		response.getStatus().getError().setHttpCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));

		return handleExceptionInternal(ex, response, headers, HttpStatus.BAD_REQUEST, request);
	}

	@ExceptionHandler({ ValidationException.class })
	public ResponseEntity<Object> handleValidation(final ValidationException ex, final WebRequest request) {
		RespBase<Object> response = new RespBase<>();
		response.getStatus().setSuccess(Boolean.FALSE);
		response.getStatus().getError().setMessages(ex.getMessages());
		response.getStatus().getError().setCode(ex.getCode());
		response.getStatus().getError().setHttpCode(String.valueOf(ex.getHttpCode()));

		return new ResponseEntity<>(response, new HttpHeaders(), HttpStatus.resolve(ex.getHttpCode()));
	}

	@ExceptionHandler({ Exception.class })
	public ResponseEntity<Object> handleAll(final Exception ex, final WebRequest request) {
		RespBase<Object> response = new RespBase<>();
		response.getStatus().setSuccess(Boolean.FALSE);
		response.getStatus().getError().getMessages().add(ex.getMessage());
		response.getStatus().getError().setCode(null);
		response.getStatus().getError().setHttpCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));

		return new ResponseEntity<>(response, new HttpHeaders(), HttpStatus.INTERNAL_SERVER_ERROR);
	}

}
