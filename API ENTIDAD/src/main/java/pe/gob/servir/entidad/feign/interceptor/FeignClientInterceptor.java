package pe.gob.servir.entidad.feign.interceptor;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;

import feign.RequestInterceptor;
import feign.RequestTemplate;

@Component
public class FeignClientInterceptor implements RequestInterceptor {

	@Autowired
	private HttpServletRequest request;

	@Override
	public void apply(RequestTemplate template) {
		Object authorizationHeader = request.getAttribute("authorizationHeader");
		if (authorizationHeader != null) {
			template.header(HttpHeaders.AUTHORIZATION, (String) authorizationHeader);
		}
	}

}
