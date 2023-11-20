package pe.gob.servir.mensajeria.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import pe.gob.servir.mensajeria.audit.AuditEndpointInterceptor;
import pe.gob.servir.mensajeria.interceptor.AuthenticationInterceptor;

@Configuration
public class InterceptorConfiguration implements WebMvcConfigurer {

	@Autowired
	AuthenticationInterceptor authenticationInterceptor;
	@Autowired
	AuditEndpointInterceptor auditEndpointInterceptor;

	@Override
	public void addInterceptors(InterceptorRegistry registry) {
		registry.addInterceptor(authenticationInterceptor);
		registry.addInterceptor(auditEndpointInterceptor);
	}
}
