package pe.gob.servir.entidad.interceptor;

import java.io.IOException;
import java.lang.reflect.Method;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jboss.resteasy.jose.jws.JWSInput;
import org.jboss.resteasy.jose.jws.crypto.RSAProvider;
import org.jboss.resteasy.spi.ResteasyProviderFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import pe.gob.servir.entidad.common.ErrorAuthentication;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.security.BasicAuthentication;
import pe.gob.servir.entidad.security.FreeAuthentication;
import pe.gob.servir.entidad.security.MyJsonWebToken;

@Component
public class AuthenticationInterceptor extends HandlerInterceptorAdapter {

	private static final Logger logger = LoggerFactory.getLogger(AuthenticationInterceptor.class);

	private static final String BASIC = "Basic";
    private static final String BEARER = "Bearer";
    @Value("${rsa.public.key}")
	private String RSA_PUBLIC_KEY;

	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
			throws Exception {
		if (!(handler instanceof HandlerMethod)) {
            return true;
        }
        try {
            // Se obtiene el metodo HTTP y el path
            String httpMethod = request.getMethod();
            String path = request.getRequestURI().replace(request.getContextPath(), "");
            logger.info(httpMethod + " " + path);
            // Swagger endpoint
			if (HttpMethod.GET.matches(httpMethod)
					&& (path.startsWith("/swagger-ui.html") || path.startsWith("/v3/api-docs"))) {				
				return true;
			}

            HandlerMethod handlerMethod = (HandlerMethod) handler;
            Method method = handlerMethod.getMethod();
            // Sin autenticacion
            if (method.isAnnotationPresent(FreeAuthentication.class)) {
                return true;
            }
            // Autenticacion Basic
            if (method.isAnnotationPresent(BasicAuthentication.class)) {
                basicAuthentication(request);
            // Autenticacion Bearer
            } else {
                bearerAuthentication(request);
            }

        } catch (ValidationException e) {
        	logger.error(e.getMessages().toString(), e);
        	throw e;
            
        } catch (Exception e) {
        	logger.error(e.getMessage(), e);
        	throw e;
        }
		return true;
	}

	private void basicAuthentication(HttpServletRequest request) throws ValidationException {
		// Se obtiene el header Authorization
		String authorizationHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
		// Si el header es nulo o no es Basic, se devuelve status 401
		if (authorizationHeader == null || !authorizationHeader.startsWith(BASIC)) {
			ValidationException e = new ValidationException(ErrorAuthentication.BASIC_AUTH_NULO.getCodigo(), 401);
			e.add(ErrorAuthentication.BASIC_AUTH_NULO.getMensaje());
			throw e;
		}
	}

	@SuppressWarnings("rawtypes")
	private void bearerAuthentication(HttpServletRequest request) throws ValidationException, IOException {
        // Se obtiene el header Authorization
        String authorizationHeader = request.getHeader(HttpHeaders.AUTHORIZATION);
        // Si el header es nulo o no es Bearer, se devuelve status 401
        if (authorizationHeader == null || !authorizationHeader.startsWith(BEARER)) {
            ValidationException e = new ValidationException(ErrorAuthentication.BEARER_AUTH_NULO.getCodigo(), 401);
            e.add(ErrorAuthentication.BEARER_AUTH_NULO.getMensaje());
            throw e;
        }
        // Se obtiene el jsonWebToken
        final String jsonWebToken = authorizationHeader.replaceFirst(BEARER + " ", "");
        JWSInput jwsInput = new JWSInput(jsonWebToken, ResteasyProviderFactory.getInstance());
        // Si el jsonWebToken no es valido, se devuelve status 401
        if (!RSAProvider.verify(jwsInput, getRSAPublicKey())) {
            ValidationException e = new ValidationException(ErrorAuthentication.JWT_INVALIDO.getCodigo(), 401);
            e.add(ErrorAuthentication.JWT_INVALIDO.getMensaje());
            throw e;
        }
        // Se obtiene el jsonWebToken payload
        String jwtPayload = jwsInput.readContent(String.class);
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        MyJsonWebToken jwt = null;
        jwt = mapper.readValue(jwtPayload, MyJsonWebToken.class);

        // Se evalua expiracion
        Map pathVariables = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE); 
        String access= (String) pathVariables.get("access");
        if (!"private".equals(access) && jwt.isExpired()) {
            ValidationException e = new ValidationException(ErrorAuthentication.JWT_EXPIRADO.getCodigo(), 401);
            e.add(ErrorAuthentication.JWT_EXPIRADO.getMensaje());
            throw e;
        }
        // Se asigna el jwt
        request.setAttribute("jwt", jwt);
        request.setAttribute("authorizationHeader", authorizationHeader);
    }

	private PublicKey getRSAPublicKey() {
		try {
			byte[] encodedKey = Base64.getDecoder().decode(RSA_PUBLIC_KEY.getBytes());
			X509EncodedKeySpec keySpec = new X509EncodedKeySpec(encodedKey);
			KeyFactory keyFactory = KeyFactory.getInstance("RSA");
			return keyFactory.generatePublic(keySpec);

		} catch (Exception e) {
			return null;
		}
	}
}
