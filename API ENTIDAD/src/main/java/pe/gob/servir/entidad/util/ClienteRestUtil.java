package pe.gob.servir.entidad.util;

import java.net.HttpURLConnection;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.response.RespBase;

@Getter
@Setter
@Service
public class ClienteRestUtil {
		
	private String serverUri;
    private String token;
    
	public void init(String serverUri,String token) {
        this.serverUri = serverUri;
        this.token = token;
    }
    
	public <T> RespBase<T> serviceGET(String endpoint, Map<String, Object> parametros,Class<T> clase) throws Exception
	{
		if (serverUri == null) {
            throw new RuntimeException("service url is null");
        }
		String url = serverUri+endpoint;
	    HttpHeaders headers = new HttpHeaders();
	    headers.setContentType(MediaType.APPLICATION_JSON);
    	headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + new String(token));
    	HttpEntity<?> entity = new HttpEntity<Object>(headers);    	    	
    	UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(url);
    	if (null != parametros && !parametros.isEmpty()) {
		    for (String key : parametros.keySet()) {
		    	builder.queryParam(key, parametros.get(key));            	
		    }
		}    	
	    RestTemplate restTemplate = new RestTemplate();	    
	    String responseJSON = restTemplate.exchange(builder.toUriString(),HttpMethod.GET,entity,String.class).getBody();  
	    RespBase<T> respuestServicio = parseRespBase(responseJSON,clase);  
	    return respuestServicio;
	}
	
	 public <T> RespBase<T> servicePOST(String endpoint, String jsonBody, Class<T> clase) { 
		if (serverUri == null) {
	        throw new RuntimeException("service url is null");
	    }
		String url = serverUri+endpoint;
	    HttpHeaders headers = new HttpHeaders();
	    headers.setContentType(MediaType.APPLICATION_JSON);
    	headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + new String(token));
    	HttpEntity<String> entity = new HttpEntity<String>(jsonBody,headers);  
    	RestTemplate restTemplate = new RestTemplate();	
    	String responseJSON = restTemplate.postForObject(url,entity,String.class);
    	RespBase<T> respuestaServicio = null;
    	if(clase != null) {
        	respuestaServicio = parseRespBase(responseJSON,clase);      		
    	}else {
        	respuestaServicio = parseRespBase(responseJSON);      		
    	}
		return respuestaServicio;
	 }
	
	@SuppressWarnings("unchecked")
	private <T> RespBase<T>  parseRespBase(String responseJSON){
		RespBase<T> respuestServicio  = new RespBase<T>();  
    	try {
    		respuestServicio = JsonUtil.convertirCadenaJsonPostAObjeto(responseJSON,RespBase.class);    		
		} catch (Exception e) {
			respuestServicio.getStatus().setSuccess(Boolean.FALSE);
			respuestServicio.getStatus().getError().setHttpCode(String.valueOf(HttpURLConnection.HTTP_UNAVAILABLE));
		}
    	return respuestServicio;
    }
	
	@SuppressWarnings("unchecked")
	private <T> RespBase<T>  parseRespBase(String responseJSON, Class<T> clase){	
        RespBase<T> respuestServicio = new RespBase<T>();
		try {
			respuestServicio = JsonUtil.convertirCadenaJsonPostAObjeto(responseJSON,RespBase.class);
			String payLoadString = JsonUtil.convertirObjetoACadenaJson(respuestServicio.getPayload());
			respuestServicio.setPayload(JsonUtil.convertirCadenaJsonPostAObjeto(payLoadString,clase)); 
		} catch (Exception e) {
			respuestServicio.getStatus().setSuccess(Boolean.FALSE);
			respuestServicio.getStatus().getError().setHttpCode(String.valueOf(HttpURLConnection.HTTP_UNAVAILABLE));
		}
		return respuestServicio;
    	
    }
	
}
