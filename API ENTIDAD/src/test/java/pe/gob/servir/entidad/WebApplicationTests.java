package pe.gob.servir.entidad;

import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
public class WebApplicationTests {
	
	
//	@Autowired
//	ClienteRestUtil clienteRestUtil;
//	
//	@Autowired
//	NotificacionService notificacionService;
//		
//	@Autowired
//	SolicitudPersonaRepository solicitudPersonaRepository;
//	
//	@Autowired
//	private HttpServletRequest httpServletRequest;
//	
//	@Autowired
//	private PersonaApiClient personaApiClient;
//			
//	@Autowired
//	private SeguridadApiClient seguridadApiClient;
//	
//	@Autowired
//	private MaestraApiClient maestraApiClient;
//		
//	@Autowired
//	private VariablesSistema variablesSistema;
//	
//	@Autowired
//	private GeneralService generalService;
//	
//	@Autowired
//	private OrganigramaService organigramaService;
	
//	@Test
//	public void contextLoads() throws Exception {
		//clienteRestUtil.init("http://10.240.132.31:8080/persona/api/private/", "eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIn0.ew0KICAidXN1YXJpbyIgOiB7DQogICAgInVzdWFyaW9JZCIgOiA0MTIsDQogICAgInVzdWFyaW8iIDogIjEyMzQ1Njc3Nzg1MiIsDQogICAgImNvcnJlb0VsZWN0cm9uaWNvIiA6ICJDRi5DQU5WSUEuUkUxQFlPUE1BSUwuQ09NIg0KICB9LA0KICAiYXBsaWNhY2lvbiIgOiB7DQogICAgImFwbGljYWNpb25JZCIgOiAxLA0KICAgICJub21icmUiIDogIkNBUEFDSVRBKyIsDQogICAgIm11bHRpc2VzaW9uIiA6ICJTIg0KICB9LA0KICAicHJuIiA6ICIxMjM0NTY3Nzc4NTIiLA0KICAiZXhwIiA6IDE1OTk4OTExOTQsDQogICJqdGkiIDogImMzZTI0YzY5LTlhMWQtNDZiYy04Y2FlLTIyMDIzNDg3ODE3ZCIsDQogICJpYXQiIDogMTU5OTg4Mzk5NA0KfQ.ZgYHoXyJOklUB0oHA-5lnQb0J7Mv1ImIsNQLCulfPgyXXnXxmQEO20XvjdWqlEAsphuobhQdt8SG7COgZ7b_2A");
		//clienteRestUtil.init("http://10.240.132.31:8080/maestra/api/private/", "eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIn0.ew0KICAidXN1YXJpbyIgOiB7DQogICAgInVzdWFyaW9JZCIgOiA0MTIsDQogICAgInVzdWFyaW8iIDogIjEyMzQ1Njc3Nzg1MiIsDQogICAgImNvcnJlb0VsZWN0cm9uaWNvIiA6ICJDRi5DQU5WSUEuUkUxQFlPUE1BSUwuQ09NIg0KICB9LA0KICAiYXBsaWNhY2lvbiIgOiB7DQogICAgImFwbGljYWNpb25JZCIgOiAxLA0KICAgICJub21icmUiIDogIkNBUEFDSVRBKyIsDQogICAgIm11bHRpc2VzaW9uIiA6ICJTIg0KICB9LA0KICAicHJuIiA6ICIxMjM0NTY3Nzc4NTIiLA0KICAiZXhwIiA6IDE1OTk5NDgxOTcsDQogICJqdGkiIDogImZlNTAwODZmLTc4Y2EtNGI3ZC05ZTE5LTM3NTliNzllNWY2OSIsDQogICJpYXQiIDogMTU5OTk0MDk5Nw0KfQ.S6Wd5Izb9SiQWgcbjUGFmlp4xqfwsDKrJq_xBSfGYasdnG82djBYTb3D_98NxJ5JhAf1-UaqeLNTYl15vD31vQ");
		//		Map<String,Object> mapParametro = new HashMap<String, Object>();
//		mapParametro.put("tipoDocumento", 6);
//		mapParametro.put("numeroDocumento", "20211614545");
//		RespBase<ApiPersonaResponseDTO> response = new RespBase<ApiPersonaResponseDTO>();
//		response = clienteRestUtil.serviceGET("v1/personas/documentoQuery", mapParametro, ApiPersonaResponseDTO.class);
//		System.out.println("HOLA");
//		System.out.println(response);	
//		Map<String, Object> parametros = new HashMap<>();
//		parametros.put("NOMBRE_SISTEMA", "ENTIDAD");
//		parametros.put("URL_SISTEMA", "WWW.GOOGLE.COM");
//		parametros.put("USUARIO_LOGIN", "71081263");
//        parametros.put("USUARIO_PASSWORD", "122345");
//        parametros.put("CORREO_FOOTER", "TODO OK");
//		
//		RespBase<ApiEnvioCorreoDTO> envioCorreoResquest = new RespBase<>();
//		ApiEnvioCorreoDTO envioCorreo = new ApiEnvioCorreoDTO();
//		envioCorreo.setAsunto("Bienvenido al Sistema");
//		envioCorreo.setCodigoPlantilla("CREA_USUA");
//		List<String> correosEnvio = new ArrayList<String>();
//		correosEnvio.add("rosinaldo.santur@gmail.com");
//		correosEnvio.add("rosi_8_1993@hotmail.com");
//		envioCorreo.setEnviarTo(correosEnvio.toString());
//        envioCorreo.setJsonParametros(JsonUtil.convertirObjetoACadenaJson(parametros));
//        envioCorreoResquest.setPayload(envioCorreo);        
//        RespBase<Object> responseWS = clienteRestUtil.servicePOST("v1/email/enviar", JsonUtil.convertirObjetoACadenaJson(envioCorreoResquest), null);
//        System.out.println(responseWS);
//		httpServletRequest.setAttribute("authorizationHeader", "Bearer eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIn0.ew0KICAidXN1YXJpbyIgOiB7DQogICAgInVzdWFyaW9JZCIgOiA1MDYsDQogICAgInVzdWFyaW8iIDogIjk4NzUxMTUwMDAwMCIsDQogICAgImNvcnJlb0VsZWN0cm9uaWNvIiA6ICJjZi5jYW52aWEuZGVsZWdhZG8zM0B5b3BtYWlsLmNvbSINCiAgfSwNCiAgImFwbGljYWNpb24iIDogew0KICAgICJhcGxpY2FjaW9uSWQiIDogMSwNCiAgICAibm9tYnJlIiA6ICJDQVBBQ0lUQSsiLA0KICAgICJtdWx0aXNlc2lvbiIgOiAiUyINCiAgfSwNCiAgImVudGlkYWQiIDogeyB9LA0KICAiaWF0IiA6IDE2MDEwMjAyNjcsDQogICJleHAiIDogMTYwMTAyNzQ2NywNCiAgInBybiIgOiAiOTg3NTExNTAwMDAwIiwNCiAgImp0aSIgOiAiZjAxNzhkNTktMzM5Ni00YTJmLTkzNGUtYTE2ZDE4YTY1NDk3Ig0KfQ.Ks7npaO0HZj-uuKHXoIj_1YkoTWV3ELlobMTFj8Pg1NTKqbwHmBWMfkqdUZPQBnPcHBRcYL-3CMxG2MskZt2iA");
		//ApiSeguridadRequestDTO api = new ApiSeguridadRequestDTO("33242343232", "kjfskdfddfres@gmail.com", 235, "INS_US_CAP");
		//RespBase<ApiSeguridadRequestDTO> request = new RespBase<>();
		//request.setPayload(api);
//		try {
		//RespBase<RespApiPersona> response = personaApiClient.obtienePersonaPorDocumento(6,"20563223457");
//		RespBase<RespApiObtenerUsuario> responseWS = seguridadApiClient.buscarUsuariosByFiltro("741474444444",null,null,null);//(144);//(request);
		//enviarCorreoCrearSolicitud(Long.parseLong("42"),Constantes.PLANTILLA_CREAR_SOLICITUD);
		//RespBase<ApiPersonaResponseDTO> responseWS = personaApiClient.obtienePersonaPorDocumento(6, "20107798049");
			//RespBase<RespObtieneLista> listaParametros = maestraApiClient.obtieneParametros(Constantes.CODIGO_PARAMETRO_OBSERVACION);
//			System.out.println(variablesSistema.URL_SISTEMA);
//		} catch (Exception e) {
//			System.out.println(e.getMessage());
//		}
//	}
	
//	public void enviarCorreoCrearSolicitud(Long solicitudEntidadId,String plantilla){		
//		Map<String, Object> parametrosCorreo = new HashMap<String, Object>();
//		List<String> correosEnvio = new ArrayList<String>();
//		SolicitudPersona solicitudPersona = new SolicitudPersona();
//		solicitudPersona.setSolicitudEntidadId(solicitudEntidadId);
//		Example<SolicitudPersona> example = Example.of(solicitudPersona);
//		List<SolicitudPersona> listaPersonas = solicitudPersonaRepository.findAll(example);
//		for (SolicitudPersona persona : listaPersonas) {
//			if(!Strings.isEmpty(persona.getCorreoPrincipal()) && persona.getCorreoPrincipal() != null){
//				correosEnvio.add(persona.getCorreoPrincipal());				
//			}
//		}
//		correosEnvio.add("rosinaldo.santur@gmail.com");
//		if(correosEnvio != null && correosEnvio.size() > 0) {
//			parametrosCorreo.put(Constantes.CORREOS_ENVIO, correosEnvio);
//			Map<String, Object> parametrosPlantilla = new HashMap<String, Object>();
//			parametrosCorreo.put(Constantes.PARAMETROS, parametrosPlantilla);
//			notificacionService.enviarNotificacion(Constantes.ASUNTO_CREAR_SOLICITUD, plantilla, parametrosCorreo, false);
//		}
//	}
	
//	@Test
//	public void archivos()  {
//		URL ruta = httpServletRequest.getServletContext().getResource("/formatos/Organo.xlsx");
//		System.out.println(ruta);
//		httpServletRequest.setAttribute("authorizationHeader", "Bearer eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIn0.ew0KICAidXN1YXJpbyIgOiB7DQogICAgInVzdWFyaW9JZCIgOiA1MDYsDQogICAgInVzdWFyaW8iIDogIjk4NzUxMTUwMDAwMCIsDQogICAgImNvcnJlb0VsZWN0cm9uaWNvIiA6ICJjZi5jYW52aWEuZGVsZWdhZG8zM0B5b3BtYWlsLmNvbSINCiAgfSwNCiAgImFwbGljYWNpb24iIDogew0KICAgICJhcGxpY2FjaW9uSWQiIDogMSwNCiAgICAibm9tYnJlIiA6ICJDQVBBQ0lUQSsiLA0KICAgICJtdWx0aXNlc2lvbiIgOiAiUyINCiAgfSwNCiAgImVudGlkYWQiIDogeyB9LA0KICAiaWF0IiA6IDE2MDEwMjAyNjcsDQogICJleHAiIDogMTYwMTAyNzQ2NywNCiAgInBybiIgOiAiOTg3NTExNTAwMDAwIiwNCiAgImp0aSIgOiAiZjAxNzhkNTktMzM5Ni00YTJmLTkzNGUtYTE2ZDE4YTY1NDk3Ig0KfQ.Ks7npaO0HZj-uuKHXoIj_1YkoTWV3ELlobMTFj8Pg1NTKqbwHmBWMfkqdUZPQBnPcHBRcYL-3CMxG2MskZt2iA");
//		
//			String rutaA = httpServletRequest.getServletContext().getRealPath("/formatos/Organo.xlsx");
//			System.out.println(rutaA);
//			HttpServletResponse response = null;
//			String headerKey = "Content-Disposition";
//	        String headerValue = "attachment; filename=/formatos/Organo.xlsx";
//	        response.setHeader(headerKey, headerValue);
//	        System.out.println(response);
//			URL ruta = httpServletRequest.getServletContext().getResource("/formatos/Organo.xlsx");
//			try(InputStream uploadedInputStream = new FileInputStream((new File(rutaA)))){
//				Map<String,Object> parametroMap  = new  HashMap<String, Object>();
//				parametroMap.put("entidadId", 47);
//				RespBase<RespParametro> listaEstado = generalService.comboEstado();
//				RespBase<RespParametro> listaNivel = generalService.comboNivel();
//				RespBase<RespParametro> listaNaturaleza = generalService.comboNaturalezaOrgano();
//				RespBase<RespParametro> listaTipoDoc = generalService.comboTipoDocumento();
//				RespBase<RespObtenerOrganigrama> listaOrgano = organigramaService.buscarOrganigramaByFilter(parametroMap);
//				listaTipoDoc = ParametrosUtil.listaTipoDNI_CE(listaTipoDoc.getPayload().getListaParametros());				
//				
//				List<Generico> listaEstadoGen= ParametrosUtil.convertirEstadoAGenerico(listaEstado.getPayload().getListaParametros());
//				List<Generico> listaNivelGen= ParametrosUtil.convertirNivelAGenerico(listaNivel.getPayload().getListaParametros());
//				List<Generico> listaNaturalezaGen= ParametrosUtil.convertirNaturalezaAGenerico(listaNaturaleza.getPayload().getListaParametros());
//				List<Generico> listaTipoDocGen= ParametrosUtil.convertirTipoDocumentoAGenerico(listaTipoDoc.getPayload().getListaParametros());
//				List<Generico> listaOrganoGen= ParametrosUtil.convertirOrganoAGenerico(listaOrgano.getPayload().getListaOrganigrama());
//				
//				Map<String,Object> mapaLista = new LinkedHashMap<>();			
//				mapaLista.put("ESTADO", listaEstadoGen);						
//				mapaLista.put("NIVEL", listaNivelGen);
//				mapaLista.put("NATURALEZA", listaNaturalezaGen);
//				mapaLista.put("ORGANO", listaOrganoGen);
//				mapaLista.put("TIPOS DOCUMENTOS",listaTipoDocGen);
//				
//				byte[] arrayExcel =  ExcelUtil.updateDropDownXLSX(uploadedInputStream,mapaLista);
//				RespBase<Object> response = new RespBase<>();
//				response.getStatus().setSuccess(Boolean.TRUE);
//				response.setPayload(Base64.getEncoder().encodeToString(arrayExcel));
//				System.out.println(response.getPayload());
//			}catch (Exception e) {
//				System.out.println(e.getMessage());
//			}
//		}
		
		
//		InputStream rutaB = httpServletRequest.getServletContext().getResourceAsStream("/formatos/Organo.xlsx");
//		System.out.println(rutaB.toString());
//	}
	

//	@Test
//	public void StringNulo()  {
//		String nulo = null;
//		
//			System.out.println("------"+StringUtils.equals(nulo, null));
//		
//		
//		
//	}
}


