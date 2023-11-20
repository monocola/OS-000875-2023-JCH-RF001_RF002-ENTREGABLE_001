package pe.gob.servir.entidad.common;

public enum RoutingKey {

	LOG_TABLE("log_table"), 
	LOG_SERVICE("log_service"), 
	PRINT("print"),
	SEND_PROCESO_MASIVO("send_proceso_masivo");

	private String key;

	private RoutingKey(String key) {
		this.key = key;
	}

	public String getKey() {
		return key;
	}
}