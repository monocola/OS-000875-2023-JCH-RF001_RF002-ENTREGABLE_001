package pe.gob.servir.mensajeria.common;

public enum RoutingKey {

	LOG_TABLE("log_table"), 
	LOG_SERVICE("log_service"), 
	PRINT("print"),
	SEND_EMAIL("send_email"),
	SEND_EMAIL_MASIVO("send_email_masivo"),
	SEND_EMAIL_CREDENCIALES("send_email_credenciales");

	private String key;

	private RoutingKey(String key) {
		this.key = key;
	}

	public String getKey() {
		return key;
	}
}
