package pe.gob.servir.entidad.model;

import java.time.Instant;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.InstantSerializer;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.common.Constantes;

@Entity
@Table(name = "TBL_PROCESO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Proceso extends AuditEntity {
	
	@Id
	@Column(name = "PROCESO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PROCESO_ID")
	@SequenceGenerator(sequenceName = "SEQ_PROCESO_ID", allocationSize = 1, schema = "SCH_ENTIDAD", name = "SEQ_PROCESO_ID")
	private Long procesoId;

	@Column(name = "TIPO_PROCESO_ID")
	private Long tipoProcesoId;

	@Column(nullable = false, updatable = false, name = "USUARIO_ENVIO")
	private String usuarioEnvio;

	@Column(nullable = false, updatable = false, name = "FECHA_ENVIO")
	@JsonSerialize(using = InstantSerializer.class)
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_AUDITORIA, timezone = Constantes.FORMATO_TIMEZONE)
	private Instant fechaEnvio;
	
	@Column(insertable = false, name = "FECHA_FIN")
	@JsonSerialize(using = InstantSerializer.class)
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_AUDITORIA, timezone = Constantes.FORMATO_TIMEZONE)
	private Instant fechaFin;

	@Column(name = "URL_ARCHIVO_INPUT")
	private String urlArchivoInput;

	@Column(name = "URL_ARCHIVO_OUTPUT")
	private String urlArchivoOutput;

	@Column(name = "JSON_VARIABLES_INPUT")
	private String jsonVariablesInput;

	@Column(name = "JSON_VARIABLES_OUTPUT")
	private String jsonVariablesOutput;

	@Column(name = "ESTADO_PROCESO_ID")
	private Long estadoProcesoId;

	@Column(name = "MENSAJE_ERROR")
	private String mensajeError;

}