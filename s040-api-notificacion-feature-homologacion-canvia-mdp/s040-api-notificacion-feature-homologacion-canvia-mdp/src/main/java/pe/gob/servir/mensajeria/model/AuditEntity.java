package pe.gob.servir.mensajeria.model;

import java.time.Instant;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.mensajeria.common.EstadoRegistro;

@MappedSuperclass
@Getter
@Setter
public abstract class AuditEntity {

	@Column(nullable = false, name = "ESTADO_REGISTRO")
	protected String estadoRegistro;
	@Column(nullable = false, updatable = false, name = "USUARIO_CREACION")
	protected String usuarioCreacion;
	@Column(nullable = false, updatable = false, name = "FECHA_CREACION")
	protected Instant fechaCreacion;
	@Column(insertable = false, name = "USUARIO_MODIFICACION")
	protected String usuarioModificacion;
	@Column(insertable = false, name = "FECHA_MODIFICACION")
	protected Instant fechaModificacion;
	
	public void setCampoSegIns(String usuarioCreacion, Instant fechaCreacion){
		this.estadoRegistro = EstadoRegistro.ACTIVO.getCodigo();
		this.usuarioCreacion = usuarioCreacion;
		this.fechaCreacion = fechaCreacion;
	}
	public void setCampoSegUpd(String estadoRegistro, String usuarioModificacion, Instant fechaModificacion){
		this.estadoRegistro = estadoRegistro;
		this.usuarioModificacion = usuarioModificacion;
		this.fechaModificacion = fechaModificacion;
	}
}
