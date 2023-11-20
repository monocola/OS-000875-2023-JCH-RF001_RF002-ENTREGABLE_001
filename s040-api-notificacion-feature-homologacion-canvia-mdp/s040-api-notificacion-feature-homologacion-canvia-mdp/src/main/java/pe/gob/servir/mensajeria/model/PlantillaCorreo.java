package pe.gob.servir.mensajeria.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;

/**
 * Created by Developer02 on 21/04/2020.
 */

@Entity
@Table(name = "TBL_PLANTILLA_CORREO", schema = "SCH_NOTIFICACION")
@SuppressWarnings("serial")
public class PlantillaCorreo extends AuditEntity implements Serializable {

	@Id
	@Column(name = "plantilla_correo_id")
	private int plantillaId;

	@Column(name = "codigo")
	private String codigoPlantilla;

	@Column(name = "nombre")
	private String nombrePlantilla;

	@Column(name = "html")
	private String html;

	@Column(name = "asunto_correo")
	private String asuntoPlantilla;

	@Column(name = "cc_correo")
	private String cc;

	@Column(name = "cco_correo")
	private String cco;

	public int getPlantillaId() {
		return plantillaId;
	}

	public void setPlantillaId(int plantillaId) {
		this.plantillaId = plantillaId;
	}

	public String getCodigoPlantilla() {
		return codigoPlantilla;
	}

	public void setCodigoPlantilla(String codigoPlantilla) {
		this.codigoPlantilla = codigoPlantilla;
	}

	public String getNombrePlantilla() {
		return nombrePlantilla;
	}

	public void setNombrePlantilla(String nombrePlantilla) {
		this.nombrePlantilla = nombrePlantilla;
	}

	public String getAsuntoPlantilla() {
		return asuntoPlantilla;
	}

	public void setAsuntoPlantilla(String asuntoPlantilla) {
		this.asuntoPlantilla = asuntoPlantilla;
	}

	public String getCc() {
		return cc;
	}

	public void setCc(String cc) {
		this.cc = cc;
	}

	public String getCco() {
		return cco;
	}

	public void setCco(String cco) {
		this.cco = cco;
	}

	public String getHtml() {
		return html;
	}

	public void setHtml(String html) {
		this.html = html;
	}

    @Override
    public String toString() {
        return "PlantillaCorreo{" +
                "plantillaId=" + plantillaId +
                ", codigoPlantilla='" + codigoPlantilla + '\'' +
                ", nombrePlantilla='" + nombrePlantilla + '\'' +
                ", asuntoPlantilla='" + asuntoPlantilla + '\'' +
                ", cc='" + cc + '\'' +
                ", cco='" + cco + '\'' +
                ", html='" + html + '\'' +
                '}';
    }
}
