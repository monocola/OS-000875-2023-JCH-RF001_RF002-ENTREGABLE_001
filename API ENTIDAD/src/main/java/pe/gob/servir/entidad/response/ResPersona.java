package pe.gob.servir.entidad.response;

import lombok.Data;

import java.util.List;

@Data
public class ResPersona {
    private Persona persona;
    private PersonaNatural personaNatural;
    private PersonaJuridica personaJuridica;
    private List<Documento> documentos;
    private List<Direccion> direcciones;
    private List<Telefono> telefonos;
    private List<Correo> correos;
    private List<Web> webs;
}
