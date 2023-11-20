package pe.gob.servir.entidad.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import pe.gob.servir.entidad.model.SolicitudPersona;

public interface SolicitudPersonaRepository extends JpaRepository<SolicitudPersona, Long> {
	
	@Query("select c from SolicitudPersona c where c.tipoPersona = :tipoPersonaJuridico and c.numeroDocumento = :ndocumento order by c.solicitudEntidadId desc")
	List<SolicitudPersona> findByNroRUC(@Param(value = "ndocumento") String ndocumento,@Param(value = "tipoPersonaJuridico") int tipoPersonaJuridico);
	
	@Query("select c from SolicitudPersona c where c.tipoPersona = :tipoPersonaNatural and c.solicitudEntidadId =:entidadId and c.numeroDocumento = :ndocumento ")
	List<SolicitudPersona> findByEntidadIdNroDNI(@Param(value = "entidadId") Long idSolicitudEntidad, @Param(value = "ndocumento") String nroDni,@Param(value = "tipoPersonaNatural") int tipoPersonaNatural);
    

}
