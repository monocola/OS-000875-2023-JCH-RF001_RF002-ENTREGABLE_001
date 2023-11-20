package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import pe.gob.servir.entidad.model.SolicitudExterna;

public interface SolicitudExternaRepository extends JpaRepository<SolicitudExterna, Long>{

	@Query("select c from SolicitudExterna c where c.rucEntidad = :rucEntidad and c.estadoSolicitud = :estadoSolicitud and c.estadoRegistro = '1' order by c.solicitudEntidadExtId desc")
	Optional<SolicitudExterna> findByNroRUCEstado(@Param(value = "rucEntidad") String rucEntidad,@Param(value = "estadoSolicitud") Long estadoSolicitud);
	
	@Query("select c from SolicitudExterna c where c.rucEntidad = :rucEntidad and c.estadoSolicitud = :estadoSolicitud and c.numeroDocumento = :numeroDocumento order by c.solicitudEntidadExtId desc")
	Optional<SolicitudExterna> findByRucDniEstado(@Param(value = "rucEntidad") String rucEntidad,@Param(value = "estadoSolicitud") Long estadoSolicitud, @Param(value = "numeroDocumento") String numeroDocumento);
	
	@Query("select c from SolicitudExterna c where c.rucEntidad = :rucEntidad and c.estadoSolicitud = :estadoSolicitud order by c.solicitudEntidadExtId desc")
	List<SolicitudExterna> findByListaRuc(@Param(value = "rucEntidad") String rucEntidad,@Param(value = "estadoSolicitud") Integer estadoSolicitud);

	@Query("select c from SolicitudExterna c where c.solicitudEntidadExtId = :id")
	SolicitudExterna findBySolExtId(@Param(value = "id") Long id);
}
