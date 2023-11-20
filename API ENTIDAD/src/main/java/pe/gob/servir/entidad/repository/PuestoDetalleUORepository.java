package pe.gob.servir.entidad.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.model.DetUnidadOrganica;

@Repository
public interface PuestoDetalleUORepository extends JpaRepository<DetUnidadOrganica, Long> {
	
	@Query(value = "select * from sch_entidad.tbl_detalle_uo "+
			"where entidad_id = :entidadId and persona_id = :personaId"+
			" order by det_uo_id ", nativeQuery = true)
			List<DetUnidadOrganica> listaPuestoUOEvaluador(@Param(value = "personaId") Long entidadId,@Param(value = "entidadId") Long personaId);

	@Query(value = "select * from sch_entidad.tbl_detalle_uo "+
			"where entidad_id = :entidadId and persona_id = :personaId "+
			" order by det_uo_id DESC FETCH NEXT 1 ROWS ONLY", nativeQuery = true)
			List<DetUnidadOrganica> listaPuestoUOEvaluadoGdr(@Param(value = "personaId") Long entidadId,@Param(value = "entidadId") Long personaId);
}
