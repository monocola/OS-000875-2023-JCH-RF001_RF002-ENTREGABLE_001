package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import pe.gob.servir.entidad.model.Correo;

public interface CorreoRepository extends JpaRepository<Correo, Long> {
	
	@Query(value = "SELECT * FROM sch_persona.tbl_correo where correo = :correo and estado_registro = '1'", nativeQuery = true)
	List<Correo> findByCorreo(@Param("correo") String correo);
	
	@Query(value = "SELECT * FROM sch_persona.tbl_correo where persona_id = :personaId and tipo_correo = :tipoCorreoPrinc  and estado_registro = '1' and rownum = 1", nativeQuery = true)
	Optional<Correo> findBypersonaIdAndTipoCorreo(@Param("personaId") Integer personaId,@Param("tipoCorreoPrinc") String tipoCorreoPrinc);

	@Query(value = "SELECT * FROM sch_persona.tbl_correo where persona_id = :personaId and tipo_correo = :tipoCorreoAlter  and estado_registro = '1'", nativeQuery = true)
	List<Correo> findBypersonaIdAndTipoCorreoAlternativo(@Param("personaId")Integer personaId,@Param("tipoCorreoAlter") String tipoCorreoAlter);
	
}
