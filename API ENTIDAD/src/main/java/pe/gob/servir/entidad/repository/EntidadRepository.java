package pe.gob.servir.entidad.repository; 

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pe.gob.servir.entidad.model.Entidad;

public interface EntidadRepository extends JpaRepository<Entidad, Long> {

	@Query("select e from Entidad e where e.personaId  = :id")
	Entidad findByEntidadPorIdPersona(@Param(value = "id") Long id);
	List<Entidad> findBySigla(String sigla);
} 