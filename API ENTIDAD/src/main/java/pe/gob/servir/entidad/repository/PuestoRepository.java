package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Puesto;

public interface PuestoRepository extends JpaRepository<Puesto, Long> {
	
	Optional<Puesto> findByDescripcion(String descripcion);
	
	
	Optional<Puesto> findByEntidadIdAndDescripcionAndOrganigramaIdAndEstadoRegistro(Long entidadId, String puesto, Long organigramaId, String estadoRegistro);
	List<Puesto> findByEntidadId(Long entidadId);
}
