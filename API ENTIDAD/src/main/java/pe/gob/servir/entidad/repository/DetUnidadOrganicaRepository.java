package pe.gob.servir.entidad.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.DetUnidadOrganica;

public interface DetUnidadOrganicaRepository extends JpaRepository<DetUnidadOrganica, Long> {
	
	List<DetUnidadOrganica> findByEntidadIdAndOrganigramaIdAndPersonaEvaluadorId(Long entidadId, Long uoId, Long personaEvaluadorId);
	
	DetUnidadOrganica findByDetUnidadOrganicaIdAndEstadoRegistro(Long detUoId, String estado);
	
	DetUnidadOrganica findByDetUnidadOrganicaId(Long detUoId);
	
	DetUnidadOrganica findByEntidadIdAndPuestoIdAndEstadoRegistro(Long entidadId, Long puestoId, String estado);
	
	List<DetUnidadOrganica> findByOrganigramaIdAndEstadoRegistro(Long organigramaId, String estado);
}
