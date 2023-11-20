package pe.gob.servir.entidad.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Empleado;

public interface EmpleadoRepository extends JpaRepository<Empleado, Long>{

	Empleado findByEntidadIdAndPersonaId(Long entidadId, Long personaId);
	
	List<Empleado> findAllByEntidadIdAndPersonaId(Long entidadId, Long personaId);

	Empleado findByEntidadIdAndPersonaIdAndPuestoId(Long entidadId, Long personaId, Long puestoId);
	
	Empleado findByEntidadIdAndPersonaIdAndPuestoIdAndEstadoRegistro(Long entidadId, Long personaId, Long puestoId, String EstadoRegistro);
	
	Empleado findByEntidadIdAndPersonaIdAndEstadoRegistro(Long entidadId, Long personaId, String EstadoRegistro);
	
}
