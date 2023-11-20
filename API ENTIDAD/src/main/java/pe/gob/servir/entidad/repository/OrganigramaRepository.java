package pe.gob.servir.entidad.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Organigrama;

public interface OrganigramaRepository extends JpaRepository<Organigrama, Long> {

	List<Organigrama> findByEntidadId(Long entidadId);
	
	Organigrama findByOrganigramaId(Long organigramaId);	
	
	
	
}
