package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Persona;

public interface PersonaRepository extends JpaRepository<Persona, Long> {
	

}
