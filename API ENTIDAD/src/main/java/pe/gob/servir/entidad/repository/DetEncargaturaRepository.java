package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.DetEncargaturaDTO;

public interface DetEncargaturaRepository extends JpaRepository<DetEncargaturaDTO, Long> {
	

}
