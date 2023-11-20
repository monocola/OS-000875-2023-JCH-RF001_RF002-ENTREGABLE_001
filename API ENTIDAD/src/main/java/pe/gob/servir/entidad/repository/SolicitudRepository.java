package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Solicitud;

public interface SolicitudRepository extends JpaRepository<Solicitud, Long> {
	
}
