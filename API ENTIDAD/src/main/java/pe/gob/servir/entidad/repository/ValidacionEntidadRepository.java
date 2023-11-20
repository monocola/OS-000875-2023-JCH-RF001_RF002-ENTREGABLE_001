package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.ValidacionEntidad;

public interface ValidacionEntidadRepository extends JpaRepository<ValidacionEntidad, Long> {

}
