package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Archivo;

public interface ArchivoRepository  extends JpaRepository<Archivo, Long> {

}
