package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import pe.gob.servir.entidad.model.Cargo;

public interface CargoRepository extends JpaRepository<Cargo, Long> {

}
