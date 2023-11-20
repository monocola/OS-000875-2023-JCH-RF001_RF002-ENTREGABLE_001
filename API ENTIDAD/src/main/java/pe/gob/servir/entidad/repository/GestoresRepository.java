package pe.gob.servir.entidad.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.model.Gestores;

@Repository
public interface GestoresRepository extends JpaRepository<Gestores, Long> {

}
